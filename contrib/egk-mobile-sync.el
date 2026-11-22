;;; egk-mobile-sync.el --- Sync ekg notes with the mobile service  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qingshui Zheng

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The Mac is the only writer of the canonical ekg database.  This module
;; mirrors notes to the ekg-sync service on FNOS, and applies notes edited on
;; the iPhone:
;;
;;   ekg save/delete hooks  -> mark dirty -> POST /v1/sync (batches)
;;   a timer               -> GET  /v1/sync?since=cursor -> apply locally
;;
;; The sync unit is a note: id, title, body, mode, tags, timestamps.  Triples,
;; pinyin and inline commands stay an implementation detail of ekg -- remote
;; changes are applied through `ekg-note-create'/`ekg-save-note', never SQL, so
;; the FTS index, pinyin shadow table and vulpea mirror stay in step.
;;
;; Conflicts are never overwritten silently: when the server has a version the
;; Mac did not see, the local text is kept in a new note tagged `conflict' and
;; the server's version is applied under the original id.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'plz nil t)

(declare-function ekg-connect "ekg")
(declare-function ekg-get-note-with-id "ekg" (id))
(declare-function ekg-note-create "ekg" (&rest args))
(declare-function ekg-note-trash "ekg" (note))
(declare-function ekg-note-id "ekg" (note))
(declare-function ekg-note-text "ekg" (note))
(declare-function ekg-note-mode "ekg" (note))
(declare-function ekg-note-tags "ekg" (note))
(declare-function ekg-note-properties "ekg" (note))
(declare-function ekg-note-creation-time "ekg" (note))
(declare-function ekg-note-modified-time "ekg" (note))
(defvar ekg-db)
(declare-function ekg-save-note "ekg" (note))
(declare-function plz "plz" (method url &rest args))
(declare-function triples-subjects-of-type "triples" (db type))

(defgroup egk-mobile-sync nil
  "Mirror ekg notes to the ekg-sync service."
  :group 'ekg
  :prefix "egk-mobile-sync-")

(defcustom egk-mobile-sync-endpoint "http://10.100.90.12:8787"
  "Base URL of the ekg-sync service (no trailing slash)."
  :type 'string)

(defcustom egk-mobile-sync-token nil
  "Bearer token, or nil to read `egk-mobile-sync-token-file'."
  :type '(choice (const nil) string))

(defcustom egk-mobile-sync-token-file "~/.config/ekg-mobile-sync/token"
  "File holding the bearer token when `egk-mobile-sync-token' is nil."
  :type 'file)

(defcustom egk-mobile-sync-interval 60
  "Seconds between pull attempts."
  :type 'integer)

(defcustom egk-mobile-sync-conflict-tag "conflict"
  "Tag put on a note that keeps one side of a conflict."
  :type 'string)

(defcustom egk-mobile-sync-state-file
  (expand-file-name "egk-mobile-sync/state.json"
                    (or (getenv "XDG_CACHE_HOME") "~/Library/Caches"))
  "Where the cursor and per-note revisions are stored."
  :type 'file)

(defconst egk-mobile-sync--batch 100
  "Mutations per push request.")

(defvar egk-mobile-sync--state nil
  "Plist (:cursor N :revs hash :dirty hash).
`revs' maps note id to the last server rev the Mac has seen; `dirty' maps id
to either t (edited) or `deleted'.")

(defvar egk-mobile-sync--busy nil)
(defvar egk-mobile-sync--pulled nil
  "Non-nil when the last pull applied at least one remote change.")

(defvar egk-mobile-sync--applying nil
  "Bound to t while a remote change is written, so hooks stay quiet.")
(defvar egk-mobile-sync--timer nil)
(defvar egk-mobile-sync--debounce nil)
(defvar egk-mobile-sync--log-buffer "*egk-mobile-sync*")

;;; State

(defun egk-mobile-sync--fresh-state ()
  (list :cursor 0
        :revs (make-hash-table :test #'equal)
        :dirty (make-hash-table :test #'equal)))

(defun egk-mobile-sync--state-get ()
  (unless egk-mobile-sync--state
    (setq egk-mobile-sync--state (egk-mobile-sync--fresh-state))
    (when (file-exists-p egk-mobile-sync-state-file)
      (condition-case err
          (let* ((json-object-type 'alist)
                 (json-array-type 'list)
                 (data (json-read-file egk-mobile-sync-state-file)))
            (setf (plist-get egk-mobile-sync--state :cursor)
                  (or (alist-get 'cursor data) 0))
            (dolist (cell (alist-get 'revs data))
              (puthash (egk-mobile-sync--key (car cell)) (cdr cell)
                       (plist-get egk-mobile-sync--state :revs)))
            (dolist (cell (alist-get 'dirty data))
              (puthash (egk-mobile-sync--key (car cell))
                       (if (equal (cdr cell) "deleted") 'deleted t)
                       (plist-get egk-mobile-sync--state :dirty))))
        (error
         (egk-mobile-sync--log "cannot read state file %s: %s"
                               egk-mobile-sync-state-file
                               (error-message-string err))))))
  egk-mobile-sync--state)

(defun egk-mobile-sync--state-save ()
  (let* ((state (egk-mobile-sync--state-get))
         (revs nil) (dirty nil))
    (maphash (lambda (k v) (push (cons k v) revs)) (plist-get state :revs))
    (maphash (lambda (k v) (push (cons k (if (eq v 'deleted) "deleted" "1")) dirty))
             (plist-get state :dirty))
    (make-directory (file-name-directory egk-mobile-sync-state-file) t)
    (with-temp-file egk-mobile-sync-state-file
      (insert (json-encode `((cursor . ,(plist-get state :cursor))
                             (revs . ,revs)
                             (dirty . ,dirty)))))))

(defun egk-mobile-sync--key (key)
  "Return KEY as the string note id used in the hash tables.
`json-read' turns object keys into symbols and ekg subjects are sometimes
numbers, so everything is normalised before it is stored or sent."
  (cond ((symbolp key) (symbol-name key))
        ((stringp key) key)
        (t (format "%s" key))))

(defun egk-mobile-sync--find-note (id)
  "Find note ID, which may be a string or (for migrated notes) a number."
  (or (ekg-get-note-with-id id)
      (when (and (stringp id) (string-match-p "\\`[0-9]+\\'" id))
        (ekg-get-note-with-id (string-to-number id)))))

(defun egk-mobile-sync--cursor ()
  (plist-get (egk-mobile-sync--state-get) :cursor))

(defun egk-mobile-sync--revs () (plist-get (egk-mobile-sync--state-get) :revs))
(defun egk-mobile-sync--dirty-table () (plist-get (egk-mobile-sync--state-get) :dirty))

;;; Logging

(defun egk-mobile-sync--log (fmt &rest args)
  (with-current-buffer (get-buffer-create egk-mobile-sync--log-buffer)
    (goto-char (point-max))
    (insert (format-time-string "[%Y-%m-%d %H:%M:%S] ")
            (apply #'format fmt args) "\n")))

;;; Token and HTTP

(defun egk-mobile-sync--token ()
  (or egk-mobile-sync-token
      (when (file-readable-p egk-mobile-sync-token-file)
        (string-trim
         (with-temp-buffer
           (insert-file-contents egk-mobile-sync-token-file)
           (buffer-string))))))

(defun egk-mobile-sync--url (path)
  (concat (string-remove-suffix "/" egk-mobile-sync-endpoint) path))

(defun egk-mobile-sync--request (method path &rest args)
  (let ((token (egk-mobile-sync--token)))
    (unless token
      (error "egk-mobile-sync: no token; set `egk-mobile-sync-token' or %s"
             egk-mobile-sync-token-file))
    (apply #'plz method (egk-mobile-sync--url path)
           :headers `(("Authorization" . ,(concat "Bearer " token))
                      ("Content-Type" . "application/json"))
           :timeout 25
           :as 'string
           args)))

;;; Notes <-> mutations

(defun egk-mobile-sync--uuid ()
  (secure-hash 'sha1 (format "%s-%s-%s" (float-time) (random) (emacs-pid))))

(defun egk-mobile-sync--note-titles (note)
  "Return all of NOTE's titles; ekg allows a list in `:titled/title'."
  (let ((value (plist-get (ekg-note-properties note) :titled/title)))
    (cond ((stringp value) (list value))
          ((listp value) (seq-filter #'stringp value))
          (t nil))))

(defun egk-mobile-sync--note-title (note)
  "Return NOTE's primary title, tolerating a string or a list."
  (or (car (egk-mobile-sync--note-titles note)) ""))

(defun egk-mobile-sync--strings (value)
  "Normalise VALUE to a vector of strings for JSON arrays."
  (cond ((null value) [])
        ((stringp value) (vector value))
        ((listp value) (vconcat (seq-filter #'stringp value)))
        (t (vector (format "%s" value)))))

(defun egk-mobile-sync--set-org-time (props key value)
  "Set KEY to VALUE in PROPS, or remove it when VALUE is 0/nil."
  (if (and (integerp value) (> value 0))
      (plist-put props key value)
    (let (out (rest props))
      (while rest
        (let ((k (pop rest)) (v (pop rest)))
          (unless (eq k key) (push k out) (push v out))))
      (nreverse out))))

(defun egk-mobile-sync--note-mode-name (note)
  (let ((mode (ekg-note-mode note)))
    (cond ((stringp mode) mode)
          ((symbolp mode) (symbol-name mode))
          (t "org-mode"))))

(defun egk-mobile-sync--now ()
  (time-convert nil 'integer))

(defun egk-mobile-sync--note->mutation (id)
  "Return the mutation alist for dirty note ID, or nil when it is gone."
  (let* ((id (egk-mobile-sync--key id))
         (dirty (gethash id (egk-mobile-sync--dirty-table)))
         (base (or (gethash id (egk-mobile-sync--revs)) ""))
         (mutation-id (egk-mobile-sync--uuid)))
    (if (eq dirty 'deleted)
        `(("mutation_id" . ,mutation-id)
          ("id" . ,id)
          ("base_rev" . ,base)
          ("deleted" . t)
          ("modified" . ,(egk-mobile-sync--now)))
      (let ((note (egk-mobile-sync--find-note id)))
        (when note
          `(("mutation_id" . ,mutation-id)
            ("id" . ,id)
            ("base_rev" . ,base)
            ("title" . ,(egk-mobile-sync--note-title note))
            ("titles" . ,(egk-mobile-sync--strings
                          (egk-mobile-sync--note-titles note)))
            ("body" . ,(or (ekg-note-text note) ""))
            ("mode" . ,(egk-mobile-sync--note-mode-name note))
            ("tags" . ,(egk-mobile-sync--strings (ekg-note-tags note)))
            ("refs" . ,(egk-mobile-sync--strings
                        (plist-get (ekg-note-properties note) :reffed/ref)))
            ("created" . ,(truncate (or (ekg-note-creation-time note) 0)))
            ("modified" . ,(truncate (or (ekg-note-modified-time note)
                                         (egk-mobile-sync--now))))
            ("scheduled" . ,(or (plist-get (ekg-note-properties note)
                                            :org/scheduled)
                                0))
            ("deadline" . ,(or (plist-get (ekg-note-properties note)
                                           :org/deadline)
                               0))
            ("deleted" . :json-false)))))))

;;; Applying remote notes

(defun egk-mobile-sync--apply-note (note)
  "Write one note alist from the server into ekg."
  (let* ((id (alist-get 'id note))
         (deleted (eq (alist-get 'deleted note) t))
         (title (or (alist-get 'title note) ""))
         (titles (let ((value (alist-get 'titles note)))
                   (cond ((vectorp value) (append value nil))
                         ((listp value) value)
                         (t nil))))
         (titles (if titles titles (if (string-empty-p title) nil (list title))))
         (body (or (alist-get 'body note) ""))
         (mode (intern (or (alist-get 'mode note) "org-mode")))
         (tags (append (alist-get 'tags note) nil))
         (refs (append (alist-get 'refs note) nil))
         (existing (egk-mobile-sync--find-note id))
         (egk-mobile-sync--applying t))
    (cond
     (deleted
      (when (and existing
                 (not (member "trash" (ekg-note-tags existing))))
        (ekg-note-trash existing)))
     ((null existing)
      ;; `ekg-note-create' always stamps creation time with `current-time'.
      ;; Fake it to the server's created time; the struct's setf accessor is
      ;; not available in this build.  Save outside the let so modified time
      ;; is still "now".
      (let* ((created-at (or (alist-get 'created note) (egk-mobile-sync--now)))
             (created
              (cl-letf (((symbol-function 'current-time)
                         (lambda () (seconds-to-time created-at))))
                (ekg-note-create
                 :id id
                 :text body
                 :mode mode
                 :tags tags
                 :properties
                 (let ((props (list :titled/title titles)))
                   (when refs (setq props (plist-put props :reffed/ref refs)))
                   (setq props (egk-mobile-sync--set-org-time
                                props :org/scheduled
                                (alist-get 'scheduled note)))
                   (setq props (egk-mobile-sync--set-org-time
                                props :org/deadline
                                (alist-get 'deadline note)))
                   props)))))
        (ekg-save-note created)))
     (t
      ;; The struct's (setf ekg-note-*) accessors are not available in this
      ;; ekg build, so build a fresh note object with the server fields and
      ;; let `ekg-save-note' replace the stored row.
      (let ((props (ekg-note-properties existing)))
        (setq props (plist-put props :titled/title titles))
        (setq props (plist-put props :reffed/ref refs))
        (setq props (egk-mobile-sync--set-org-time
                     props :org/scheduled (alist-get 'scheduled note)))
        (setq props (egk-mobile-sync--set-org-time
                     props :org/deadline (alist-get 'deadline note)))
        (let ((updated (make-ekg-note
                        :id (ekg-note-id existing)
                        :text body
                        :mode mode
                        :tags tags
                        :creation-time (ekg-note-creation-time existing)
                        :modified-time (egk-mobile-sync--now)
                        :properties props
                        :inlines (ekg-note-inlines existing))))
          (ekg-save-note updated)))))
    (puthash id (or (alist-get 'rev note) "") (egk-mobile-sync--revs))
    id))

(defun egk-mobile-sync--conflict-copy (note)
  "Save the local version of NOTE as a new `conflict'-tagged note."
  (let* ((title (egk-mobile-sync--note-title note))
         (titles (or (egk-mobile-sync--note-titles note) (list title)))
         (props (copy-sequence (ekg-note-properties note))))
    (setf (plist-get props :titled/title)
          (cons (format "[冲突] %s" (if (string-empty-p title) "无标题" title))
                (cdr titles)))
    (let ((copy (ekg-note-create
                 :id (egk-mobile-sync--uuid)
                 :text (or (ekg-note-text note) "")
                 :mode (ekg-note-mode note)
                 :tags (cons egk-mobile-sync-conflict-tag (ekg-note-tags note))
                 :properties props)))
      (ekg-save-note copy)
      (egk-mobile-sync--log "conflict on %s: local copy kept as %s"
                            (ekg-note-id note) (ekg-note-id copy)))))

(defun egk-mobile-sync--resolve-conflict (conflict)
  (let* ((id (alist-get 'id conflict))
         (server (alist-get 'server conflict))
         (local (egk-mobile-sync--find-note id)))
    (when local
      (egk-mobile-sync--conflict-copy local))
    (if server
        (egk-mobile-sync--apply-note server)
      (remhash id (egk-mobile-sync--revs)))
    (remhash id (egk-mobile-sync--dirty-table))))

;;; Push

(defun egk-mobile-sync--dirty-ids ()
  (let (ids)
    (maphash (lambda (k _) (push k ids)) (egk-mobile-sync--dirty-table))
    ids))

(defun egk-mobile-sync--push (done)
  "Push up to `egk-mobile-sync--batch' dirty notes, then continue, then DONE."
  (let ((ids (seq-take (egk-mobile-sync--dirty-ids) egk-mobile-sync--batch)))
    (if (null ids)
        (funcall done)
      (let (mutations gone)
        (dolist (id ids)
          (let ((m (egk-mobile-sync--note->mutation id)))
            (if m (push m mutations) (push id gone))))
        (dolist (id gone)
          (remhash id (egk-mobile-sync--dirty-table)))
        (if (null mutations)
            (egk-mobile-sync--push done)
          (egk-mobile-sync--log "push %d notes" (length mutations))
          (egk-mobile-sync--request
           'post "/v1/sync"
           :body (json-encode `(("device" . "mac")
                                ("mutations" . ,(vconcat mutations))))
           :body-type 'text
           :then (lambda (body)
                   (egk-mobile-sync--handle-push body done))
           :else (lambda (err)
                   (egk-mobile-sync--fail "push" err))))))))

(defun egk-mobile-sync--handle-push (body done)
  (condition-case err
      (let* ((json-object-type 'alist)
             (json-array-type 'list)
             (resp (json-read-from-string body))
             (applied (alist-get 'applied resp))
             (conflicts (alist-get 'conflicts resp)))
        (dolist (a applied)
          (let ((id (alist-get 'id a)))
            (puthash id (or (alist-get 'rev a) "") (egk-mobile-sync--revs))
            (remhash id (egk-mobile-sync--dirty-table))))
        (dolist (c conflicts)
          (egk-mobile-sync--resolve-conflict c))
        (egk-mobile-sync--state-save)
        (egk-mobile-sync--push done))
    (error
     (egk-mobile-sync--fail "push response" err))))

;;; Pull

(defun egk-mobile-sync--pull ()
  (egk-mobile-sync--log "pull since %d" (egk-mobile-sync--cursor))
  (egk-mobile-sync--request
   'get (format "/v1/sync?since=%d&limit=500" (egk-mobile-sync--cursor))
   :then #'egk-mobile-sync--handle-pull
   :else (lambda (err) (egk-mobile-sync--fail "pull" err))))

(defun egk-mobile-sync--handle-pull (body)
  (condition-case err
      (let* ((json-object-type 'alist)
             (json-array-type 'list)
             (resp (json-read-from-string body))
             (notes (alist-get 'notes resp))
             (cursor (alist-get 'cursor resp))
             (more (eq (alist-get 'more resp) t)))
        (dolist (note notes)
          (let* ((id (egk-mobile-sync--key (alist-get 'id note)))
                 (known (gethash id (egk-mobile-sync--revs)))
                 (rev (alist-get 'rev note)))
            ;; A locally dirty note is pushed first; the server answers with
            ;; a conflict and both sides survive.  An unchanged note is not
            ;; rewritten: saving thousands of identical notes would run
            ;; ekg's backup and hooks for nothing.
            (unless (or (gethash id (egk-mobile-sync--dirty-table))
                        (equal known rev))
              (condition-case apply-err
                  (progn
                    (egk-mobile-sync--apply-note note)
                    (setq egk-mobile-sync--pulled t))
                (error
                 (egk-mobile-sync--log "apply %s failed: %s" id
                                       (error-message-string apply-err)))))))
        (let ((state (egk-mobile-sync--state-get)))
          (setf (plist-get state :cursor) cursor))
        (egk-mobile-sync--state-save)
        (if more
            (egk-mobile-sync--pull)
          (egk-mobile-sync--finish)))
    (error
     (egk-mobile-sync--fail "pull response" err))))

(defun egk-mobile-sync--fail (what err)
  (egk-mobile-sync--log "%s failed: %s" what
                        (if (and (fboundp 'plz-error-p) (plz-error-p err))
                            (format "%s (status %s)"
                                    (or (plz-error-message err) "plz error")
                                    (or (ignore-errors
                                          (plz-response-status
                                           (plz-error-response err)))
                                        "?"))
                          (format "%S" err)))
  (egk-mobile-sync--finish))

(defun egk-mobile-sync--refresh-views ()
  "Refresh plain ekg-notes buffers after a pull.
`ekg-org-view-mode' installs its own save/delete hook; the generic
notes list does not, so remote changes would stay invisible."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (and (derived-mode-p 'ekg-notes-mode)
                   (not (derived-mode-p 'ekg-org-view-mode)))
          (ignore-errors (ekg-notes-refresh)))))))

(defun egk-mobile-sync--finish ()
  (setq egk-mobile-sync--busy nil)
  (egk-mobile-sync--state-save)
  (when egk-mobile-sync--pulled
    (setq egk-mobile-sync--pulled nil)
    (egk-mobile-sync--refresh-views)))

;;; Entry points

;;;###autoload
(defun egk-mobile-sync-sync ()
  "Push local changes, then pull remote ones."
  (interactive)
  (unless egk-mobile-sync--busy
    (setq egk-mobile-sync--busy t
          egk-mobile-sync--pulled nil)
    (condition-case err
        (progn
          (ekg-connect)
          (egk-mobile-sync--push #'egk-mobile-sync--pull))
      (error
       (egk-mobile-sync--fail "sync" err)))))

;;;###autoload
(defun egk-mobile-sync-seed ()
  "Mark every note dirty, then push them all (first-server-seed only)."
  (interactive)
  (ekg-connect)
  (let ((dirty (egk-mobile-sync--dirty-table))
        (ids (triples-subjects-of-type ekg-db 'text)))
    (dolist (id ids)
      (puthash (egk-mobile-sync--key id) t dirty))
    (egk-mobile-sync--log "seeding %d notes" (length ids))
    (egk-mobile-sync-sync)))

(defun egk-mobile-sync--on-save (note)
  (unless egk-mobile-sync--applying
    (puthash (egk-mobile-sync--key (ekg-note-id note)) t (egk-mobile-sync--dirty-table))
    (egk-mobile-sync--schedule)))

(defun egk-mobile-sync--on-delete (id)
  (unless egk-mobile-sync--applying
    (puthash (egk-mobile-sync--key id) 'deleted (egk-mobile-sync--dirty-table))
    (egk-mobile-sync--schedule)))

(defun egk-mobile-sync--schedule ()
  (unless (timerp egk-mobile-sync--debounce)
    (setq egk-mobile-sync--debounce
          (run-with-timer 3 nil
                          (lambda ()
                            (setq egk-mobile-sync--debounce nil)
                            (egk-mobile-sync-sync))))))

(defun egk-mobile-sync--tick ()
  (when (and (not egk-mobile-sync--busy)
             (egk-mobile-sync--token))
    (egk-mobile-sync-sync)))

;;;###autoload
(defun egk-mobile-sync-enable ()
  "Start mirroring ekg notes to the ekg-sync service."
  (interactive)
  (egk-mobile-sync--state-get)
  (add-hook 'ekg-note-save-hook #'egk-mobile-sync--on-save)
  (add-hook 'ekg-note-delete-hook #'egk-mobile-sync--on-delete)
  (unless (timerp egk-mobile-sync--timer)
    (setq egk-mobile-sync--timer
          (run-with-timer 5 egk-mobile-sync-interval #'egk-mobile-sync--tick)))
  (egk-mobile-sync--log "enabled (endpoint %s)" egk-mobile-sync-endpoint))

;;;###autoload
(defun egk-mobile-sync-disable ()
  "Stop mirroring."
  (interactive)
  (remove-hook 'ekg-note-save-hook #'egk-mobile-sync--on-save)
  (remove-hook 'ekg-note-delete-hook #'egk-mobile-sync--on-delete)
  (when (timerp egk-mobile-sync--timer)
    (cancel-timer egk-mobile-sync--timer)
    (setq egk-mobile-sync--timer nil))
  (egk-mobile-sync--log "disabled"))

(provide 'egk-mobile-sync)
;;; egk-mobile-sync.el ends here
