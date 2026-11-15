;;; ekg-vulpea.el --- Mirror vulpea notes into ekg -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qingshui Zheng

;; Author: Qingshui Zheng <qingshuizheng@outlook.com>

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; While vulpea and ekg coexist, ekg holds one *mirror note* per vulpea note:
;;
;;   id        the vulpea UUID, so [[id:...]] links need no rewriting
;;   text      the file body (the whole file for a level-0 note, the subtree
;;             for a heading note)
;;   title     the vulpea title
;;   tags      vulpea tags (split on whitespace) plus `vulpea' and
;;             `vulpea/mirror'
;;   :vulpea/* the source file, position, level, mtime and state
;;
;; A mirror only makes a note findable and jumpable: search and tag views go
;; through ekg's own FTS, and RET opens the *vulpea file* rather than an EKG
;; Edit buffer.  Editing therefore keeps writing only the file, with no
;; second source of truth.  The state lives in `:vulpea/state' instead of
;; being inferred from whether a file exists:
;;
;;   "mirror"    not migrated yet.  `ekg-vulpea-sync' keeps the text in
;;               step with the file, and `id:' links open the file.
;;   "migrated"  migrated into ekg.  ekg is now the only source of truth;
;;               sync never touches it, and `id:' links open the ekg note.
;;
;; Migration is a first-class operation rather than "import everything, then
;; delete the files": `ekg-vulpea-promote' turns the current mirror into a
;; migrated note, carrying the latest body over, after which RET and `id:'
;; links reach the ekg note.  The original file may stay as a read-only
;; archive, or `C-u' may move it under `.ekg-migrated/'.  The model is
;; consistent at every point: un-migrated notes follow the file, migrated
;; ones follow ekg.
;;
;; Journal files are the one exception (`ekg-vulpea-journal-paths'): a day
;; file's time headings are sections, not notes, so only the file-level note
;; is mirrored and every property drawer is stripped from it.  The rest of
;; the corpus keeps one mirror per note, heading notes included.
;;
;;   M-x ekg-vulpea-sync           incremental sync (C-u re-reads all text)
;;   M-x ekg-vulpea-show-mirrors   view of the notes not migrated yet
;;   M-x ekg-vulpea-promote        migrate the note at point (C-u also
;;                                   archives the file once every note in it
;;                                   is migrated)
;;   M-x ekg-vulpea-open-file      open the vulpea source of the current note
;;
;; Saving a vulpea file re-indexes that file three seconds later
;; (`ekg-vulpea-autosync').  Changes made elsewhere -- another Dropbox
;; machine, a batch script -- run no save hooks, so run a full sync by hand
;; after those.
;;
;; Known trade-offs:
;;   * A level-0 note's text is the whole file, so a heading note shows up a
;;     second time inside its parent file's mirror.  Searching multi-note
;;     files can therefore return duplicates.  In exchange, no body text
;;     without an ID is lost.
;;   * vulpea tags may contain spaces (`#+filetags: graph design slip-box');
;;     they are split on whitespace, which matches org's own semantics.
;;   * vulpea's `draft'/`trash' collide with ekg's control tags.  Mirrors
;;     prefix them (`vulpea/draft'/`vulpea/trash'); otherwise ekg would hide
;;     those notes as drafts or trash -- on this corpus, 151 of 2995 notes
;;     would become unsearchable.
;;   * The first import into a database without any mirrors forces a backup
;;     of the ekg database (`ekg-vulpea-backup-on-first-import').

;;; Code:

(require 'cl-lib)
(require 'ekg)
(require 'org-id)
(require 'seq)
(require 'subr-x)

(defvar vulpea-db-sync-directories)
(defvar org-directory)

(declare-function vulpea-db-query "vulpea-db-query")
(declare-function vulpea-db-query-by-file-path "vulpea-db-query")
(declare-function vulpea-note-id "vulpea-note")
(declare-function vulpea-note-level "vulpea-note")
(declare-function vulpea-note-path "vulpea-note")
(declare-function vulpea-note-pos "vulpea-note")
(declare-function vulpea-note-tags "vulpea-note")
(declare-function vulpea-note-title "vulpea-note")

;;; Options

(defgroup ekg-vulpea nil
  "Mirroring vulpea notes into ekg."
  :group 'ekg)

(defcustom ekg-vulpea-sync-directory nil
  "Directory whose vulpea notes are mirrored into ekg.
When nil, use the first entry of `vulpea-db-sync-directories', falling
back to `org-directory'."
  :type '(choice (const :tag "Derive from vulpea" nil) directory))

(defcustom ekg-vulpea-tag "vulpea"
  "Tag put on every note mirrored from vulpea.
It records where the note came from and survives promotion, so
`ekg-show-notes-with-tag' on it lists the whole old corpus."
  :type 'string)

(defcustom ekg-vulpea-mirror-tag "vulpea/mirror"
  "Tag put only on notes still backed by a vulpea file.
`ekg-vulpea-show-mirrors' lists them; promotion removes it."
  :type 'string)

(defcustom ekg-vulpea-archive-directory nil
  "Where `ekg-vulpea-promote' with a prefix argument moves source files.
When nil, use `.ekg-migrated' inside `ekg-vulpea-sync-directory'.
vulpea ignores hidden directories, so an archived file stops being
indexed there as well."
  :type '(choice (const :tag "Next to the notes" nil) directory))

(defcustom ekg-vulpea-autosync t
  "When non-nil, re-index a vulpea file shortly after it is saved.
Only that file is touched, and the wait gives vulpea's asynchronous
extractor time to update its own database first."
  :type 'boolean)

(defcustom ekg-vulpea-autosync-delay 3
  "Seconds to wait after saving a vulpea file before re-indexing it."
  :type 'number)

(defcustom ekg-vulpea-list-limit 100
  "How many un-migrated mirrors `ekg-vulpea-show-mirrors' lists.
`ekg-get-notes-with-tag' over the whole corpus takes ~10 seconds
because every note is loaded one by one, so the default view shows
the most recently modified files and a prefix argument shows all."
  :type 'integer)

(defcustom ekg-vulpea-journal-paths '("/journal/")
  "Regexps matching the vulpea files treated as journal entries.
A journal file is one note per day: its time headings are sections,
not separate notes, so only the file-level note is mirrored.  Every
property drawer is stripped from that note's text, which also keeps a
heading's UUID out of the day note's body; otherwise looking the UUID
up would match both notes.

Files outside these paths keep one mirror per note, heading notes
included.  Set this to nil to mirror journal headings and keep their
drawers."
  :type '(repeat regexp))

(defcustom ekg-vulpea-backup-on-first-import t
  "When non-nil, back up ekg before the first import of a database.
An empty index means the sync is about to write thousands of notes
into the main database, so a backup is cheap insurance.  Later syncs
add or update a handful of notes and do not force one."
  :type 'boolean)

(defconst ekg-vulpea--state-mirror "mirror"
  "`ekg-vulpea-state' value for a note still backed by a vulpea file.")

(defconst ekg-vulpea--state-migrated "migrated"
  "`ekg-vulpea-state' value for a note ekg now owns.")

;;; Schema

(defvar ekg-vulpea--schema-db nil
  "The `ekg-db' handle the vulpea schema was installed on.")

(defun ekg-vulpea--add-schema ()
  "Install the `vulpea' triples type on the connected ekg database."
  (when ekg-db
    (triples-add-schema ekg-db 'vulpea
                        '(state :base/unique t :base/type string)
                        '(file :base/unique t :base/type string)
                        '(pos :base/unique t :base/type integer)
                        '(level :base/unique t :base/type integer)
                        '(mtime :base/unique t :base/type integer))
    ;; `ekg-note-cotypes' consults this to decide which types belong to a note;
    ;; without it `:vulpea/*' properties never make it back into a note struct.
    (triples-set-type ekg-db 'vulpea 'ekg-note-type)
    (dolist (prop '(:vulpea/state :vulpea/file :vulpea/pos
                    :vulpea/level :vulpea/mtime))
      (add-to-list 'ekg-header-hidden-properties prop))))

(defun ekg-vulpea--ensure-schema ()
  "Connect to ekg and make sure the vulpea schema exists."
  (ekg-connect)
  (unless (eq ekg-vulpea--schema-db ekg-db)
    (ekg-vulpea--add-schema)
    (setq ekg-vulpea--schema-db ekg-db)))

(defun ekg-vulpea--on-add-schema ()
  "Add the vulpea schema whenever ekg rebuilds its own schema."
  (ekg-vulpea--add-schema)
  (setq ekg-vulpea--schema-db ekg-db))

(add-hook 'ekg-add-schema-hook #'ekg-vulpea--on-add-schema)

;;; Helpers

(defun ekg-vulpea--directory ()
  "Return the vulpea directory to mirror, with a trailing slash."
  (file-name-as-directory
   (expand-file-name
    (or ekg-vulpea-sync-directory
        (and (boundp 'vulpea-db-sync-directories)
             (car vulpea-db-sync-directories))
        (bound-and-true-p org-directory)
        (user-error "ekg-vulpea: cannot find the vulpea directory; set `ekg-vulpea-sync-directory'")))))

(defun ekg-vulpea--file-mtime (file)
  "Return the modification time of FILE as an integer."
  (time-convert
   (file-attribute-modification-time (file-attributes file))
   'integer))

(defun ekg-vulpea--normalize-tags (tags)
  "Return TAGS split into ekg-style single tags.
vulpea keeps a `#+filetags: a b c' value as the single tag \"a b c\",
whereas org (and ekg) mean three tags.  Commas are removed and case is
folded, the same normalization `ekg-save-note' applies.

Tags in `ekg-hidden-tags' get the `vulpea/' prefix.  `draft' and
`trash' are ekg control tags: a vulpea note tagged `draft' would
otherwise disappear from every default view after being mirrored."
  (delete-dups
   (delq nil
         (mapcar (lambda (tag)
                   (let ((tag (string-trim
                               (downcase (string-replace "," "" tag)))))
                     (cond ((string-empty-p tag) nil)
                           ((member tag ekg-hidden-tags)
                            (concat ekg-vulpea-tag "/" tag))
                           (t tag))))
                 (seq-mapcat (lambda (tag) (split-string tag "[ \t]+" t))
                             tags)))))

(defun ekg-vulpea--tags (note)
  "Return the ekg tags for NOTE, including the mirror bookkeeping tags."
  (append (list ekg-vulpea-tag ekg-vulpea-mirror-tag)
          (ekg-vulpea--normalize-tags (vulpea-note-tags note))))

(defun ekg-vulpea-state (note)
  "Return NOTE's vulpea state string, or nil when it did not come from vulpea."
  (plist-get (ekg-note-properties note) :vulpea/state))

(defun ekg-vulpea-mirror-p (note)
  "Return non-nil when NOTE is still backed by a vulpea file."
  (equal (ekg-vulpea-state note) ekg-vulpea--state-mirror))

(defun ekg-vulpea--journal-p (note)
  "Return non-nil when NOTE's file matches `ekg-vulpea-journal-paths'."
  (let ((path (vulpea-note-path note)))
    (and path
         (seq-some (lambda (regexp) (string-match-p regexp path))
                   ekg-vulpea-journal-paths))))

(defun ekg-vulpea--ignored-note-p (note)
  "Return non-nil for a journal heading, which is not mirrored.
The day's file-level note already contains it, and journal headings
are sections rather than notes."
  (and (> (or (vulpea-note-level note) 0) 0)
       (ekg-vulpea--journal-p note)))

(defun ekg-vulpea--strip-all-drawers-p (note)
  "Return non-nil when NOTE's file should lose every property drawer."
  (ekg-vulpea--journal-p note))

(defun ekg-vulpea--strip-drawers (text)
  "Remove every `:PROPERTIES: ... :END:' block from TEXT."
  (replace-regexp-in-string
   "^[ \t]*:PROPERTIES:\n\\(?:[^\n]*\n\\)*?[ \t]*:END:\n?" "" text))

(defun ekg-vulpea--clean-text (text &optional all-drawers)
  "Strip the leading `:PROPERTIES: ... :END:' drawer from TEXT.
The drawer holds the org ID and bookkeeping properties; ekg already
shows the ID and title separately and the drawer only eats preview
space.  With ALL-DRAWERS, remove every such drawer, not just the
leading one (see `ekg-vulpea-strip-drawer-paths')."
  (let ((text (if all-drawers (ekg-vulpea--strip-drawers text) text)))
    (if (string-match "\\`:PROPERTIES:\n\\(?:[^\n]*\n\\)*?:END:\n?" text)
        (substring text (match-end 0))
      text)))

(defun ekg-vulpea--note-text (note)
  "Return NOTE's body from the current file buffer.
Level-0 notes take the whole file; heading notes take their subtree.
The buffer must already contain the file, and be in `org-mode' when
NOTE is a heading (level > 0)."
  (let* ((pos (max 1 (or (vulpea-note-pos note) 1)))
         (text (if (and (> (or (vulpea-note-level note) 0) 0)
                        (<= pos (point-max)))
                   (condition-case nil
                       (progn
                         (goto-char pos)
                         (save-restriction
                           (org-narrow-to-subtree)
                           (buffer-substring-no-properties (point-min) (point-max))))
                     (error
                      (buffer-substring-no-properties (point-min) (point-max))))
                 (buffer-substring-no-properties (point-min) (point-max)))))
    (ekg-vulpea--clean-text text (ekg-vulpea--strip-all-drawers-p note))))

(defun ekg-vulpea--write-mirror (note text mtime)
  "Store NOTE as an ekg mirror of TEXT, whose file was modified at MTIME.
The write goes straight to triples instead of through `ekg-save-note':
an import must not run the inline-tag rewriting or bump every mirror's
modified time to now, and none of ekg's save hooks are needed for a
read-only mirror."
  (let* ((id (vulpea-note-id note))
         (tags (ekg-vulpea--tags note))
         (title (string-trim
                 (replace-regexp-in-string "[\n\r]+" " "
                                           (or (vulpea-note-title note) ""))))
         (seconds (time-convert mtime 'integer)))
    (triples-with-transaction
      ekg-db
      (triples-set-type ekg-db id 'text
                        :text text :mode 'org-mode)
      (triples-set-type ekg-db id 'tagged :tag tags)
      (triples-set-type ekg-db id 'time-tracked
                        :creation-time seconds :modified-time seconds)
      (if (string-empty-p title)
          (triples-remove-type ekg-db id 'titled)
        (triples-set-type ekg-db id 'titled :title (list title)))
      (triples-set-type ekg-db id 'vulpea
                        :state ekg-vulpea--state-mirror
                        :file (vulpea-note-path note)
                        :pos (max 1 (or (vulpea-note-pos note) 1))
                        :level (or (vulpea-note-level note) 0)
                        :mtime seconds)
      ;; Keep the tag -> notes mapping ekg's tag views read.
      (dolist (tag tags)
        (triples-set-type ekg-db tag 'tag)))))

;;; Sync

(defvar ekg-vulpea--counts nil
  "Hash of sync counters, dynamically bound by `ekg-vulpea-sync'.")

(defvar ekg-vulpea--dirty-files nil
  "Vulpea files saved since the last `ekg-vulpea--sync-dirty' run.")

(defvar ekg-vulpea--autosync-timer nil
  "Pending timer for `ekg-vulpea--sync-dirty'.")

(defun ekg-vulpea--count (key &optional n)
  "Add N (default 1) to KEY in `ekg-vulpea--counts'."
  (puthash key (+ (gethash key ekg-vulpea--counts 0) (or n 1))
           ekg-vulpea--counts))

(defun ekg-vulpea--sync-note (note mtime force)
  "Refresh or create the ekg mirror for NOTE, modified at MTIME.
FORCE means re-read the text even when the stored mtime is current."
  (let* ((id (vulpea-note-id note))
         (existing (ekg-get-note-with-id id))
         (props (and existing (ekg-note-properties existing)))
         (state (plist-get props :vulpea/state))
         (stored (plist-get props :vulpea/mtime))
         (path (vulpea-note-path note))
         (pos (max 1 (or (vulpea-note-pos note) 1)))
         (level (or (vulpea-note-level note) 0)))
    (cond
     ((equal state ekg-vulpea--state-migrated)
      (ekg-vulpea--count 'migrated))
     ;; A trashed mirror stays trashed: deleting one in ekg must not make the
     ;; next sync bring it back.
     ((and existing (member ekg-trash-tag (ekg-note-tags existing)))
      (ekg-vulpea--count 'trashed))
     ;; A renamed or moved file keeps its mtime, so the path and position are
     ;; part of "unchanged" as well.
     ((and existing (not force) (integerp stored) (>= stored mtime)
           (equal (plist-get props :vulpea/file) path)
           (equal (plist-get props :vulpea/pos) pos)
           (equal (plist-get props :vulpea/level) level))
      (ekg-vulpea--count 'unchanged))
     (t
      (ekg-vulpea--write-mirror note (ekg-vulpea--note-text note) mtime)
      (ekg-vulpea--count (if existing 'updated 'created))))))

(defun ekg-vulpea--sync-file (file notes seen seen-files force)
  "Mirror NOTES, which all live in FILE.
SEEN and SEEN-FILES are updated so `ekg-vulpea--prune' can tell
which mirrors are genuinely stale.  Journal headings are skipped when
the file has a level-0 note; a journal file without one keeps its
headings mirrored, since they are then the only visible part."
  (condition-case err
      (let* ((mtime (ekg-vulpea--file-mtime file))
             (has-file-note
              (seq-some (lambda (note)
                          (zerop (or (vulpea-note-level note) 0)))
                        notes)))
        (puthash (expand-file-name file) t seen-files)
        (with-temp-buffer
          (insert-file-contents file)
          (when (seq-some (lambda (note) (> (or (vulpea-note-level note) 0) 0))
                          notes)
            ;; `org-narrow-to-subtree' is only needed for headings, and
            ;; `delay-mode-hooks' keeps the heavy org-mode-hook out of an
            ;; import that touches thousands of files.
            (delay-mode-hooks (org-mode)))
          (dolist (note notes)
            (if (and has-file-note (ekg-vulpea--ignored-note-p note))
                (ekg-vulpea--count 'ignored)
              (puthash (vulpea-note-id note) t seen)
              (ekg-vulpea--sync-note note mtime force)))))
    (error
     (ekg-vulpea--count 'failed)
     (message "ekg-vulpea: skipped %s: %s" file (error-message-string err)))))

(defun ekg-vulpea--refresh-views ()
  "Redraw the ekg notes buffers, ignoring a view that cannot be redrawn.
A view whose fetched notes were just deleted may still hold one of them
in its render state; that must not turn a successful sync into an
error."
  (condition-case err
      (ekg--refresh-notes-buffers)
    (error
     (message "ekg-vulpea: could not redraw an ekg view: %s"
              (error-message-string err)))))

(defun ekg-vulpea--prune (seen seen-files)
  "Delete mirrors whose vulpea source is gone.
SEEN holds the note IDs present in vulpea, SEEN-FILES the files this
run managed to read.  A mirror is only deleted when its file no longer
exists, or when the file was read but no longer produces that ID.  This
keeps a stale or half-scanned vulpea database from wiping the index."
  (dolist (id (triples-subjects-of-type ekg-db 'vulpea))
    (let* ((rec (triples-get-type ekg-db id 'vulpea))
           (state (plist-get rec :state))
           (file (plist-get rec :file)))
      (when (and (equal state ekg-vulpea--state-mirror)
                 (not (gethash id seen))
                 (or (null file)
                     (not (file-exists-p file))
                     (gethash (expand-file-name file) seen-files)))
        (ekg-note-delete-by-id id)
        (ekg-vulpea--count 'removed)))))

;;;###autoload
(defun ekg-vulpea-sync (&optional force)
  "Mirror every vulpea note into ekg.
Existing mirrors are refreshed only when the file changed, unless
FORCE (a prefix argument) is given, in which case all text is re-read.
Notes already promoted into ekg are left alone.  The command is
idempotent, so it can be re-run whenever vulpea changed externally.

When the ekg database holds no mirrors at all yet, this forces a
backup first (see `ekg-vulpea-backup-on-first-import')."
  (interactive "P")
  (require 'vulpea)
  (ekg-vulpea--ensure-schema)
  (let ((notes (vulpea-db-query)))
    (unless notes
      (user-error "ekg-vulpea: vulpea's database returned no notes; refusing to prune the index"))
    (when (and ekg-vulpea-backup-on-first-import
               (null (triples-subjects-of-type ekg-db 'vulpea)))
      (message "ekg-vulpea: first import, backing up the ekg database...")
      (condition-case err
          (ekg-backup t)
        (error
         (display-warning 'ekg-vulpea
                          (format "backup before the first import failed: %s"
                                  (error-message-string err))))))
    (let* ((by-file (seq-group-by #'vulpea-note-path notes))
           (seen (make-hash-table :test #'equal))
           (seen-files (make-hash-table :test #'equal))
           (ekg-vulpea--counts (make-hash-table)))
      (message "ekg-vulpea: mirroring %d notes from %d files..."
               (length notes) (length by-file))
      (let ((i 0))
        (dolist (entry by-file)
          (ekg-vulpea--sync-file (car entry) (cdr entry) seen seen-files force)
          (cl-incf i)
          (when (zerop (% i 200))
            (message "ekg-vulpea: %d/%d files..." i (length by-file)))))
      (ekg-vulpea--prune seen seen-files)
      (ekg-vulpea--refresh-views)
      (message "ekg-vulpea: %d created, %d updated, %d unchanged, %d migrated, %d trashed, %d ignored, %d removed, %d failed"
               (gethash 'created ekg-vulpea--counts 0)
               (gethash 'updated ekg-vulpea--counts 0)
               (gethash 'unchanged ekg-vulpea--counts 0)
               (gethash 'migrated ekg-vulpea--counts 0)
               (gethash 'trashed ekg-vulpea--counts 0)
               (gethash 'ignored ekg-vulpea--counts 0)
               (gethash 'removed ekg-vulpea--counts 0)
               (gethash 'failed ekg-vulpea--counts 0))
      nil)))

(defun ekg-vulpea--sync-dirty ()
  "Re-index the vulpea files that were saved recently.
Nothing is pruned here: the full sync owns deletion, and a single-file
view of vulpea's database is not evidence that a note is gone."
  (setq ekg-vulpea--autosync-timer nil)
  (let ((files ekg-vulpea--dirty-files))
    (setq ekg-vulpea--dirty-files nil)
    (when (and files (fboundp 'vulpea-db-query-by-file-path))
      (require 'vulpea)
      (ekg-vulpea--ensure-schema)
      (let ((ekg-vulpea--counts (make-hash-table)))
        (dolist (file files)
          (when (file-exists-p file)
            (condition-case err
                (ekg-vulpea--sync-file
                 file (vulpea-db-query-by-file-path file)
                 (make-hash-table :test #'equal)
                 (make-hash-table :test #'equal)
                 nil)
              (error
               (message "ekg-vulpea: skipped %s: %s" file
                        (error-message-string err))))))))))

(defun ekg-vulpea--after-save ()
  "Queue the current vulpea file for re-indexing."
  (when (and ekg-vulpea-autosync
             buffer-file-name
             (ignore-errors
               (string-prefix-p (ekg-vulpea--directory)
                                (expand-file-name buffer-file-name))))
    (add-to-list 'ekg-vulpea--dirty-files buffer-file-name)
    (unless ekg-vulpea--autosync-timer
      (setq ekg-vulpea--autosync-timer
            (run-at-time ekg-vulpea-autosync-delay nil
                         #'ekg-vulpea--sync-dirty)))))

(add-hook 'after-save-hook #'ekg-vulpea--after-save)

;;; Opening and migrating

(defun ekg-vulpea--origin (note)
  "Return (FILE . POS) for NOTE's vulpea source, or nil."
  (let ((props (ekg-note-properties note)))
    (when-let* ((file (plist-get props :vulpea/file)))
      (cons file (or (plist-get props :vulpea/pos) 1)))))

;;;###autoload
(defun ekg-vulpea-open-file (note &optional search)
  "Visit NOTE's vulpea source file at its recorded position.
SEARCH is an optional org search string (the part after `::' in an
`id:' link)."
  (interactive (list (ekg-current-note-or-error-expanded) nil))
  (let ((origin (ekg-vulpea--origin note)))
    (unless origin
      (user-error "This note has no vulpea source file"))
    (let ((file (car origin))
          (pos (cdr origin)))
      (unless (file-exists-p file)
        (user-error "vulpea source file is gone: %s" file))
      (find-file file)
      (widen)
      (goto-char (max (point-min) (min (point-max) pos)))
      (when (and search (derived-mode-p 'org-mode))
        (ignore-errors (org-link-search search)))
      (recenter))))

(defun ekg-vulpea--ekg-edit-open-mirror (note)
  "Open NOTE's vulpea file instead of an EKG edit buffer.
`ekg-edit' :before-until advice."
  (when (ekg-vulpea-mirror-p note)
    (ekg-vulpea-open-file note)
    t))

(unless (advice-member-p #'ekg-vulpea--ekg-edit-open-mirror 'ekg-edit)
  (advice-add 'ekg-edit :before-until #'ekg-vulpea--ekg-edit-open-mirror))

(defun ekg-vulpea--browse-mirror ()
  "Browse a vulpea-backed note by visiting its source file.
`ekg-notes-browse' :before-until advice."
  (let ((note (ignore-errors (ekg-current-note-or-error))))
    (when (and note (ekg-vulpea-mirror-p note))
      (ekg-vulpea-open-file note)
      t)))

(unless (advice-member-p #'ekg-vulpea--browse-mirror 'ekg-notes-browse)
  (advice-add 'ekg-notes-browse :before-until #'ekg-vulpea--browse-mirror))

(defun ekg-vulpea--open-id (link &rest _)
  "Open LINK through ekg when it names an ekg note.
LINK is the argument `org-id-open' receives: the link path, possibly
with a `::search' suffix.  Mirrors open their vulpea file; promoted and
plain ekg notes open in ekg.  Return nil for anything ekg does not
know, so plain org IDs keep org's own behaviour."
  (let* ((id (car (split-string link "::")))
         (search (cadr (split-string link "::")))
         (note (ignore-errors (ekg-get-note-with-id id))))
    (when note
      (if (ekg-vulpea-mirror-p note)
          (progn (ekg-vulpea-open-file note search) t)
        (progn (ekg-edit note) t)))))

(unless (advice-member-p #'ekg-vulpea--open-id 'org-id-open)
  (advice-add 'org-id-open :before-until #'ekg-vulpea--open-id))

(defun ekg-vulpea--archive-file (file)
  "Move FILE into `ekg-vulpea-archive-directory'.
A hidden directory is used so vulpea stops indexing the file."
  (let* ((dir (file-name-as-directory
               (expand-file-name
                (or ekg-vulpea-archive-directory
                    (expand-file-name ".ekg-migrated" (ekg-vulpea--directory))))))
         (target (expand-file-name (file-name-nondirectory file) dir)))
    (when (get-file-buffer file)
      (with-current-buffer (get-file-buffer file)
        (when (buffer-modified-p)
          (save-buffer))))
    (make-directory dir t)
    (rename-file file target t)
    (when-let* ((buf (get-file-buffer file)))
      (kill-buffer buf))
    (message "ekg-vulpea: archived %s -> %s" file target)))

(defun ekg-vulpea--sync-file-quietly (file)
  "Re-index the vulpea notes in FILE without messages."
  (require 'vulpea)
  (let ((inhibit-message t)
        (ekg-vulpea--counts (make-hash-table)))
    (ekg-vulpea--sync-file
     file (vulpea-db-query-by-file-path file)
     (make-hash-table :test #'equal)
     (make-hash-table :test #'equal)
     nil)))

;;;###autoload
(defun ekg-vulpea-promote (&optional archive)
  "Turn the note at point into a real ekg note.
The mirror's latest text is kept as the ekg text, the mirror tag is
removed and the state becomes `migrated': from then on ekg owns the
note, `id:' links open it in ekg, and sync no longer overwrites it.
With a prefix argument ARCHIVE, also move the source file to
`ekg-vulpea-archive-directory' once no un-migrated note remains in
it."
  (interactive "P")
  (let ((note (ekg-current-note-or-error-expanded)))
    (unless (ekg-vulpea-mirror-p note)
      (user-error "This note is not a vulpea mirror"))
    (let* ((id (ekg-note-id note))
           (file (plist-get (ekg-note-properties note) :vulpea/file))
           (stored (plist-get (ekg-note-properties note) :vulpea/mtime)))
      ;; Pick up edits that the save hook missed, then read the note back so
      ;; the save below works from the freshest struct.
      (when (and file (file-exists-p file)
                 (integerp stored)
                 (> (ekg-vulpea--file-mtime file) stored))
        (ekg-vulpea--sync-file-quietly file))
      (setq note (ekg-get-note-with-id id))
      (setf (ekg-note-tags note)
            (remove ekg-vulpea-mirror-tag (ekg-note-tags note)))
      (setf (ekg-note-properties note)
            (plist-put (ekg-note-properties note)
                       :vulpea/state ekg-vulpea--state-migrated))
      (ekg-save-note note)
      (when archive
        (ekg-vulpea--maybe-archive file)))
    (ekg-vulpea--refresh-views)
    (message "ekg-vulpea: promoted %s" (ekg-note-id note))))

(defun ekg-vulpea--maybe-archive (file)
  "Archive FILE when every note in it has been promoted."
  (let ((remaining 0))
    (dolist (id (triples-subjects-of-type ekg-db 'vulpea))
      (let ((rec (triples-get-type ekg-db id 'vulpea)))
        (when (and (equal (plist-get rec :state) ekg-vulpea--state-mirror)
                   (equal (plist-get rec :file) file))
          (cl-incf remaining))))
    (if (> remaining 0)
        (message "ekg-vulpea: %d un-migrated note(s) still in %s; not archiving"
                 remaining file)
      (ekg-vulpea--archive-file file))))

(defun ekg-vulpea--recent-mirror-notes (limit)
  "Return up to LIMIT un-migrated mirrors, most recently modified first.
Ids come from a single scan of `vulpea/mtime' so that the expensive
per-note `ekg-get-note-with-id' calls are limited to the notes that
are actually shown."
  (let* ((rows (sort (triples-db-select ekg-db nil 'vulpea/mtime)
                     (lambda (a b) (> (nth 2 a) (nth 2 b)))))
         (ids (mapcar #'car rows))
         (notes '()))
    (while (and ids (< (length notes) limit))
      (let ((note (ekg-get-note-with-id (pop ids))))
        (when (and note
                   (ekg-vulpea-mirror-p note)
                   (not (member ekg-trash-tag (ekg-note-tags note))))
          (push note notes))))
    (nreverse notes)))

;;;###autoload
(defun ekg-vulpea-show-mirrors (&optional all)
  "Show un-migrated vulpea notes, most recently modified first.
The whole corpus is 3k notes and rendering it costs seconds per
refresh, so only the first `ekg-vulpea-list-limit' are shown; with a
prefix argument every mirror is listed instead."
  (interactive "P")
  (let ((limit (if all most-positive-fixnum ekg-vulpea-list-limit)))
    (ekg-setup-notes-buffer
     (if all
         "vulpea (un-migrated)"
       (format "vulpea (un-migrated, %d most recent)" limit))
     (lambda () (ekg-vulpea--recent-mirror-notes limit))
     nil)))

;;;###autoload
(defun ekg-vulpea-remove-mirrors ()
  "Delete every un-migrated vulpea mirror from ekg.
Promoted notes are kept.  Use this to undo `ekg-vulpea-sync' or to
start the index over; `ekg-vulpea-sync' rebuilds it."
  (interactive)
  (ekg-vulpea--ensure-schema)
  (when (y-or-n-p "Delete all un-migrated vulpea mirrors from ekg? ")
    (let ((n 0))
      (dolist (id (triples-subjects-of-type ekg-db 'vulpea))
        (when (equal (plist-get (triples-get-type ekg-db id 'vulpea) :state)
                     ekg-vulpea--state-mirror)
          (ekg-note-delete-by-id id)
          (cl-incf n)))
      (ekg--refresh-notes-buffers)
      (message "ekg-vulpea: removed %d mirrors" n))))

(provide 'ekg-vulpea)
;;; ekg-vulpea.el ends here
