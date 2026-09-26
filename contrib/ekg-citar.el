;;; ekg-citar.el --- Citar integration for EKG -*- lexical-binding: t -*-

;; Copyright (C) 2023-2026 Qingshui Zheng

;; Author: Qingshui Zheng <qingshuizheng@outlook.com>

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Integrate `citar' with `ekg': create bibliographic notes, browse
;; existing notes from the citar interface, and jump between citations
;; and their notes.
;;
;; Requires `ekg-ref' (part of ekg contrib) for the `reffed/ref' property.
;;
;;; Code:

(require 'triples)
(require 'ekg)
(require 'ekg-ref)
(require 'citar)

(defgroup ekg-citar nil
  "Citar integration for EKG."
  :group 'ekg
  :prefix "ekg-citar-")

(defcustom ekg-citar-tag "bib"
  "Tag to indicate bibliographical notes."
  :group 'ekg-citar
  :type 'string)

(defcustom ekg-citar-capture-mode 'org-mode
  "Major mode used when creating a new bibliographic note.

Please note that citations are not supported in plain text."
  :group 'ekg-citar
  :type '(choice
          (const :tag "Org mode (default)" org-mode)
          (const :tag "Markdown mode" markdown-mode)
          (const :tag "Plain text" text-mode)))

(defcustom ekg-citar-title-template "${author} (${year}) ${title}"
  "Template for new note titles.
Uses citar template variables, e.g. ${author}, ${title}, ${year}, ${date}.
Set to empty string or nil to use the citekey as the title."
  :group 'ekg-citar
  :type 'string)

(defcustom ekg-citar-use-bib-tags nil
  "When non-nil, add BibTeX keywords as ekg tags on note creation."
  :group 'ekg-citar
  :type 'boolean)

(defcustom ekg-citar-open-attachment t
  "When non-nil, open the first PDF in another window after creating a note."
  :group 'ekg-citar
  :type 'boolean)

;;; Title generation

(defun ekg-citar--generate-title (citekey)
  "Generate title for new bibliographic note using CITEKEY.
Uses `ekg-citar-title-template' via citar's template engine."
  (if (or (null ekg-citar-title-template)
          (string-empty-p ekg-citar-title-template))
      citekey
    (or (citar-format--entry ekg-citar-title-template
                             (citar-get-entry citekey))
        citekey)))

;;; Citar notes source

(defconst ekg-citar-notes-config
  (list :name "Ekg Notes"
        :category 'ekg-note
        :items #'ekg-citar--get-candidates
        :hasitems #'ekg-citar--has-notes
        :open #'ekg-citar--open-note
        :create #'ekg-citar--create-note
        :annotate #'ekg-citar--annotate))

(defvar citar-notes-source)

(defun ekg-citar--get-candidates (&optional citekeys)
  "Return hash table of Ekg notes associated with CITEKEYS citation keys.

If CITEKEYS is omitted, return all Ekg notes containing refs."
  (let ((cands (make-hash-table :test 'equal)))
    (prog1 cands
      (if citekeys
          ;; Targeted lookup: use the reverse index from ekg-ref.
          (dolist (ref citekeys)
            (dolist (note (ekg-get-notes-with-ref ref))
              (push (number-to-string (ekg-note-id note))
                    (gethash ref cands))))
        ;; Full scan: iterate all reffed notes and collect their refs.
        (dolist (id (triples-subjects-of-type ekg-db 'reffed))
          (let ((refs (plist-get (ekg-note-properties (ekg-get-note-with-id id))
                                 :reffed/ref)))
            (dolist (r refs)
              (push (number-to-string id) (gethash r cands))))))
      (maphash (lambda (ref notelist)
                 (puthash ref (nreverse notelist) cands))
               cands))))

(defun ekg-citar--has-notes ()
  "Return predicate: CITEKEY has an associated ekg note.
Return nil if no bibliography notes exist at all."
  (let ((hasnotes (make-hash-table :test 'equal)))
    (dolist (citekey (triples-subjects-of-type ekg-db 'ref))
      (puthash citekey t hasnotes))
    (unless (hash-table-empty-p hasnotes)
      (lambda (citekey)
        (gethash citekey hasnotes)))))

(defun ekg-citar--open-note (ekg-note-id)
  "Open ekg note for EKG-NOTE-ID."
  (ekg-edit (ekg-get-note-with-id (string-to-number ekg-note-id))))

(defun ekg-citar--extract-bib-tags (citekey)
  "Extract BibTeX keywords for CITEKEY as ekg tag strings."
  (when-let* ((keywords (citar-get-value "keywords" citekey)))
    (mapcar (lambda (w) (replace-regexp-in-string " " "-" w))
            (split-string keywords ", *"))))

(defun ekg-citar--create-note (citekey &optional _entry)
  "Create a bibliographic note for CITEKEY.

The mode is determined by `ekg-citar-capture-mode'.
The title is generated from `ekg-citar-title-template'."
  (let* ((tags (if ekg-citar-use-bib-tags
                   (delete-dups (cons ekg-citar-tag
                                      (ekg-citar--extract-bib-tags citekey)))
                 (list ekg-citar-tag))))
    (ekg-capture
     :tags tags
     :mode ekg-citar-capture-mode
     :properties `( :titled/title ,(list (ekg-citar--generate-title citekey))
                    :reffed/ref ,(list citekey)))
    (when (and ekg-citar-open-attachment
               (citar-get-value "file" citekey))
      (when (one-window-p) (split-window-right))
      (other-window 1)
      (citar-open-files citekey))))

(defun ekg-citar--annotate (candidate)
  "Annotate the CANDIDATE (a note id string)."
  (when-let* ((id (string-to-number candidate))
              (note (ekg-get-note-with-id id))
              (titles (plist-get (ekg-note-properties note) :titled/title))
              (refs (plist-get (ekg-note-properties note) :reffed/ref)))
    (propertize (concat " [" (mapconcat #'identity refs ",") "] "
                        (mapconcat #'identity titles " // "))
                'face 'citar)))

;;; Public commands

;;;###autoload
(defun ekg-citar-cited (citekey)
  "Display notes that reference CITEKEY."
  (interactive (list (citar-select-ref :filter (citar-has-notes))))
  (let* ((ids (plist-get (triples-get-type ekg-db citekey 'ref) :reffed)))
    (ekg-setup-notes-buffer
     (format "Notes citing \"%s\": " citekey)
     (lambda ()
       (sort (mapcar #'ekg-get-note-with-id ids)
             #'ekg-sort-by-creation-time))
     nil)))

;;;###autoload
(defun ekg-citar-dwim ()
  "Open citar for the refs of the current ekg note."
  (interactive)
  (if-let* ((refs (plist-get (ekg-note-properties ekg-note) :reffed/ref))
            (key (if (= (length refs) 1)
                     (car refs)
                   (citar-select-ref))))
      (citar-open (list key))
    (user-error "Current note has no refs")))

;;;###autoload
(defun ekg-citar-nocite ()
  "Open bibliography entries that have no associated ekg note."
  (interactive)
  (let* ((all (hash-table-keys (citar-get-entries)))
         (noted (hash-table-keys (ekg-citar--get-candidates)))
         (unused (seq-difference all noted)))
    (if unused
        (citar-open unused)
      (message "All bibliography entries have notes"))))

;;;###autoload
(defun ekg-citar-nobib ()
  "List refs in ekg notes that no longer exist in the bibliography."
  (interactive)
  (let* ((bib-keys (hash-table-keys (citar-get-entries)))
         (refd-keys (hash-table-keys (ekg-citar--get-candidates)))
         (dead (seq-difference refd-keys bib-keys)))
    (if dead
        (message "Dead refs: %s" (mapconcat #'identity dead ", "))
      (message "All refs resolve to bibliography entries"))))

;;; Minor mode

(defvar ekg-citar--orig-source nil
  "The `citar-notes-source' before `ekg-citar-mode' was activated.")

(defun ekg-citar-setup ()
  "Set up `ekg-citar-mode'."
  (setq ekg-citar--orig-source citar-notes-source)
  (citar-register-notes-source 'ekg-citar ekg-citar-notes-config)
  (setq citar-notes-source 'ekg-citar))

(defun ekg-citar-reset ()
  "Reset `ekg-citar-mode' to the previous notes source."
  (setq citar-notes-source ekg-citar--orig-source)
  (citar-remove-notes-source 'ekg-citar))

;;;###autoload
(define-minor-mode ekg-citar-mode
  "Toggle `ekg-citar-mode'."
  :global t
  :group 'ekg-citar
  :lighter nil
  (if ekg-citar-mode
      (ekg-citar-setup)
    (ekg-citar-reset)))

(provide 'ekg-citar)
;;; ekg-citar.el ends here
