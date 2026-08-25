;;; ekg-ref.el --- Add ref -*- lexical-binding: t -*-

;; Copyright (C) 2026  Qingshui Zheng

;; Author: Qingshui Zheng <qingshuizheng@outlook.com>

;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;; Commentary:
;; Adds a "ref" (URL reference) property to ekg notes: `reffed/ref' holds the
;; URLs, `reffed' is the note type that makes them searchable, and the schema is
;; registered from `ekg-add-schema-hook' so it is (re)created on every
;; `ekg-connect', including after a database switch.
;;
;; The display template is *not* set here: this module only defines
;; `ekg-display-note-reffed' for `%n(reffed)', so the user's own
;; `ekg-display-note-template' decides whether and where refs are shown.

;;; Code:

(require 'ekg)
(require 'triples)
(require 'seq)

(defface ekg-ref
  '((((type graphic)) :inherit fixed-pitch)
    (((type tty))) :underline t)
  "Face shown for EKG ref.")

;; =====================================================================
;; Schema
;; =====================================================================

(defun ekg-ref-add-schema ()
  "Add the ref schema to the ekg database."
  (triples-add-schema ekg-db 'reffed '(ref :base/type string))
  (triples-add-schema ekg-db 'ref '(reffed :base/type string
                                           :base/virtual-reversed reffed/ref))
  (triples-set-type ekg-db 'reffed/ref 'ekg-property :name "Ref")
  (triples-set-type ekg-db 'reffed 'ekg-note-type))

(add-hook 'ekg-add-schema-hook 'ekg-ref-add-schema)

;; =====================================================================
;; Display
;; =====================================================================

(defun ekg-display-note-reffed (note &optional numwords &rest format)
  "返回 NOTE 的 ref 文本。单个 ref 可点击（RET/左键）查看相关笔记。"
  (ekg-display--format
   (if-let* ((refs (plist-get (ekg-note-properties note) :reffed/ref)))
       ;; 引用行：纯展示 + 可点击跳转（见 ekg-notes-alive 的 advice）
       (propertize (concat (mapconcat #'identity refs ", ") "\n")
                   'face 'ekg-ref
                   'ekg-ref-line t)
     "")
   numwords format))


(add-to-list 'ekg-query-pred-abbrevs '("ref" . "reffed/ref"))

(defun ekg-get-notes-with-ref (ref)
  "Get a list of note structs with REF."
  (ekg-connect)
  (delq nil (mapcar #'ekg-get-note-with-id
                    (triples-subjects-with-predicate-object
                     ekg-db 'reffed/ref ref))))

;;;###autoload
(defun ekg-show-notes-with-any-ref (refs)
  "Show notes that contain REF."
  (interactive (list (completing-read-multiple
                      "Refs: " (triples-subjects-of-type ekg-db 'ref))))
  (ekg-setup-notes-buffer
   (format "ref: %s" (ekg-tags-display refs))
   (lambda ()
     (sort
      (seq-uniq (mapcan (lambda (ref) (ekg-get-notes-with-ref ref)) refs))
      #'ekg-sort-by-creation-time))
   nil))

;;;###autoload
(defun z/ekg-browse-ref (ref)
  "Like ekg-browse-url, but directly choose from REF."
  (interactive
   (list (completing-read
          "URL: " (seq-filter
                   #'ffap-url-p
                   (progn (ekg-connect)
                          (triples-subjects-of-type ekg-db 'ref))))))
  (browse-url ref))

;; =====================================================================
;; Capture
;; =====================================================================

;; (z/ekg-capture-ref "http://baidu.com" "百度")
;; (z/ekg-capture-ref "http://qq.com" "腾讯")

;; (z/ekg-capture-ref "http://baidu.com" "联想")
;; (z/ekg-capture-ref "http://qq.com" "联想")
;; (z/ekg-capture-ref "http://github.com" "联想")

(defun z/ekg-capture-ref (ref title &optional tags)
  "Capture a new note given a REF and its TITLE with fuzzy matching.
If the ref has existing notes, choose from the existing. TAGS is
optional, and it should be a list."
  (interactive "MREF: \nMTitle: \n")
  (ekg-connect)
  (let* ((cleaned-title (string-replace "," "" title))
         ;; 1. Customize your default global tags here
         (default-global-tags '("captured-ref" "quick-note"))
         (final-tags (append tags default-global-tags))

         ;; 2. FUZZY MATCHING: Check for prefixes, suffixes, or sub-directory containment
         (partial-match-refs
          (seq-filter
           (lambda (r)
             (or (string-prefix-p r ref t)       ; Case-insensitive prefix
                 (string-prefix-p ref r t)       ; Case-insensitive reverse prefix
                 (string-match-p (regexp-quote (string-trim r)) ref))) ; Contained
           (triples-subjects-of-type ekg-db 'ref)))

         ;; 3. Merge database IDs safely without mutating cache entries
         (partial-match-ids
          (seq-mapcat
           (lambda (r) (plist-get (triples-get-type ekg-db r 'ref) :reffed))
           partial-match-refs))

         (edit-fn (lambda (id) (ekg-edit (ekg-get-note-with-id id))))
         (capture-fn
          (lambda ()
            (ekg-capture :tags final-tags
                         :properties `(:reffed/ref (,ref)
                                                   :titled/title (,cleaned-title))))))

    (if partial-match-ids
        (let ((col
               (mapcar
                (lambda (id)
                  (let* ((title-data (plist-get (triples-get-type ekg-db id 'titled) :title))
                         (ref-data (plist-get (triples-get-type ekg-db id 'reffed) :ref))
                         (tag-data (plist-get (triples-get-type ekg-db id 'tagged) :tag))
                         ;; Pre-build clean string formatting for Vertico view layout
                         (n-title (propertize (if title-data (mapconcat 'identity title-data " // ") "Untitled") 'face 'ekg-title))
                         (n-refs (if ref-data (mapconcat (lambda (r) (propertize r 'face 'ekg-ref)) ref-data " // ") ""))
                         (n-tags (if tag-data (mapconcat (lambda (t-name) (propertize t-name 'face 'ekg-tag)) tag-data " ") ""))
                         (display-string (format "%s // %s %s" n-title n-refs n-tags)))
                    (cons display-string id)))
                partial-match-ids)))
          (pcase (length col)
            (0 (funcall capture-fn))
            (_ (let* ((chosen-string (completing-read
                                      "Choose from existing Ref notes, or PRESS ‘C-j’ to capture new: "
                                      col))
                      (matched-id (cdr (assoc chosen-string col))))
                 (if (and matched-id (not (string-empty-p chosen-string)))
                     (funcall edit-fn matched-id)
                   (funcall capture-fn))))))
      (funcall capture-fn))))

(provide 'ekg-ref)
;;; ekg-ref.el ends here
