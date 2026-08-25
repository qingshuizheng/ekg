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

;;; Commentary:
;; This package provides a way to store email in a structured way, in a note.
;; It has integration with gnus, and other integrations can be easily added.

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

(setq ekg-display-note-template
      "%n(id)%n(tagged)%n(titled)%n(reffed)%n(text 500)%n(other)")
(setq ekg-oneliner-note-template
      "%n(tagged 20 oneline) %n(titled 20 oneline) %n(reffed 20 oneline) %n(text 40 oneline)")

(defun ekg-display-note-reffed (note &optional numwords &rest format)
  "返回 NOTE 的 ref 文本。当光标在整个引用行区域时，按 a 增加引用，按 r 删除引用。
古典 defun 直铺字符串版，括号完美对齐配平，无任何多余 if 判定。"
  (ekg-display--format
   (if-let* ((refs (plist-get (ekg-note-properties note) :reffed/ref)))
       (let ((ref-map (make-sparse-keymap)))
         ;; ==================== 【外层专属按键绑定（无 if 判定）】 ====================
         (define-key ref-map (kbd "a") #'ekg-note-add-property-at-point)
         (define-key ref-map (kbd "r") #'ekg-note-remove-property-at-point)

         ;; ==================== 【文本属性深度投射】 ====================
         ;; 盖上 'ekg-ref-line t 印记，完美桥接 At Point 自动角色推导内核
         (propertize (concat (mapconcat #'identity refs ", ") "\n")
                     'face 'ekg-ref
                     'keymap ref-map
                     'ekg-ref-line t))
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
