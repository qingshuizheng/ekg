;;; egk-extras-test.el --- tests for the z/ layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qingshui Zheng

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; The z/ layer resolves "the note at point" in an `ekg-notes-mode' buffer, and
;; everything else (uu/ut/ur/ux, the tags fallback of `u') hangs off it.  The
;; views put a triangle before the note id, so the `:ekg-note-id' property starts
;; in the middle of the line while `ekg-notes-next'/`-previous' leave point at the
;; beginning of it.  That mismatch broke the commands once; these tests pin it.

;;; Code:

(require 'ert)
(require 'ekg-test-utils)
(require 'ekg-extras)

(defun z/ekg-test--first-note-pos ()
  "Return the first position in this buffer that carries a note id."
  (let ((pos (point-min)) found)
    (while (and (not found) (< pos (point-max)))
      (when (get-text-property pos :ekg-note-id)
        (setq found pos))
      (setq pos (or (next-single-property-change pos :ekg-note-id nil (point-max))
                    (point-max))))
    found))

(ekg-deftest-with-db z/ekg-note--current-note-works-at-line-start ()
  "A note is found from the start of its line, not only from its id."
  (let ((note (ekg-note-create :text "正文" :tags '("line-start"))))
    (ekg-save-note note)
    (ekg-show-notes-with-tag "line-start")
    (with-current-buffer (get-buffer "*ekg tag: line-start*")
      (let ((pos (z/ekg-test--first-note-pos)))
        (should pos)
        (goto-char pos)
        (should (equal (ekg-note-id (z/ekg-note--current-note))
                       (ekg-note-id note)))
        (goto-char (line-beginning-position))
        (should (equal (ekg-note-id (z/ekg-note--current-note))
                       (ekg-note-id note)))))))

(ekg-deftest-with-db z/ekg-note--current-note-after-movement ()
  "After `ekg-notes-next' the note at point is the one that was moved to."
  (let ((first (ekg-note-create :text "第一条" :tags '("moving")))
        (second (ekg-note-create :text "第二条" :tags '("moving"))))
    (ekg-save-note first)
    (ekg-save-note second)
    (ekg-show-notes-with-tag "moving")
    (with-current-buffer (get-buffer "*ekg tag: moving*")
      (goto-char (z/ekg-test--first-note-pos))
      (let ((start (ekg-note-id (z/ekg-note--current-note))))
        (ekg-notes-next)
        (let ((after (z/ekg-note--current-note)))
          (should after)
          (should-not (equal (ekg-note-id after) start))
          ;; and back
          (ekg-notes-previous)
          (should (equal (ekg-note-id (z/ekg-note--current-note)) start)))))))

(ekg-deftest-with-db z/ekg-notes-refresh-edit-buffers--updated-and-conflict ()
  "An unmodified edit buffer follows the database; a modified one is left alone."
  (let* ((note (ekg-note-create :text "旧正文"))
         (id (ekg-note-id note)))
    (ekg-save-note note)
    (let ((other (ekg-get-note-with-id id)))
      (setf (ekg-note-text other) "同步内容")
      (ekg-save-note other))
    (with-current-buffer (get-buffer-create "*ekg edit test*")
      (insert "我改了一半")
      (setq-local ekg-note note)
      (ekg-edit-mode 1)
      (set-buffer-modified-p t)
      (z/ekg-notes-refresh-edit-buffers)
      (should (equal (buffer-string) "我改了一半"))
      (set-buffer-modified-p nil)
      (z/ekg-notes-refresh-edit-buffers)
      (should (equal (buffer-string) "同步内容"))
      (kill-buffer))))

(ert-deftest z/ekg-org-state-tag-round-trip ()
  "The tag written for a state reads back as the same state.
One vocabulary (`org-todo-keywords'), one tag shape
(`ekg-org-state-tag-prefix' + the lowercase keyword)."
  (require 'ekg-org)
  ;; `ekg-org--todo-keywords' reads the *global* value, so this has to go through
  ;; `setq-default'; a plain `let' would only bind it buffer-locally.
  (let ((original (default-value 'org-todo-keywords)))
    (unwind-protect
        (progn
          (setq-default org-todo-keywords
                        '((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "WAITING(w@/!)"
                                    "STALLED(x@/!)" "|" "DONE(d!)" "CANCELLED(c@)")))
          (dolist (kw '("TODO" "NEXT" "STARTED" "WAITING" "STALLED" "DONE" "CANCELLED"))
            ;; the ot view accepts it…
            (should (member kw (ekg-org--todo-keywords)))
            ;; …and the tag it writes reads back as the same state
            (should (equal (ekg-org-task-state
                            (make-ekg-note :tags (list (z/ekg-org--state-tag kw))))
                           kw))))
      (setq-default org-todo-keywords original))))

;;; Two upstream tests assert the display behaviour this fork deliberately changes:
;;; `ekg-test-display-note-template' expects the original template (we put the tags
;;; first, right-align the time and drop the "-->" marker) and
;;; `ekg-test-notes-title-text-fallback' expects marker text without properties
;;; (we add `:ekg-note-id' and friends).  Marking them as expected failures keeps
;;; one command able to run everything, so a *new* failure still stands out.

(dolist (z/ekg-test-name '(ekg-test-display-note-template
                           ekg-test-notes-title-text-fallback))
  (when-let* ((test (ert-get-test z/ekg-test-name)))
    (setf (ert-test-expected-result-type test) :failed)))

(provide 'egk-extras-test)
;;; egk-extras-test.el ends here
