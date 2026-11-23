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
                        '((sequence "TODO(t)" "NEXT(n)" "STRT(s)" "WAIT(w@/!)"
                                    "HOLD(h@/!)" "BLKD(b@/!)" "|" "DONE(d!)" "CXLD(c@)")))
          (dolist (kw '("TODO" "NEXT" "STRT" "WAIT" "HOLD" "BLKD" "DONE" "CXLD"))
            ;; the ot view accepts it…
            (should (member kw (ekg-org--todo-keywords)))
            ;; …and the tag it writes reads back as the same state
            (should (equal (ekg-org-task-state
                            (make-ekg-note :tags (list (z/ekg-org--state-tag kw))))
                           kw))))
      (setq-default org-todo-keywords original))))

(ekg-deftest-with-db z/ekg-global-delete-tag--bare-and-prefix ()
  "Deleting a tag removes it from every note, and a prefix takes its children too.
Renaming to an empty string used to leave an empty tag behind, which is why this
command exists at all."
  (require 'ekg-tags-renaming)
  (let ((a (ekg-note-create :text "甲" :tags '("slip-box" "doc")))
        (b (ekg-note-create :text "乙" :tags '("slip-box/sub" "keep"))))
    (ekg-save-note a)
    (ekg-save-note b)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
              ((symbol-function 'ekg-backup) #'ignore)
              ((symbol-function 'ekg--refresh-notes-buffers) #'ignore))
      (z/ekg-global-delete-tag "slip-box" t))
    (should (equal (ekg-note-tags (ekg-get-note-with-id (ekg-note-id a))) '("doc")))
    (should (equal (ekg-note-tags (ekg-get-note-with-id (ekg-note-id b))) '("keep")))
    (should-not (z/ekg-tags-get-hierarchy-tags "slip-box"))))

(ekg-deftest-with-db z/ekg-global-rename-tag--empty-target-deletes ()
  "An empty TO-TAG means delete, not rename to an empty string."
  (require 'ekg-tags-renaming)
  (let ((a (ekg-note-create :text "甲" :tags '("slip-box" "doc"))))
    (ekg-save-note a)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
              ((symbol-function 'ekg-backup) #'ignore)
              ((symbol-function 'ekg--refresh-notes-buffers) #'ignore))
      (z/ekg-global-rename-tag "slip-box" ""))
    (should (equal (ekg-note-tags (ekg-get-note-with-id (ekg-note-id a))) '("doc")))
    (should-not (z/ekg-tags-get-hierarchy-tags "slip-box"))))

(ekg-deftest-with-db z/ekg-global-rename-tag-enhanced--empty-target-deletes ()
  "The enhanced rename takes the same empty target as a delete.
Its own preview/confirm buffer must not run at all: the advice intercepts before it."
  (require 'ekg-tags-renaming)
  (let ((a (ekg-note-create :text "甲" :tags '("slip-box" "doc")))
        (b (ekg-note-create :text "乙" :tags '("slip-box/sub" "keep")))
        (confirm-called nil))
    (ekg-save-note a)
    (ekg-save-note b)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
              ((symbol-function 'ekg-backup) #'ignore)
              ((symbol-function 'ekg--refresh-notes-buffers) #'ignore)
              ((symbol-function 'z/ekg-tag-rename--confirm)
               (lambda (&rest _) (setq confirm-called t) nil)))
      (z/ekg-global-rename-tag-enhanced "slip-box" ""))
    (should-not confirm-called)
    (should (equal (ekg-note-tags (ekg-get-note-with-id (ekg-note-id a))) '("doc")))
    (should (equal (ekg-note-tags (ekg-get-note-with-id (ekg-note-id b))) '("keep")))
    (should-not (z/ekg-tags-get-hierarchy-tags "slip-box"))))

;;; `lisp/init-agentic.el' lives in the Emacs config, not in ekg, so these tests find
;;; it by path and skip themselves when the fork is checked out on its own.

(defconst z/ekg-test--agentic-file
  (expand-file-name "../../../lisp/init-agentic.el"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to init-agentic.el in the surrounding Emacs config.")

(defun z/ekg-test--load-agentic ()
  "Load init-agentic.el, or skip the test when it is not there."
  (unless (file-exists-p z/ekg-test--agentic-file)
    (ert-skip "init-agentic.el is not next to this checkout"))
  (unless (fboundp 'z/agent--resolve-repo)
    (load z/ekg-test--agentic-file nil t)))

(ert-deftest z/agent--resolve-repo--prefers-dir ()
  "A DIR property that exists wins over everything else."
  (z/ekg-test--load-agentic)
  (let* ((dir (make-temp-file "z-agent-dir" t))
         (context (list :dir dir :repo-name "ignored")))
    (should (equal (z/agent--resolve-repo context) dir))
    (should (equal (plist-get context :base-repo) dir))
    (delete-directory dir t)))

(ert-deftest z/agent--resolve-repo--falls-back-to-project-tag ()
  "Without a usable DIR, a project/<repo> tag resolves under the worktree root."
  (z/ekg-test--load-agentic)
  (let* ((context (list :dir "/nonexistent/z-agent" :repo-name "some-repo"))
         (path (z/agent--resolve-repo context)))
    (should (equal path (expand-file-name "some-repo"
                                          (expand-file-name z/agent-worktree-root))))
    (should (equal (plist-get context :base-repo) path))))

(ert-deftest z/agent--resolve-repo--asks-when-both-missing ()
  "With neither DIR nor a tag it asks, and the answer still lands under the root."
  (z/ekg-test--load-agentic)
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _) "chosen-repo")))
    (let* ((context (list))
           (path (z/agent--resolve-repo context)))
      (should (equal path (expand-file-name "chosen-repo"
                                            (expand-file-name z/agent-worktree-root)))))))

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
