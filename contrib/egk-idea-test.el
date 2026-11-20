;;; egk-idea-test.el --- tests for idea capture -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qingshui Zheng

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; The capture has to produce a task the `ot' view can see, which means the note
;; carries the hidden `org/task' tag plus `org/state/idea'; and reviewing has to be
;; nothing more than moving that state.

;;; Code:

(require 'ert)
(require 'ekg-test-utils)
(require 'egk-idea)
(require 'ekg-org)

(ekg-deftest-with-db z/ekg-capture-idea--creates-a-task ()
  "Capturing makes a note the ot view can see, in the IDEA state."
  (z/ekg-capture-idea "用 DSH 给 ekg 加个随机回顾")
  (let ((notes (z/ekg-idea--notes)))
    (should (= 1 (length notes)))
    (should (member "org/task" (ekg-note-tags (car notes))))
    (should (member (z/ekg-idea-tag) (ekg-note-tags (car notes))))
    (should (equal (ekg-org-task-state (car notes)) "IDEA"))
    (should (string-match-p "随机回顾" (ekg-note-text (car notes))))))

(ekg-deftest-with-db z/ekg-capture-idea--rejects-empty ()
  "An empty idea is not worth a note."
  (should-error (z/ekg-capture-idea "   ") :type 'user-error)
  (should (null (z/ekg-idea--notes))))

(ekg-deftest-with-db z/ekg-idea-review--stamps-and-promotes ()
  "Review stamps IDEA_REVIEWED, and C-c C-t in the view promotes the idea."
  (z/ekg-capture-idea "把 ot 缺的 8 个命令补键位")
  (z/ekg-idea-review)
  (let ((note (car (z/ekg-idea--notes))))
    (should (equal (ekg-org-get-property note "IDEA_REVIEWED")
                   (format-time-string "%F"))))
  (with-current-buffer (get-buffer (format "*ekg tags (all): %s, org/task*"
                                           (z/ekg-idea-tag)))
    (ekg-org-change-state "TODO"))
  (should (null (z/ekg-idea--notes)))
  (should (= 1 (length (ekg-get-notes-with-tags (list "org/task" "org/state/todo"))))))

(ert-deftest z/ekg-idea-capture-is-an-alias ()
  "The old name keeps working."
  (should (eq (indirect-function 'z/ekg-idea-capture)
              (indirect-function 'z/ekg-capture-idea))))

(provide 'egk-idea-test)
;;; egk-idea-test.el ends here
