;;; egk-apple-reminders.el --- Apple Reminders <-> ekg tasks -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qingshui Zheng
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; 双向同步：Apple Reminders 的一个清单 ↔ ekg 的 org task。
;;
;;   Reminders 里新建/改名的提醒  → ekg task（`org/task' + `org/state/todo'）
;;   Reminders 里勾完成 / 取消     → ekg 的 `org/state/done' / `org/state/todo'
;;   ekg 里带 `reminders' 标签的 task → 在 Reminders 里建提醒
;;   ekg 里改成 DONE              → 在 Reminders 里勾上
;;
;; 映射存在 ekg 笔记的 `:reminders/id' 属性上（schema `reminders'），所以重复跑
;; 不会重复建；每一侧都是 upsert。
;;
;;   M-x z/ekg-reminders-sync       双向同步
;;   C-u M-x z/ekg-reminders-sync   只预览计划，两边都不写

;;; Code:

(require 'seq)
(require 'subr-x)

(defcustom z/ekg-reminders-list "ekg"
  "Name of the Apple Reminders list used for sync."
  :type 'string)

(defcustom z/ekg-reminders-export-tag "reminders"
  "Tag marking ekg tasks that should be created in Reminders."
  :type 'string)

(defcustom z/ekg-reminders-create-list t
  "When non-nil, create the Reminders list if it does not exist yet."
  :type 'boolean)

(defun z/ekg-reminders--osa (script)
  "Run SCRIPT through osascript and return its output as a string."
  (with-temp-buffer
    (let ((code (call-process "osascript" nil t nil "-e" script)))
      (unless (zerop code)
        (error "Apple Reminders: osascript failed: %s" (buffer-string)))
      (string-trim (buffer-string)))))

(defun z/ekg-reminders--ensure-list ()
  "Create the sync list when it is missing."
  (let ((names (z/ekg-reminders--osa
                "tell application \"Reminders\"
set out to \"\"
repeat with l in lists
  set out to out & (name of l) & linefeed
end repeat
return out
end tell")))
    (unless (member z/ekg-reminders-list (split-string names "\n" t))
      (if z/ekg-reminders-create-list
          (z/ekg-reminders--osa
           (format "tell application \"Reminders\"
make new list with properties {name:\"%s\"}
end tell" (upcase z/ekg-reminders-list)))
        (user-error "Reminders 里没有清单 %s" z/ekg-reminders-list)))))

(defun z/ekg-reminders--reminders ()
  "Return the reminders in the sync list as plists.
Each one has :id, :name, :completed and :body."
  (let* ((raw (z/ekg-reminders--osa
               (format "tell application \"Reminders\"
set out to \"\"
repeat with r in reminders of list \"%s\"
  set out to out & (id of r) & tab & (name of r) & tab & (completed of r) & tab & (body of r) & linefeed
end repeat
return out
end tell" z/ekg-reminders-list)))
         (lines (split-string raw "\n" t)))
    (mapcar (lambda (line)
              (let ((fields (split-string line "\t")))
                (list :id (nth 0 fields)
                      :name (nth 1 fields)
                      :completed (string= "true" (nth 2 fields))
                      :body (or (nth 3 fields) ""))))
            lines)))

(defun z/ekg-reminders--add-schema ()
  "Make sure the mapping schema exists."
  (triples-add-schema ekg-db 'reminders
                      '(id :base/unique t :base/type string)))

(defun z/ekg-reminders--get-ekg-id (rem-id)
  "Return the ekg note id mapped to REM-ID, or nil."
  (car (triples-subjects-with-predicate-object ekg-db 'reminders/id rem-id)))

(defun z/ekg-reminders--set-mapping (note rem-id)
  "Record that NOTE is the ekg side of REM-ID."
  (triples-set-type ekg-db (ekg-note-id note) 'reminders :id rem-id))

(defun z/ekg-reminders--state-prefix ()
  "Return the `org/state/' tag prefix, loading ekg-org the first time."
  (require 'ekg-org)
  ekg-org-state-tag-prefix)

(defun z/ekg-reminders--state-tag (state)
  "Return the ekg tag for org TODO STATE."
  (concat (z/ekg-reminders--state-prefix) (downcase state)))

(defun z/ekg-reminders--set-state (note done)
  "Set NOTE to DONE when DONE, else to TODO (only the state tags change)."
  (let* ((state (if done "DONE" "TODO"))
         (prefix (z/ekg-reminders--state-prefix))
         (tags (seq-remove (lambda (tag)
                             (string-prefix-p prefix tag))
                           (ekg-note-tags note)))
         (new (cons (z/ekg-reminders--state-tag state) tags)))
    (triples-set-type ekg-db (ekg-note-id note) 'tagged :tag new)
    (setf (ekg-note-tags note) new)))

(defun z/ekg-reminders--task-done-p (note)
  "Return non-nil when NOTE's state tag is a done state."
  (seq-some (lambda (tag)
              (and (string-prefix-p ekg-org-state-tag-prefix tag)
                   (member (upcase (substring tag (length ekg-org-state-tag-prefix)))
                           '("DONE" "CANCELLED" "CANCELED"))))
            (ekg-note-tags note)))

(defun z/ekg-reminders--ekg-tasks ()
  "Return the ekg tasks that take part in the sync.
That is every mapped task, plus the ones carrying `z/ekg-reminders-export-tag'."
  (seq-filter
   (lambda (note)
     (or (plist-get (ekg-note-properties note) :reminders/id)
         (member z/ekg-reminders-export-tag (ekg-note-tags note))))
   (delq nil (mapcar #'ekg-get-note-with-id
                     (plist-get (triples-get-type ekg-db ekg-org-task-tag 'tag) :tagged)))))

(defun z/ekg-reminders--title (note)
  "Return NOTE's title as a string."
  (let ((title (plist-get (ekg-note-properties note) :titled/title)))
    (cond ((null title)
           (or (car (split-string (or (ekg-note-text note) "") "\n")) ""))
          ((listp title) (or (car title) ""))
          (t (format "%s" title)))))

(defun z/ekg-reminders-sync (&optional preview)
  "Two-way sync between Apple Reminders and ekg tasks.
With prefix argument PREVIEW, only report what would change."
  (interactive "P")
  (ekg-connect)
  (z/ekg-reminders--add-schema)
  (z/ekg-reminders--ensure-list)
  (let ((reminders (z/ekg-reminders--reminders))
        (created 0) (updated 0) (exported 0) (completed 0))
    ;; Reminders -> ekg
    (dolist (rem reminders)
      (let* ((rem-id (plist-get rem :id))
             (name (plist-get rem :name))
             (done (plist-get rem :completed))
             (body (plist-get rem :body))
             (ekg-id (z/ekg-reminders--get-ekg-id rem-id))
             (note (and ekg-id (ekg-get-note-with-id ekg-id))))
        (if note
            (progn
              (unless (and name (equal (z/ekg-reminders--title note) name))
                (setq updated (1+ updated))
                (unless preview
                  (triples-set-type ekg-db ekg-id 'titled :title (list name))))
              (unless (eq (and (z/ekg-reminders--task-done-p note) t) (and done t))
                (setq updated (1+ updated))
                (unless preview (z/ekg-reminders--set-state note done))))
          (when (and name (not (string-empty-p name)) (not done))
            (setq created (1+ created))
            (unless preview
              (let ((new (ekg-note-create
                          :text (or body "")
                          :mode 'org-mode
                          :tags (list ekg-org-task-tag
                                      (z/ekg-reminders--state-tag "TODO"))
                          :properties (list :titled/title (list name)))))
                (ekg-save-note new)
                (z/ekg-reminders--set-mapping new rem-id)))))))
    ;; ekg -> Reminders
    (dolist (note (z/ekg-reminders--ekg-tasks))
      (let* ((rem-id (plist-get (ekg-note-properties note) :reminders/id))
             (title (z/ekg-reminders--title note))
             (done (and (z/ekg-reminders--task-done-p note) t))
             (exists (and rem-id
                          (seq-some (lambda (r) (equal (plist-get r :id) rem-id))
                                    reminders))))
        (cond
         ((and exists done)
          (setq completed (1+ completed))
          (unless preview
            (z/ekg-reminders--osa
             (format "tell application \"Reminders\"\nset completed of reminder id \"%s\" of list \"%s\" to true\nend tell"
                     rem-id z/ekg-reminders-list))))
         ((and (not exists) (not (string-empty-p title)))
          (setq exported (1+ exported))
          (unless preview
            (let ((new-id (z/ekg-reminders--osa
                           (format "tell application \"Reminders\"\nmake new reminder at list \"%s\" with properties {name:\"%s\"}\nreturn id of result\nend tell"
                                   z/ekg-reminders-list
                                   (replace-regexp-in-string "\"" "'" title)))))
              (z/ekg-reminders--set-mapping note (string-trim new-id))))))))
    (message "Reminders %s：新建 ekg 任务 %d，更新 %d，写入 Reminders %d，勾完成 %d"
             (if preview "预览" "同步") created updated exported completed)))

(provide 'egk-apple-reminders)
;;; egk-apple-reminders.el ends here
