;;; egk-life.el --- 人生规划 skill（ekg + LLM） -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qingshui Zheng
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; 把打着 `z/ekg-life-tags' / `z/ekg-life-extra-tags' 里任一标签的笔记当作
;; "人生计划 / 资源"，连同**未完成的 org task**、以及最近生成的 todo 一起喂给
;; `ekg-llm-provider'（DeepSeek），让模型给出接下来的 todo。
;;
;;   M-x z/ekg-life-todos        生成 todo，并回写成一条 `life/todo' 笔记
;;   C-u M-x z/ekg-life-todos    只预览将要发送的 prompt（不花 token）
;;   总菜单（ekg → ai 分组）      iT
;;
;; 每天 08:00 检查一次，到了 `z/ekg-life-reminder-weekday'（1 = 周一）就提醒；
;; 设置 `z/ekg-life-auto-run' 为 t 则自动生成。
;;
;; 与 ekg-org 的关系：每日 todo 就用 ekg-org task（`org/task' 标签 + TODO 状态
;; + 可选 org/scheduled），这样它们会出现在 `ekg-org-view' 和 org agenda 里，
;; 下一次生成时又会被读进 prompt，形成 计划 → todo → 复盘 的闭环。

;;; Code:

(require 'seq)

;; ── 人生规划 skill ─────────────────────────────────────────────
;; 把打着 `z/ekg-life-tags' 里任一标签的笔记当作"人生计划/资源"，
;; 交给 DeepSeek 生成"接下来做什么"。用法：
;;   M-x z/ekg-life-todos        → 生成 todo（新 buffer）
;;   C-u M-x z/ekg-life-todos    → 只预览将要发送的 prompt（不花 token）
;; 菜单里在 ai 分组：`iT'
(defcustom z/ekg-life-tags '("life" "pi-skills/life-plan-iter")
  "Tags whose notes make up the life plan and the resource lists.
`life' is the only tag that matters: it marks a note as life-plan material, and
the horizon (long/mid/short) belongs in the title, where both the reader and the
model can see it.  The role tags (life/resource, life/manpower, life/learning)
stay on the notes for filtering, but they do not have to be listed here.

`person/*' tags are deliberately not included: the friend list says nothing about
which todo comes next and would only eat context."
  :type '(repeat string))

(defun z/ekg-life--tags ()
  "Return every tag whose notes take part in the life plan."
  (delete-dups (copy-sequence z/ekg-life-tags)))

(defun z/ekg-life--title (note)
  "Return NOTE's title as a string, or an empty string."
  (let ((title (plist-get (ekg-note-properties note) :titled/title)))
    (cond ((null title) "")
          ((listp title) (mapconcat #'identity title ", "))
          (t (format "%s" title)))))

(defun z/ekg-life--notes ()
  "Return the notes that make up the life plan, plus this month's roster.
The roster lives under a `roster/YYYY-MM' tag and is what makes the fragmented
time concrete."
  (let ((tags (append (z/ekg-life--tags)
                      (list (concat "roster/" (format-time-string "%Y-%m")))))
        notes)
    (dolist (tag tags)
      (dolist (id (plist-get (triples-get-type ekg-db tag 'tag) :tagged))
        (let ((note (ekg-get-note-with-id id)))
          (when note (push note notes)))))
    (delete-dups notes)))

(defun z/ekg-life--open-tasks ()
  "Return the org tasks that are not done yet.
Scheduled and deadline are stored directly in the note properties as epoch
integers, so they are read here and formatted for the prompt."
  (when (fboundp 'ekg-org-get-tasks)
    (seq-filter (lambda (note)
                  (not (and (fboundp 'ekg-org-task-done-p)
                            (ekg-org-task-done-p note))))
                (ekg-org-get-tasks))))

(defun z/ekg-life--render-tasks (tasks)
  "Render TASKS as a prompt section."
  (mapconcat
   (lambda (note)
     (let* ((props (ekg-note-properties note))
            (when- (or (plist-get props :org/scheduled)
                       (plist-get props :org/deadline)))
            (title (z/ekg-life--title note)))
       (format "- [%s] %s%s"
               (or (and (fboundp 'ekg-org-task-state) (ekg-org-task-state note)) "TODO")
               (if (string-empty-p title) (z/ekg-life--clip (ekg-note-text note)) title)
               (if when-
                   (format "（%s）" (if (numberp when-)
                                      (format-time-string "%Y-%m-%d" when-)
                                    when-))
                 ""))))
   tasks "\n"))

(defun z/ekg-life--context ()
  "Render the life-plan notes as prompt context."
  (mapconcat
   (lambda (note)
     (format "### %s\n标签：%s\n\n%s\n"
             (let ((title (z/ekg-life--title note)))
               (if (string-empty-p title) (无标题) title))
             (mapconcat #'identity (ekg-note-tags note) ", ")
             (or (ekg-note-text note) "")))
   (z/ekg-life--notes) "\n"))

(defun z/ekg-life--prompt ()
  "Build the prompt sent to the model."
  (concat
   "你是我的生活规划助手。下面是我的人生计划与资源笔记（长期/中期/短期）。\n\n"
   "请输出：\n"
   "1. 现在最该做的 3-5 个下一步 todo（按优先级排序，每条一行）；\n"
   "2. 每个 todo 后面注明：为什么现在做、本周能完成的最小动作；\n"
   "3. 计划里互相冲突、或明显被忽略的部分。\n\n"
   "要求：\n"
   "- 用中文；条目式；不要空话和鼓励语；信息不足就直说缺什么；\n"
   "- 每个 todo 标明所需时间块（15 / 30 / 60 分钟），并说明能塞进哪段\n"
   "  碎片时间（工作日的小块 / 周末的长块）；不要建议需要半天以上的事。\n\n"
   (z/ekg-life--context)
   (let ((tasks (z/ekg-life--open-tasks)))
     (when tasks
       (concat "\n## 当前已排但未完成的任务（不要重复提议；可调整优先级）\n\n"
               (z/ekg-life--render-tasks tasks))))
   (let ((todos (z/ekg-life--recent-todos)))
     (when todos
       (concat "\n## 我之前生成的 todo（可据此判断完成到哪、卡在哪）\n\n"
               (z/ekg-life--render todos))))))

;; ── 人生规划 skill（续）：回写、上下文控制、提醒 ──────────────
(defcustom z/ekg-life-todo-tag "life/todo"
  "Tag of the notes the generated todos are written to."
  :type 'string)

(defcustom z/ekg-life-recent-days 14
  "How many days of earlier todo notes to include in the prompt."
  :type 'integer)

(defcustom z/ekg-life-max-chars 4000
  "Maximum characters of one note to include in the prompt."
  :type 'integer)

(defcustom z/ekg-life-reminder-weekday 1
  "Weekday for the reminder, 1 = Monday."
  :type 'integer)

(defcustom z/ekg-life-auto-run nil
  "When non-nil, the reminder generates the todos instead of just reminding."
  :type 'boolean)

(defun z/ekg-life--clip (text)
  "Return TEXT, cut to `z/ekg-life-max-chars' if longer."
  (let ((text (or text "")))
    (if (> (length text) z/ekg-life-max-chars)
        (concat (substring text 0 z/ekg-life-max-chars) "…")
      text)))

(defun z/ekg-life--notes-with-tag (tag)
  "Return the notes carrying TAG."
  (delq nil (mapcar #'ekg-get-note-with-id
                    (plist-get (triples-get-type ekg-db tag 'tag) :tagged))))

(defun z/ekg-life--recent-todos ()
  "Return the todo notes from the last `z/ekg-life-recent-days' days."
  (let ((since (- (float-time) (* 86400 z/ekg-life-recent-days))))
    (seq-filter (lambda (note)
                  (>= (or (ekg-note-creation-time note) 0) since))
                (z/ekg-life--notes-with-tag z/ekg-life-todo-tag))))

(defun z/ekg-life--render (notes)
  "Render NOTES as prompt context."
  (mapconcat
   (lambda (note)
     (format "### %s\n标签：%s\n\n%s\n"
             (let ((title (z/ekg-life--title note)))
               (if (string-empty-p title) "(无标题)" title))
             (mapconcat #'identity (ekg-note-tags note) ", ")
             (z/ekg-life--clip (ekg-note-text note))))
   notes "\n"))

(defun z/ekg-life--write-todo-note (answer)
  "Create or update today's todo note with ANSWER and return it."
  (let* ((date (format-time-string "%Y-%m-%d"))
         (title (format "人生 todo %s" date))
         (existing (seq-find (lambda (n) (string= (z/ekg-life--title n) title))
                             (z/ekg-life--notes-with-tag z/ekg-life-todo-tag))))
    (if existing
        (progn (setf (ekg-note-text existing) answer)
               (ekg-save-note existing)
               existing)
      (let ((note (ekg-note-create
                   :text answer
                   :mode 'org-mode
                   :tags (list z/ekg-life-todo-tag (concat "date/" date))
                   :properties (list :titled/title (list title)))))
        (ekg-save-note note)
        note))))

(defun z/ekg-life--remind ()
  "Remind (or auto-run) about the life todos on the configured weekday."
  (when (= (string-to-number (format-time-string "%u")) z/ekg-life-reminder-weekday)
    (if z/ekg-life-auto-run
        (condition-case err (z/ekg-life-todos)
          (error (message "ekg-life：自动生成失败：%s" (error-message-string err))))
      (message "该跑一次人生 todo 了：M-x z/ekg-life-todos（菜单 iT）"))))

;; 每天 08:00 检查一次；跨重启会重新排定。
(run-at-time "08:00" (* 24 3600) #'z/ekg-life--remind)

;; 重载本文件时不要叠出多个定时器。
(defvar z/ekg-life--timer nil
  "Timer for `z/ekg-life--remind'.")

(when z/ekg-life--timer
  (cancel-timer z/ekg-life--timer))
(setq z/ekg-life--timer
      (run-at-time "08:00" (* 24 3600) #'z/ekg-life--remind))

(provide 'egk-life)
;;; egk-life.el ends here
