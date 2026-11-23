;;; egk-idea.el --- capture ideas and review them as tasks -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qingshui Zheng
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Ideas are the first state of the task pipeline rather than a parallel tagging
;; scheme: `IDEA' sits before `TODO' in `org-todo-keywords' (see init-org.el), so
;; promoting one is just `C-c C-t' -- no new state machinery.
;;
;;   C-c C-i   记一条 idea（一行，不问标题/标签）
;;   C-c C-t   在 review 视图里把它推到 TODO（或 CXLD 丢掉）
;;
;; `z/ekg-idea-review' opens the ideas as an ordinary notes view, stamps each one
;; with IDEA_REVIEWED, and prints how many there are.  `z/ekg-idea-stale' lists the
;; ones that have survived three weeks without a decision.

;;; Code:

(require 'subr-x)
;; ekg 只在真正记/查的时候加载：这个文件在启动时被 require，任何形式的顶层
;; require ekg 都会让启动倒退一秒多（`eval-when-compile' 在解释执行时也会跑 ✗）。
;; 只需要告诉编译器这些函数存在，就够展开访问器了。
(declare-function ekg-connect "ekg" (&optional force))
(declare-function ekg-get-notes-with-tag "ekg" (tag))
(declare-function ekg-note-create "ekg" (&rest args))
(declare-function ekg-save-note "ekg" (note))
(declare-function ekg-note-text "ekg" (note))
(declare-function ekg-note-tags "ekg" (note))
(declare-function ekg-note-properties "ekg" (note))
(declare-function ekg-note-creation-time "ekg" (note))
(declare-function ekg-tag-for-date "ekg" (&optional date))
(declare-function ekg-org-state-tag-prefix "ekg-org" ())
(declare-function ekg-org-set-property "ekg-org" (note key value))
(declare-function ekg-org-get-property "ekg-org" (note key))
(declare-function ekg-org-change-state "ekg-org" (new-state))
(declare-function ekg-llm--provider "ekg-llm" ())

(defun z/ekg-idea--ensure ()
  "Load ekg (and ekg-org) on first use."
  (require 'ekg)
  (require 'ekg-org))

(defcustom z/ekg-idea-state "IDEA"
  "The todo keyword that marks a captured idea."
  :type 'string)

(defun z/ekg-idea-tag ()
  "Return the tag that stores the IDEA state.
A function, not a constant: `ekg-org-state-tag-prefix' only exists once ekg-org is
loaded, and this file must not load ekg at startup."
  (require 'ekg-org)
  (concat ekg-org-state-tag-prefix (downcase z/ekg-idea-state)))

(defcustom z/ekg-idea-stale-days 21
  "Ideas untouched for this many days are reported by `z/ekg-idea-stale'."
  :type 'integer)

(defun z/ekg-idea--query-tags ()
  "Return the tags to query with.
`org/task' has to be included: it is in `ekg-hidden-tags' (ekg-org puts it there), and
`ekg-get-notes-with-tags' hides any note carrying a hidden tag unless that tag is
asked for explicitly."
  (require 'ekg-org)
  (list ekg-org-task-tag (z/ekg-idea-tag)))

(defun z/ekg-idea--notes ()
  "Return the notes that are still ideas, oldest first."
  (require 'ekg-org)
  (sort (ekg-get-notes-with-tags (z/ekg-idea--query-tags))
        (lambda (a b) (< (ekg-note-creation-time a) (ekg-note-creation-time b)))))

;;; Capturing

(defun z/ekg-capture-idea (text)
  "Save TEXT as an idea.
One line, no title and no tag prompts: anything that asks a question here makes
capturing cost more than the idea is worth.  The date tag is added automatically."
  (interactive "sIdea: ")
  (z/ekg-idea--ensure)
  (when (string-empty-p (string-trim (or text "")))
    (user-error "空 idea 就不记了"))
  (ekg-connect)
  (let ((note (ekg-note-create :text (string-trim text)
                               :mode 'org-mode
                               :tags (list ekg-org-task-tag
                                           (z/ekg-idea-tag)
                                           (ekg-tag-for-date)))))
    (ekg-save-note note)
    (message "记下了（第 %d 条待 review）" (length (z/ekg-idea--notes)))))

(defun z/ekg-capture-idea-from-context ()
  "Capture an idea out of whatever is around: the region, an elfeed entry, or a URL.
The source is kept in the `captured-ref' tag and the `:reffed/ref' property, so an
idea still points back to where it came from."
  (interactive)
  (z/ekg-idea--ensure)
  (ekg-connect)
  (let* ((region (when (use-region-p)
                   (string-trim (buffer-substring-no-properties (region-beginning) (region-end)))))
         (elfeed (and (bound-and-true-p elfeed-show-entry)
                      (cons (elfeed-entry-title elfeed-show-entry)
                            (elfeed-entry-link elfeed-show-entry))))
         (url (cond (elfeed (cdr elfeed))
                    ((and (derived-mode-p 'eww-mode) (plist-get eww-data :url)))
                    ((and (derived-mode-p 'elfeed-show-mode)
                          (bound-and-true-p elfeed-show-entry)
                          (elfeed-entry-link elfeed-show-entry)))))
         (title (cond (elfeed (car elfeed))
                      (region region)
                      (t (or (and (derived-mode-p 'eww-mode) (plist-get eww-data :title))
                             (buffer-name)))))
         (text (read-string "Idea: " (or region title))))
    (when (string-empty-p (string-trim (or text "")))
      (user-error "空 idea 就不记了"))
    (let ((note (ekg-note-create
                 :text (string-trim text)
                 :mode 'org-mode
                 :tags (append (list ekg-org-task-tag (z/ekg-idea-tag)
                                     (ekg-tag-for-date))
                               (when url '("captured-ref")))
                 :properties (when url
                               (list :reffed/ref (list url)
                                     :titled/title (list (string-trim (or title text))))))))
      (ekg-save-note note)
      (message "记下了（第 %d 条待 review）%s" (length (z/ekg-idea--notes))
               (if url (format "  ← %s" url) "")))))

;;; Reviewing

(defun z/ekg-idea--idea-tagged-notes ()
  "Return notes tagged `idea' or `idea/...' that are not tasks yet.
Upstream's `ekg-inline-custom-tag-completion-symbols' completes `!' to the `idea'
prefix, so a note written that way is an idea in the author's vocabulary -- it just
never became a task.  Reviewing promotes it (see `z/ekg-idea-review')."
  (let ((tags (seq-filter (lambda (tag)
                            (or (equal tag "idea")
                                (string-prefix-p "idea/" tag)))
                          (z/ekg-tags))))
    (seq-filter (lambda (note)
                  (not (member ekg-org-task-tag (ekg-note-tags note))))
                (seq-mapcat (lambda (tag) (ekg-get-notes-with-tag tag)) tags))))

(defun z/ekg-idea--promote-into-system (note)
  "Give NOTE the tags that make `ot' and the review view show it as an idea.
The plain `idea/...' tags stay: they say what the idea was about."
  (setf (ekg-note-tags note)
        (seq-uniq (append (list ekg-org-task-tag (z/ekg-idea-tag))
                          (ekg-note-tags note))))
  (ekg-save-note note))

(defun z/ekg-idea--stamp-reviewed (note)
  "Record that NOTE was looked at today, in the org property IDEA_REVIEWED.
`ekg-org-set-property' only mutates the struct, so save here."
  ;; 打戳只是"记一笔"，失败不该挡住 review（比如库还没建 org 的 schema）。
  (condition-case err
      (progn
        (require 'ekg-org)
        (ekg-org-add-schema)
        (ekg-org-set-property note "IDEA_REVIEWED" (format-time-string "%F"))
        (ekg-save-note note))
    (error (message "ekg-idea: 记 IDEA_REVIEWED 失败（不影响 review）：%s"
                    (error-message-string err)))))

(defun z/ekg-idea-review ()
  "Show every idea as a notes view, oldest first, and stamp them as reviewed.
In that view `C-c C-t' promotes an idea to TODO -- or parks it back on IDEA, or
cancels it, which is the same three-way decision every review needs."
  (interactive)
  (z/ekg-idea--ensure)
  ;; `idea/...' 标签的笔记先转正，再一起 review。
  (dolist (note (z/ekg-idea--idea-tagged-notes))
    (z/ekg-idea--promote-into-system note))
  (let ((notes (z/ekg-idea--notes)))
    (unless notes
      (user-error "没有待 review 的 idea"))
    (dolist (note notes)
      (z/ekg-idea--stamp-reviewed note))
    (ekg-show-notes-with-all-tags (z/ekg-idea--query-tags))
    (message "%d 条 idea 待 review（C-c C-t 决定：TODO / IDEA / CXLD）"
             (length notes))))

(defun z/ekg-idea-stale (&optional days)
  "List ideas nobody has looked at for DAYS (default `z/ekg-idea-stale-days').
Prints the ones that have been waiting longest; open them with `z/ekg-idea-review'."
  (interactive "P")
  (z/ekg-idea--ensure)
  (let* ((days (or days z/ekg-idea-stale-days))
         (cutoff (time-to-seconds (time-subtract (current-time) (* 86400 days))))
         (stale (seq-filter
                 (lambda (note)
                   (let* ((props (ekg-note-properties note))
                          (reviewed (plist-get props :idea-reviewed))
                          (since (or (and reviewed (float-time (date-to-time reviewed)))
                                     (ekg-note-creation-time note))))
                     (< since cutoff)))
                 (z/ekg-idea--notes))))
    (if (null stale)
        (message "没有超过 %d 天没动的 idea" days)
      (message "%d 条 idea 超过 %d 天没动：\n%s"
               (length stale) days
               (mapconcat (lambda (note)
                            (format "  · %s"
                                    (or (car-safe (plist-get (ekg-note-properties note)
                                                             :titled/title))
                                        (substring (or (ekg-note-text note) "") 0 60))))
                          stale "\n")))))

(defun z/ekg-idea-cluster ()
  "Ask the configured LLM to group the current ideas into a few themes.
Runs asynchronously and shows the answer in a buffer; nothing is written back."
  (interactive)
  (z/ekg-idea--ensure)
  (require 'ekg-llm)
  (let* ((notes (z/ekg-idea--notes))
         (texts (mapcar (lambda (note)
                          (or (car-safe (plist-get (ekg-note-properties note) :titled/title))
                              (ekg-note-text note)))
                        notes))
         (buffer (get-buffer-create "*ekg ideas clustered*")))
    (unless notes
      (user-error "没有待 review 的 idea"))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "对 %d 条 idea 分组中…\n" (length notes))))
    (display-buffer buffer)
    (llm-chat-async
     (ekg-llm--provider)
     (llm-make-chat-prompt
      (format "把下面这些 idea 归成 3-6 个主题，每个主题一行标题，下面列出属于它的 idea（原样引用，不要改写）。用中文回答。\n\n%s"
              (mapconcat (lambda (text) (format "- %s" text)) texts "\n")))
     (lambda (result)
       (with-current-buffer buffer
         (erase-buffer)
         (insert result)))
     (lambda (err)
       (message "idea 分组失败：%s" err)))))


(defalias 'z/ekg-idea-capture #'z/ekg-capture-idea
  "Old name of `z/ekg-capture-idea'.")

(provide 'egk-idea)
;;; egk-idea.el ends here
