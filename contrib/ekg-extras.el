;;; ekg-extras.el --- Extra EKG commands and utilities -*- lexical-binding: t -*-

;; Copyright (C) 2026  Qingshui Zheng

(require 'ekg)
(require 'triples)
(require 'seq)

;; =====================================================================
;;; Database management
;; =====================================================================

(defun z/ekg-db-face-update (db)
  "Update `ekg-notes-mode-title' face background for DB."
  (dolist (face '(ekg-notes-mode-title))
    (when (facep face)
      (set-face-background
       face
       (pcase (file-name-nondirectory db)
         ("anesth.db" "darkgreen")
         ("triples.db" "purple")
         (_ "red"))))))

;;;###autoload
(defun z/ekg-db-switch (&optional db)
  "Switch the active EKG database to DB."
  (interactive
   (list (completing-read
          "Switch to DB: "
          (directory-files user-emacs-directory t "\\.db$")
          (lambda (f) (not (string= ekg-db-file f))))))
  (let ((target-db (or db ekg-db-file)))
    (ekg-close)
    (setq ekg-db-file target-db)
    (z/ekg-db-face-update target-db)
    (ekg--refresh-notes-buffers)
    (message "Switched to database: %s"
             (file-name-nondirectory target-db))))

;;;###autoload
(defun z/ekg-move-to-db (&optional db)
  "Move the current note to DB, staying in the current database."
  (interactive
   (list (completing-read
          "db: "
          (directory-files user-emacs-directory t "\\.db$")
          (lambda (f) (not (string= ekg-db-file f))))))
  (let ((db0 ekg-db-file))
    (if (eq major-mode 'ekg-notes-mode)
        (let ((note (ekg-current-note-or-error)))
          (unwind-protect
              (progn
                (ekg-close)
                (setq ekg-db-file db)
                (let ((note-copy (copy-ekg-note note)))
                  (setf (ekg-note-id note-copy) nil)
                  (ekg-note-save note-copy)))
            (ekg-close)
            (setq ekg-db-file db0))
          (ekg-note-delete note)
          (ekg-notes-redisplay)
          (message "已将笔记移动到：%s" (file-name-nondirectory db)))
      (unwind-protect
          (progn
            (ekg-close)
            (setq ekg-db-file db)
            (setq-local ekg-note-id nil)
            (ekg--save-note-in-buffer))
        (ekg-close)
        (setq ekg-db-file db0))
      (ekg-note-delete ekg-note)
      (kill-buffer (current-buffer))
      (message "已将笔记移动到：%s" (file-name-nondirectory db))))
  (z/ekg-db-face-update db0)
  (ekg--refresh-notes-buffers))

;; =====================================================================
;;; Find and edit notes
;; =====================================================================

;;;###autoload
(defun z/ekg-edit-note ()
  "Find a note by title, falling back to tag selection."
  (interactive)
  (ekg-connect)
  (let* ((title-rows (triples-db-select ekg-db nil 'titled/title))
         (titles (mapcar (lambda (row) (nth 2 row)) title-rows))
         (title (completing-read "Choose TITLE (or C-j for TAGS): " titles)))
    (if (not (string-empty-p title))
        (ekg-edit (ekg-get-note-with-id
                   (caar (seq-filter (lambda (row) (equal title (nth 2 row))) title-rows))))
      (let* ((notes (ekg-get-notes-with-tag
                     (completing-read "Tag: " (ekg-tags) nil t)))
             (completion-pairs (mapcar
                                (lambda (note)
                                  (cons (ekg-display-note-text note 10)
                                        note)) notes))
             (chosen (completing-read "Note: " completion-pairs nil t)))
        (ekg-edit (ekg-get-note-with-id (cdr (assoc chosen completion-pairs))))))))

;;;###autoload
(defun z/ekg-edit-note-/ai-version-with-annotations ()
  "Find a note by title with metadata annotations, falling back to tags."
  (interactive)
  (ekg-connect)
  (let* ((title-rows (triples-db-select ekg-db nil 'titled/title))
         (title-alist (mapcar
                       (lambda (row)
                         (let* ((id (car row))
                                (title (nth 2 row))
                                (note (ekg-get-note-with-id id))
                                (tags (ekg-note-tags note))
                                (tags-str (if tags (format " [%s]" (string-join tags ", ")) ""))
                                (all-triples (triples-db-select ekg-db id))
                                (date-row (seq-find (lambda (tr)
                                                      (string-match-p "created\\|timestamp" (symbol-name (nth 1 tr))))
                                                    all-triples))
                                (date-str (if date-row (format " (%s)" (nth 2 date-row)) "")))
                           (cons title (list :id id :meta (format "%s%s" date-str tags-str)))))
                       title-rows))
         (completion-extra-properties
          `(:annotation-function
            ,(lambda (candidate)
               (let ((match (assoc candidate title-alist)))
                 (if match
                     (propertize (concat "   " (plist-get (cdr match) :meta))
                                 'face 'font-lock-comment-face)
                   "")))))
         (chosen-title (completing-read "Choose title (or C-j for tags): " title-alist))
         (target-id nil))
    (if (not (string-empty-p chosen-title))
        (setq target-id (plist-get (cdr (assoc chosen-title title-alist)) :id))
      (let* ((tag (completing-read "Tag: " (ekg-tags) nil t))
             (notes (when (not (string-empty-p tag)) (ekg-get-notes-with-tag tag)))
             (completion-pairs (mapcar (lambda (note)
                                         (cons (ekg-display-note-text note 10) note))
                                       notes))
             (chosen-note-text (when completion-pairs
                                 (completing-read "Note: " completion-pairs nil t)))
             (matched-note (cdr (assoc chosen-note-text completion-pairs))))
        (when matched-note
          (setq target-id (ekg-note-id matched-note)))))
    (if target-id
        (ekg-edit (ekg-get-note-with-id target-id))
      (message "No note selected."))))

;;;###autoload
(defun z/ekg-edit-note-latest-modified ()
  "Open the most recently modified note."
  (interactive)
  (ekg-connect)
  (ekg-edit
   (ekg-get-note-with-id
    (caar (sort (triples-with-predicate
                 ekg-db
                 'time-tracked/modified-time)
                (lambda (trip1 trip2) (> (nth 2 trip1)
                                         (nth 2 trip2))))))))

;;;###autoload
(defun z/ekg-edit-note-latest-created ()
  "Open the most recently created note."
  (interactive)
  (ekg-connect)
  (ekg-edit
   (ekg-get-note-with-id
    (caar (sort (triples-with-predicate
                 ekg-db
                 'time-tracked/creation-time)
                (lambda (trip1 trip2) (> (nth 2 trip1)
                                         (nth 2 trip2))))))))

;; =====================================================================
;;; Hierarchical tag navigation
;; =====================================================================

;;;###autoload
(defun z/ekg-show-notes-with-tag/template ()
  "Show notes tagged as template."
  (interactive)
  (condition-case err
      (progn
        (ekg-connect)
        (ekg-show-notes-with-any-tags '("template")))
    (error (user-error "EKG error: %s" (error-message-string err)))))

;;;###autoload
(defun z/ekg-show-notes-with-tag/tag-defun ()
  "Show notes tagged as tag-defun."
  (interactive)
  (condition-case err
      (progn
        (ekg-connect)
        (ekg-show-notes-with-any-tags '("tag-defun")))
    (error (user-error "EKG error: %s" (error-message-string err)))))

;;;###autoload
(defun z/ekg-tags ()
  "Return a flat list of all tags, expanding hierarchies.
Excludes special tags like trash and draft."
  (condition-case err
      (progn
        (ekg-connect)
        (flatten-list
         (mapcar #'ekg-tag-to-hierarchy
                 (triples-subjects-of-type ekg-db 'tag))))
    (error (user-error "获取标签列表失败: %s" (error-message-string err)))))

;;;###autoload
(defun z/ekg-tags-get-hierarchy-tags (substr)
  "Return all hierarchy tags matching SUBSTR as a prefix."
  (condition-case err
      (progn
        (ekg-connect)
        (let ((clean-substr (string-trim-right substr "/")))
          (seq-filter
           (lambda (tag)
             (or (string-equal clean-substr tag)
                 (string-prefix-p (concat clean-substr "/") tag)))
           (triples-subjects-of-type ekg-db 'tag))))
    (error (user-error "获取层级标签失败: %s" (error-message-string err)))))

;;;###autoload
(defun z/ekg-show-notes-with-any-hierarchy-tags (tags)
  "Show notes matching any of TAGS (hierarchy-aware, union)."
  (interactive (list (let ((crm-separator "[,，]"))
                       (completing-read-multiple "Hierarchy Tags: " (z/ekg-tags) nil t))))
  (condition-case err
      (progn
        (when (null tags)
          (user-error "未选择任何标签"))
        (ekg-connect)
        (let ((hrchy-tags
               (seq-uniq
                (flatten-list
                 (mapcar (lambda (tag) (z/ekg-tags-get-hierarchy-tags tag)) tags)))))
          (ekg-setup-notes-buffer
           (format "TAGS (hierarchy, any): %s" (ekg-tags-display tags))
           (lambda ()
             (sort (seq-uniq
                    (mapcan (lambda (tag) (ekg-get-notes-with-tag tag)) hrchy-tags))
                   #'ekg-sort-by-creation-time))
           hrchy-tags)))
    (error (user-error "层级标签筛选失败: %s" (error-message-string err)))))

;;;###autoload
(defun z/ekg-show-notes-with-all-hierarchy-tags (tags)
  "Show notes matching all of TAGS (hierarchy-aware, intersection)."
  (interactive (list (let ((crm-separator "[,，]"))
                       (completing-read-multiple "Hierarchy Tags: " (z/ekg-tags) nil t))))
  (condition-case err
      (progn
        (when (null tags)
          (user-error "未选择任何标签"))
        (ekg-connect)
        (let* ((ids-by-tag-list
                (mapcar
                 (lambda (tag)
                   (flatten-list
                    (mapcar (lambda (t) (plist-get (triples-get-type ekg-db t 'tag) :tagged))
                            (z/ekg-tags-get-hierarchy-tags tag))))
                 tags))
               (all-empty (seq-every-p #'null ids-by-tag-list)))
          (if all-empty
              (message "未找到匹配所有标签的笔记")
            (ekg-setup-notes-buffer
             (format "TAGS (hierarchy, all): %s" (ekg-tags-display tags))
             (lambda ()
               (sort (mapcar #'ekg-get-note-with-id
                             (seq-reduce #'seq-intersection
                                         (cdr ids-by-tag-list)
                                         (car ids-by-tag-list)))
                     #'ekg-sort-by-creation-time))
             nil))))
    (error (user-error "层级标签交集筛选失败: %s" (error-message-string err)))))

;;;###autoload
(defun z/ekg-show-notes-with-partial-tag (tag)
  "Show notes with any tag partially matching TAG."
  (interactive (list (completing-read "Tag: " (ekg-tags) nil nil)))
  (condition-case err
      (progn
        (when (string-empty-p tag)
          (user-error "标签不能为空"))
        (ekg-connect)
        (ekg-show-notes-with-any-tags (ekg-tags-including tag)))
    (error (user-error "标签模糊搜索失败: %s" (error-message-string err)))))

;;;###autoload
(defun z/ekg-show-notes-with-exact-match ()
  "Search all notes for a text match across titles, tags, and body.
Excludes trashed notes. Uses active region if present."
  (interactive)
  (condition-case err
      (progn
        (ekg-connect)
        (let* ((query-input
                (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (read-from-minibuffer "EKG EXACT Match: ")))
               (query-input (string-trim query-input))
               (trash-tag (or (bound-and-true-p ekg-trash-tag) "trash"))
               (raw-ids
                (flatten-list
                 (pcase triples-sqlite-interface
                   ('builtin
                    (sqlite-execute
                     ekg-db
                     "SELECT DISTINCT subject FROM triples
WHERE instr(object, ?) > 0
  AND predicate IN ('text/text', 'titled/title', 'tagged/tag', 'reffed/ref')
  AND subject NOT IN (
    SELECT DISTINCT subject FROM triples
    WHERE predicate = 'tagged/tag' AND object = ?
  )"
                     (list query-input trash-tag)))
                   ('emacsql
                    (emacsql ekg-db
                             [:select [distinct subject] :from triples
                                      :where (and (> (instr object $s1) 0)
                                                  (in predicate [text/text titled/title tagged/tag reffed/ref])
                                                  (not (in subject
                                                           [:select [distinct subject] :from triples
                                                                    :where (and (= predicate tagged/tag)
                                                                                (= object $s2))])))]
                             query-input trash-tag))
                   (_ (user-error "不支持的 triples 后端: %s" triples-sqlite-interface)))))
               (ids (mapcar (lambda (id)
                              (cond ((stringp id) (intern id))
                                    ((symbolp id) id)
                                    (t id)))
                            raw-ids)))
          (if (null ids)
              (message "未找到包含 \"%s\" 的笔记" query-input)
            (ekg-setup-notes-buffer
             (concat "Notes with Exact Match (No Trash): " query-input)
             (lambda ()
               (remove nil (mapcar #'ekg-get-note-with-id ids)))
             nil))))
    (error (user-error "全文搜索失败: %s" (error-message-string err)))))

;;;###autoload
(defun z/ekg-show-notes-within-date-range (&optional from-date to-date)
  "Find notes created between FROM-DATE and TO-DATE."
  (interactive)
  (let* ((from-date0 (or from-date (org-read-date)))
         (to-date0 (or to-date (org-read-date)))
         (reverse-order-p (string> from-date0 to-date0))
         (from-date (if reverse-order-p to-date0 from-date0))
         (to-date (if reverse-order-p from-date0 to-date0))
         (from-date-start (date-to-time (concat from-date " 00:00:00")))
         (from-time-start (time-to-seconds from-date-start))
         (to-date-end (date-to-time (concat to-date " 23:59:59")))
         (to-time-end (time-to-seconds to-date-end)))
    (ekg-connect)
    (ekg-setup-notes-buffer
     (format "Notes created from %s to %s" from-date to-date)
     (lambda ()
       (cl-loop for id in (mapcar
                           #'car
                           (sort (seq-filter (lambda (ct)
                                               (let ((ct (nth 2 ct)))
                                                 (and (> ct from-time-start)
                                                      (< ct to-time-end))))
                                             (triples-with-predicate
                                              ekg-db
                                              'time-tracked/creation-time))
                                 (lambda (trip1 trip2)
                                   (> (nth 2 trip1)
                                      (nth 2 trip2)))))
                for note = (ekg-get-note-with-id id)
                when (and note (ekg-note-tags note))
                collect note into selected
                finally return selected))
     nil)))

;; =====================================================================
;;; Note editing primitives
;; =====================================================================

(defun z/ekg-note--current-note ()
  "Return the note at point, or `ekg-note' in edit buffers."
  (if (eq major-mode 'ekg-notes-mode)
      (let* ((pos (point))
             (bol (line-beginning-position))
             (id (or (get-text-property pos :ekg-note-id)
                     (and (> pos bol) (get-text-property (1- pos) :ekg-note-id))
                     (let ((prev (previous-single-property-change pos :ekg-note-id nil bol)))
                       (and prev (get-text-property prev :ekg-note-id))))))
        (and id (ekg-get-note-with-id id)))
    ekg-note))

(defun z/ekg-note--refresh-interface (note)
  "Refresh UI for NOTE across buffers."
  (if (eq major-mode 'ekg-notes-mode)
      (let ((inhibit-read-only t))
        (ekg-save-note note)
        (ekg--refresh-notes-buffers))
    (let ((id (format "%s" (ekg-note-id note))))
      (dolist (b (buffer-list))
        (when (string-match-p id (buffer-name b))
          (with-current-buffer b
            (setq ekg-note note
                  header-line-format (ekg--header-line-format))
            (set-buffer-modified-p t)
            (force-mode-line-update)))))))

(defun z/ekg-note--prop-op (kw &optional val)
  "Edit property KW on the current note, optionally setting to VAL."
  (let* ((note (z/ekg-note--current-note))
         (raw (symbol-name kw))
         (name (capitalize (if (string-match "/\\([^/]+\\)\\'" raw)
                               (match-string 1 raw)
                             (substring raw 1)))))
    (when note
      (let* ((ps (ekg-note-properties note))
             (v (plist-get ps kw))
             (vs (cond ((null v) nil)
                       ((listp v) v)
                       ((vectorp v) (append v nil))
                       (t (list (format "%s" v)))))
             (new (if val
                      (list val)
                    (let ((crm-separator "[,，]"))
                      (seq-uniq
                       (completing-read-multiple
                        (format "编辑 %s：" name)
                        vs nil nil
                        (when vs (concat (mapconcat #'identity vs ", ") ", "))))))))
        (setf (ekg-note-properties note)
              (plist-put ps kw new))
        (z/ekg-note--refresh-interface note)
        (message "%s 更新完成（共 %d 项）" name (length new))))))

(defun z/ekg-note--tags-op (&optional val)
  "Edit tags on the current note, optionally setting to VAL."
  (let* ((note (z/ekg-note--current-note))
         (old (ekg-note-tags note))
         (new (if val (list val)
                (let ((crm-separator "[,，]"))
                  (seq-uniq
                   (completing-read-multiple
                    "编辑 Tags："
                    (z/ekg-tags) nil nil
                    (when old (concat (mapconcat #'identity old ", ") ", "))
                    'ekg-tag-history))))))
    (setf (ekg-note-tags note) new)
    (let ((inhibit-read-only t))
      (dolist (tag new)
        (run-hook-with-args 'ekg-note-add-tag-hook tag)
        (ekg-maybe-function-tag tag))
      (z/ekg-note--refresh-interface note))
    (message "Tags 更新完成（共 %d 项）" (length new))))

;;;###autoload
(defun z/ekg-note-update-at-point ()
  "Edit the property or text at point in ekg-notes-mode."
  (interactive nil ekg-notes-mode)
  (let* ((pos (point))
         (bol (line-beginning-position))
         (prop-key (or (get-text-property pos 'ekg-prop-key)
                       (and (> pos bol) (get-text-property (1- pos) 'ekg-prop-key))
                       (let ((prev (previous-single-property-change pos 'ekg-prop-key nil bol)))
                         (and prev (get-text-property prev 'ekg-prop-key)))))
         (text-prop (or (get-text-property pos 'ekg-note-text)
                        (and (> pos (point-min)) (get-text-property (1- pos) 'ekg-note-text))
                        (let ((prev (previous-single-property-change pos 'ekg-note-text)))
                          (and prev (get-text-property prev 'ekg-note-text))))))
    (cond
     (prop-key
      (if (eq prop-key 'tagged/tag)
          (z/ekg-note--tags-op)
        (z/ekg-note--prop-op prop-key)))
     (text-prop
      (z/ekg-note-update-text))
     (t
      (message "光标不在可编辑区域")))))

(with-eval-after-load 'ekg
  (define-key ekg-notes-mode-map (kbd "u") #'z/ekg-note-update-at-point))

(defun z/ekg-note-update-text ()
  "Update the body text of the note at point."
  (interactive nil ekg-notes-mode)
  (let* ((text-state (get-text-property (point) 'ekg-note-text))
         (note (ekg-current-note-or-error)))
    (cond
     ((eq text-state 'inline)
      (message "当前为 inline 预览，请按 o 打开笔记后修改"))
     ((and note (eq text-state t))
      (let* ((old-text (ekg-note-text note))
             (new-text (string-trim
                        (read-from-minibuffer "编辑 Text：" old-text))))
        (unless (string= old-text new-text)
          (setf (ekg-note-text note) new-text)
          (z/ekg-note--refresh-interface note)
          (message "Text 更新完成"))))
     (t
      (message "无法获取当前笔记正文")))))

;; =====================================================================
;;; Auto-generated display advice
;; =====================================================================

(defun z/ekg-note-auto-derive-from-triples ()
  "Auto-generate update commands and display advice for known properties."
  (interactive)
  (dolist (sub  ;; (triples-subjects-of-type ekg-db 'ekg-property)
           (list 'titled/title 'reffed/ref 'tagged/tag))
    (let* ((raw (symbol-name sub))
           (tag-p (string= raw "tagged/tag"))
           (name-suf (if tag-p "tags" (if (string-match "/\\([^/]+\\)$" raw) (match-string 1 raw) raw)))
           (type-suf (if (string-match "^\\([^/]+\\)/" raw) (match-string 1 raw) raw))
           (update-fn (intern (format "z/ekg-note-update-%s" name-suf)))
           (display-func (intern (format "ekg-display-note-%s" type-suf)))
           (kw (intern (concat ":" raw))))
      (unless (fboundp update-fn)
        (fset update-fn `(lambda (&optional v)
                           (interactive nil ekg-capture-mode ekg-edit-mode ekg-notes-mode)
                           (,(if tag-p 'z/ekg-note--tags-op 'z/ekg-note--prop-op)
                            ,@(unless tag-p (list kw))
                            v)))
        (put update-fn 'function-documentation
             (format "专属 %s 属性批量更新命令，一次性增删改。" (capitalize name-suf))))
      (unless tag-p
        (when (fboundp display-func)
          (advice-add display-func :around
                      `(lambda (orig-fn note &rest args)
                         (let ((result (apply orig-fn note args)))
                           (propertize result
                                       'keymap (let ((m (make-sparse-keymap)))
                                                 (define-key m (kbd "u") #'z/ekg-note-update-at-point)
                                                 m)
                                       'ekg-prop-key ',kw
                                       :ekg-note-id (ekg-note-id note)
                                       'help-echo ,(format "按 u 编辑 %s" (capitalize name-suf)))))
                      `((name . ,(intern (format "unified-%s-line" type-suf))))))))))

(define-advice ekg-display-note-tagged (:override (note &optional numwords &rest fmt-args) unified-tags-line)
  "Render tags line with clickable time, tags, and u-to-edit."
  (let ((line-map (make-sparse-keymap)))
    (define-key line-map (kbd "u") #'z/ekg-note-update-at-point)
    (ekg-display--format
     (propertize
      (concat
       (let* ((creation (ekg-note-creation-time note))
              (time-str (format-time-string "%Y-%m-%d %H:%M" creation))
              (time-tag-map (make-sparse-keymap)))
         (set-keymap-parent time-tag-map line-map)
         (define-key time-tag-map [mouse-1]
                     (lambda (event)
                       (interactive "e")
                       (let ((from-date (format-time-string "%Y-%m-%d" (time-add creation (days-to-time -3))))
                             (to-date (format-time-string "%Y-%m-%d" (time-add creation (days-to-time 3)))))
                         (z/ekg-show-notes-within-date-range from-date to-date))))
         (propertize (format "⌚ %s" time-str)
                     'face 'ekg-tag
                     'mouse-face 'highlight
                     'keymap time-tag-map
                     'help-echo "点击查看该笔记前后3天的笔记"
                     :ekg-note-id (ekg-note-id note)))
       (propertize " --> " 'keymap line-map :ekg-note-id (ekg-note-id note))
       (mapconcat
        (lambda (tag)
          (let ((tag-map (make-sparse-keymap)))
            (set-keymap-parent tag-map line-map)
            (define-key tag-map (kbd "RET")
                        (lambda ()
                          (interactive)
                          (xref-push-marker-stack)
                          (ekg-show-notes-with-tag tag)))
            (define-key tag-map [mouse-1]
                        (lambda (event)
                          (interactive "e")
                          (posn-set-point (event-end event))
                          (xref-push-marker-stack)
                          (ekg-show-notes-with-tag tag)))
            (propertize tag
                        'face 'ekg-tag
                        'mouse-face 'highlight
                        'keymap tag-map
                        'help-echo "RET/左键: 查看此标签笔记 | u: 批量编辑标签"
                        'ekg-tag tag
                        :ekg-note-id (ekg-note-id note))))
        (ekg-note-tags note)
        (propertize " " 'keymap line-map :ekg-note-id (ekg-note-id note)))
       (propertize "\n" 'keymap line-map :ekg-note-id (ekg-note-id note)))
      'ekg-prop-key 'tagged/tag
      :ekg-note-id (ekg-note-id note))
     numwords fmt-args)))

(define-advice ekg-display-note-text (:around (orig-fn note &rest args) add-text-prop)
  "Add `ekg-note-text' property to body text output."
  (let* ((result (apply orig-fn note args))
         (has-inline (ekg-note-inlines note)))
    (propertize result 'ekg-note-text (if has-inline 'inline t))))

(with-eval-after-load 'ekg-ref
  (z/ekg-note-auto-derive-from-triples))

;; =====================================================================
;;; Trash undo / untrash
;; =====================================================================

(defvar z/ekg-last-trashed-note-id nil
  "ID of the most recently trashed note, for undo.")

(define-advice ekg-note-trash (:after (note) z/record-trash)
  "Record the trashed note ID for later untrash."
  (setq z/ekg-last-trashed-note-id (ekg-note-id note)))

;;;###autoload
(defun z/ekg-note-trash-untrash ()
  "Remove the trash tag from the note at point."
  (interactive)
  (unless (derived-mode-p 'ekg-notes-mode)
    (error "Not in ekg-notes-mode"))
  (let* ((note (ekg-current-note-or-error))
         (tags (ekg-note-tags note)))
    (if (member ekg-trash-tag tags)
        (progn
          (setf (ekg-note-tags note) (delete ekg-trash-tag tags))
          (ekg-save-note note)
          (ekg-backup)
          (ekg-notes-refresh)
          (message "Note untrashed"))
      (message "Note is not trashed"))))

;;;###autoload
(defun z/ekg-note-trash-undo ()
  "Undo the most recent trash operation.
Finds the last trashed note by recorded ID, even if it is no longer
visible in the current notes buffer."
  (interactive)
  (unless (derived-mode-p 'ekg-notes-mode)
    (error "Not in ekg-notes-mode"))
  (if z/ekg-last-trashed-note-id
      (let ((note (ekg-get-note-with-id z/ekg-last-trashed-note-id)))
        (if note
            (progn
              (setf (ekg-note-tags note)
                    (delete ekg-trash-tag (ekg-note-tags note)))
              (ekg-save-note note)
              (ekg-backup)
              (setq z/ekg-last-trashed-note-id nil)
              (ekg-notes-refresh)
              (message "Undid trash: %s" (ekg-note-id note)))
          (message "Last trashed note not found")))
    (message "No recent trash to undo")))

(with-eval-after-load 'ekg
  (define-key ekg-notes-mode-map "z" #'z/ekg-note-trash-undo)
  (define-key ekg-notes-mode-map "Z" #'z/ekg-note-trash-untrash))

(provide 'ekg-extras)
;;; ekg-extras.el ends here
