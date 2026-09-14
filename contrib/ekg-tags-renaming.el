;;; ekg-tags-renaming.el --- description -*- lexical-binding: t -*-
;; =====================================================================
;; Tag Rename Preview (tabulated-list-mode + marks + dry-run)
;; =====================================================================

(require 'cl-lib)
(require 'ekg-extras)

;; --- buffer-local 状态 ---
(defvar z/ekg-tag-rename--dry-run nil
  "Buffer-local flag for dry-run mode in tag rename preview.")
(make-variable-buffer-local 'z/ekg-tag-rename--dry-run)

(defvar z/ekg-tag-rename--marked nil
  "Buffer-local hash set of marked from-tags.")
(make-variable-buffer-local 'z/ekg-tag-rename--marked)

(defvar z/ekg-tag-rename--from-subtags nil)
(defvar z/ekg-tag-rename--to-subtags nil)
(defvar z/ekg-tag-rename--counts nil)
(make-variable-buffer-local 'z/ekg-tag-rename--from-subtags)
(make-variable-buffer-local 'z/ekg-tag-rename--to-subtags)
(make-variable-buffer-local 'z/ekg-tag-rename--counts)

(defvar z/ekg-tag-rename--from-w nil)
(defvar z/ekg-tag-rename--to-w nil)
(defvar z/ekg-tag-rename--count-w nil)
(defvar z/ekg-tag-rename--total-notes nil)
(make-variable-buffer-local 'z/ekg-tag-rename--from-w)
(make-variable-buffer-local 'z/ekg-tag-rename--to-w)
(make-variable-buffer-local 'z/ekg-tag-rename--count-w)
(make-variable-buffer-local 'z/ekg-tag-rename--total-notes)

;; --- 内部工具 ---
(defun z/ekg-tag-rename--refresh-entries ()
  "根据标记状态重建 tabulated-list-entries 并完整重绘（含汇总帮助行）。"
  (setq tabulated-list-entries
        (seq-mapn (lambda (from to count)
                    (let ((marked (gethash from z/ekg-tag-rename--marked)))
                      (list from
                            (vector
                             (propertize (concat (if marked "[x]" "[ ]") " " from)
                                         'face (if marked
                                                   (list 'error :weight 'bold)
                                                 (list 'shadow)))
                             (propertize to 'face (list 'error :weight 'bold :slant 'italic))
                             (propertize (number-to-string count)
                                         'face 'font-lock-comment-face)))))
                  z/ekg-tag-rename--from-subtags
                  z/ekg-tag-rename--to-subtags
                  z/ekg-tag-rename--counts))
  (let ((inhibit-read-only t))
    (tabulated-list-print)
    ;; 末尾追加：分隔线 + 汇总 + 帮助行
    (goto-char (point-max))
    (insert "\n")
    (insert (propertize (make-string (+ z/ekg-tag-rename--from-w 3 z/ekg-tag-rename--to-w 3 z/ekg-tag-rename--count-w) ?-)
                        'face 'font-lock-comment-face))
    (insert "\n")
    (insert (format "Total: %d tags, %d notes.  "
                    (length z/ekg-tag-rename--from-subtags)
                    z/ekg-tag-rename--total-notes))
    (insert (propertize "[m] mark  [u] unmark  [U] unmark all  [d] dry-run  [y] confirm  [q] cancel\n"
                        'face (list :weight 'bold)))
    ;; 光标归位到第一个数据行（跳过表头）
    (goto-char (point-min))
    (forward-line 1)))


(defun z/ekg-tag-rename--current-from-tag ()
  "获取当前行的 from-tag 字符串，无则返回 nil。"
  (let ((id (or (get-text-property (point) 'tabulated-list-id)
                (save-excursion
                  (back-to-indentation)
                  (get-text-property (point) 'tabulated-list-id)))))
    (when id
      (if (stringp id) id (format "%s" id)))))

(defun z/ekg-tag-rename--toggle-mark ()
  "切换当前行的标记状态（[x] = 选中）。"
  (interactive)
  (let ((from (z/ekg-tag-rename--current-from-tag)))
    (unless from (user-error "No entry at point"))
    (if (gethash from z/ekg-tag-rename--marked)
        (remhash from z/ekg-tag-rename--marked)
      (puthash from t z/ekg-tag-rename--marked))
    (z/ekg-tag-rename--refresh-entries)))

(defun z/ekg-tag-rename--toggle-dry-run ()
  "切换 dry-run 模式。"
  (interactive)
  (setq z/ekg-tag-rename--dry-run (not z/ekg-tag-rename--dry-run))
  (message "Dry-run: %s — 按 y 查看将执行的 SQL，不实际修改数据库"
           (if z/ekg-tag-rename--dry-run "ON" "OFF")))

(defun z/ekg-tag-rename--get-marked ()
  "返回所有标记行的 (from . to) 对列表。"
  (let (marked)
    (maphash (lambda (from _)
               (let ((idx (cl-position from z/ekg-tag-rename--from-subtags :test #'string=)))
                 (when idx
                   (push (cons from (nth idx z/ekg-tag-rename--to-subtags)) marked))))
             z/ekg-tag-rename--marked)
    (nreverse marked)))

(defun z/ekg-tag-rename--show-dry-run (from-tag to-tag marked)
  "弹出 dry-run SQL 预览 buffer（只读，不修改 DB）。"
  (let ((buf (get-buffer-create "*ekg-tag-rename-dry-run*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Dry-Run Preview — 不会修改数据库\n"
                            'face (list :weight 'bold :underline t)))
        (insert (format "前缀: %s -> %s\n"
                        (propertize from-tag 'face 'error)
                        (propertize to-tag 'face (list 'error :slant 'italic))))
        (insert (format "已选 %d 个标签\n\n" (length marked)))
        (insert (propertize (make-string 60 ?-) 'face 'font-lock-comment-face))
        (insert "\n\n")
        (dolist (pair marked)
          (let ((from (car pair))
                (to (cdr pair)))
            (insert (propertize (format "-- %s -> %s\n" from to)
                                'face 'font-lock-comment-face))
            (insert (format "UPDATE triples SET object = '%s'\n  WHERE object = '%s' AND predicate = 'tagged/tag';\n"
                            to from))
            (insert (format "(triples-remove-type ekg-db '%s 'tag)\n" from))
            (insert (format "(triples-set-type ekg-db '%s 'tag)\n" to))
            (insert "(mapc #'ekg-fix-renamed-dup-tags old-tag-ids)\n\n"))))
      (special-mode)
      (goto-char (point-min)))
    (display-buffer buf)))

;; --- buffer 风格预览（≥5 个 或 C-u 强制）---
(defun z/ekg-tag-rename--preview (from-tag to-tag from-subtags to-subtags counts)
  "弹出重命名预览 buffer（tabulated-list-mode）。
支持：点击列头排序、m 标记/取消、d 切换 dry-run。
返回选中的 (from . to) 列表，或 nil。"
  (let* ((buf (get-buffer-create "*ekg-tag-rename*"))
         (from-w (+ 4 (max (length "FROM-TAG")
                           (seq-max (mapcar #'length from-subtags)))))
         (to-w (max (length "TO-TAG")
                    (seq-max (mapcar #'length to-subtags))))
         (count-w (max (length "COUNT")
                       (length (number-to-string (or (seq-max counts) 0)))))
         (total-notes (apply #'+ counts))
         result)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (tabulated-list-mode)
        (setq tabulated-list-format
              (vector (list "FROM-TAG" from-w nil)
                      (list "TO-TAG" to-w nil)
                      (list "COUNT" count-w nil)))
        ;; 初始化状态（含列宽、总数，供 refresh-entries 重绘时使用）
        (setq z/ekg-tag-rename--from-subtags from-subtags)
        (setq z/ekg-tag-rename--to-subtags to-subtags)
        (setq z/ekg-tag-rename--counts counts)
        (setq z/ekg-tag-rename--from-w from-w)
        (setq z/ekg-tag-rename--to-w to-w)
        (setq z/ekg-tag-rename--count-w count-w)
        (setq z/ekg-tag-rename--total-notes total-notes)
        (setq z/ekg-tag-rename--marked (make-hash-table :test 'equal))
        (setq z/ekg-tag-rename--dry-run nil)
        ;; 默认全部标记
        (dolist (from from-subtags)
          (puthash from t z/ekg-tag-rename--marked))
        (tabulated-list-init-header)
        ;; refresh-entries 内部已包含：打印表格 + 追加汇总帮助行 + 光标归位
        (z/ekg-tag-rename--refresh-entries))
      (local-set-key (kbd "m") 'z/ekg-tag-rename--toggle-mark)
      (local-set-key (kbd "u") (lambda () (interactive)
                                 (let ((from (z/ekg-tag-rename--current-from-tag)))
                                   (when from
                                     (remhash from z/ekg-tag-rename--marked)
                                     (z/ekg-tag-rename--refresh-entries)))))
      (local-set-key (kbd "U") (lambda () (interactive)
                                 (clrhash z/ekg-tag-rename--marked)
                                 (z/ekg-tag-rename--refresh-entries)))
      (local-set-key (kbd "d") 'z/ekg-tag-rename--toggle-dry-run)
      (local-set-key (kbd "y") (lambda () (interactive) (setq result 'confirm) (exit-recursive-edit)))
      (local-set-key (kbd "q") (lambda () (interactive) (setq result 'cancel) (exit-recursive-edit)))
      (local-set-key (kbd "n") (lambda () (interactive) (setq result 'cancel) (exit-recursive-edit))))
    (pop-to-buffer buf)
    (recursive-edit)
    ;; kill-buffer 之前先保存 buffer-local 状态，否则变量会丢失
    (let* ((dry-run (buffer-local-value 'z/ekg-tag-rename--dry-run buf))
           (marked (when (and (eq result 'confirm) (buffer-live-p buf))
                     (with-current-buffer buf (z/ekg-tag-rename--get-marked)))))
      (when (buffer-live-p buf) (kill-buffer buf))
      (cond
       ((not (eq result 'confirm)) nil)
       ((null marked) (message "没有选中任何标签") nil)
       (dry-run
        (z/ekg-tag-rename--show-dry-run from-tag to-tag marked) nil)
       (t marked)))))


;; --- diff 风格轻量预览（<5 个）---
(defun z/ekg-tag-rename--preview-diff (from-tag to-tag from-subtags to-subtags counts)
  "轻量预览（子标签 < 5 时使用）。确认返回全部 (from . to) 列表，取消返回 nil。"
  (let* ((from-w (1+ (max (length "FROM")
                          (seq-max (mapcar #'length from-subtags)))))
         (to-w (max (length "TO")
                    (seq-max (mapcar #'length to-subtags))))
         (count-w (max (length "COUNT")
                       (length (number-to-string (or (seq-max counts) 0)))))
         (total-notes (apply #'+ counts))
         (confirm (y-or-n-p
                   (concat "Are you sure to make the following changes?\n\n"
                           (propertize
                            (concat (string-pad " FROM" from-w)
                                    "    "
                                    (string-pad "TO" to-w)
                                    "  "
                                    (concat (make-string (- count-w (length "COUNT")) ?\s)
                                            "COUNT")
                                    "\n")
                            'face (list :weight 'bold :overline t :underline t))
                           (mapconcat #'identity
                                      (seq-mapn
                                       (lambda (from to count)
                                         (concat
                                          (propertize (string-pad (concat " " from) from-w)
                                                      'face (list 'error :weight 'bold))
                                          " -> "
                                          (propertize (string-pad to to-w)
                                                      'face (list 'error :weight 'bold :slant 'italic))
                                          "  "
                                          (propertize (format (format "%%%dd" count-w) count)
                                                      'face (list 'font-lock-comment-face :weight 'bold))))
                                       from-subtags to-subtags counts)
                                      "\n")
                           "\n"
                           (propertize (make-string (+ from-w 4 to-w 2 count-w 1) ?\s)
                                       'face (list :overline t))
                           "\n"
                           (format "Total: %d tags, %d notes affected."
                                   (length from-subtags) total-notes)))))
    ;; 清除长提示在 minibuffer/echo area 的残留
    (message nil)
    (when confirm
      (seq-mapn #'cons from-subtags to-subtags))))

;; --- 路由选择 ---
(defun z/ekg-tag-rename--confirm (from-tag to-tag from-subtags to-subtags counts &optional force-buffer)
  "根据子标签数量自动选择预览方式，返回选中的 (from . to) 列表，或 nil。
FORCE-BUFFER 非 nil 时总是使用 buffer 模式（支持标记和 dry-run）。"
  (if (or force-buffer (>= (length from-subtags) 5))
      (z/ekg-tag-rename--preview from-tag to-tag from-subtags to-subtags counts)
    (z/ekg-tag-rename--preview-diff from-tag to-tag from-subtags to-subtags counts)))

;; --- Buffer更新 ---
(defun z/ekg--refresh-edit-buffers (from-subtags to-subtags)
  "根据重命名映射 FROM-SUBTAGS → TO-SUBTAGS，更新所有打开的 EKG edit/capture buffer。
通过 buffer 名定位（*EKG Edit: ID* / *EKG Capture (note ID)*），
因为这些 buffer 的 major-mode 是 org-mode，不能靠 major-mode 判断。"
  (let ((mapping (seq-mapn #'cons from-subtags to-subtags)))
    (dolist (b (buffer-list))
      (let ((name (buffer-name b)))
        (when (or (string-match-p "\\`\\*EKG Edit: " name)
                  (string-match-p "\\`\\*EKG Capture (note " name))
          (with-current-buffer b
            (when (and (boundp 'ekg-note) ekg-note)
              (let* ((old-tags (ekg-note-tags ekg-note))
                     (new-tags (seq-uniq
                                (mapcar (lambda (tag)
                                          (let ((pair (assoc tag mapping)))
                                            (if pair (cdr pair) tag)))
                                        old-tags))))
                (unless (equal old-tags new-tags)
                  (let ((new-note (copy-ekg-note ekg-note)))
                    (setf (ekg-note-tags new-note) new-tags)
                    (setq ekg-note new-note))
                  (setq header-line-format (ekg--header-line-format))
                  (force-mode-line-update)
                  (message "更新 %s: %s -> %s" name old-tags new-tags))))))))))

;; --- 主命令 ---
;;;###autoload
(defun z/ekg-global-rename-tag-enhanced (&optional from-tag to-tag force-buffer)
  "Rename FROM-TAG to TO-TAG.
This can be done whether TO-TAG already exists or not. This
renames all instances of the tag globally, and all notes with
FROM-TAG will use TO-TAG.

With \\[universal-argument] prefix (C-u), always use the full
buffer preview (supports marking individual tags and dry-run)."
  (interactive (list nil nil current-prefix-arg))
  (ekg-connect)
  (let* ((hrchy-tags (z/ekg-tags))
         (from-tag (or from-tag (completing-read "From tag: " hrchy-tags nil t)))
         (from-subtags (z/ekg-tags-get-hierarchy-tags from-tag)))
    (unless from-subtags
      (user-error "标签 '%s' 不存在或未绑定任何笔记" from-tag))
    (let* ((to-tag (ekg--normalize-tag
                    (or to-tag (completing-read
                                (format "from tag \"%s\" -> To tag: " from-tag)
                                hrchy-tags nil nil from-tag))))
           (to-subtags (seq-map (lambda (from-subtag)
                                  (replace-regexp-in-string
                                   (rx (seq bos (literal from-tag))) to-tag
                                   from-subtag))
                                from-subtags))
           (counts (mapcar (lambda (from)
                             (length (plist-get (triples-get-type ekg-db from 'tag) :tagged)))
                           from-subtags)))
      (let ((selected (z/ekg-tag-rename--confirm from-tag to-tag from-subtags to-subtags counts force-buffer)))
        (when selected
          (ekg-backup t)
          (dolist (pair selected)
            (let ((from-subtag (car pair))
                  (to-subtag (cdr pair)))
              (triples-with-transaction
                ekg-db
                (let ((old-tag-ids (plist-get (triples-get-type ekg-db from-subtag 'tag) :tagged)))
                  (pcase triples-sqlite-interface
                    ('builtin (sqlite-execute
                               ekg-db
                               "UPDATE triples SET object = ? WHERE object = ? AND predicate = 'tagged/tag'"
                               (list (triples-standardize-val to-subtag) (triples-standardize-val from-subtag))))
                    ('emacsql (emacsql ekg-db [:update triples :set (= object $s1) :where (= object $s2) :and (= predicate 'tagged/tag)]
                                       to-subtag from-subtag)))
                  (triples-remove-type ekg-db from-subtag 'tag)
                  (triples-set-type ekg-db to-subtag 'tag)
                  (mapc #'ekg-fix-renamed-dup-tags old-tag-ids)))))
          (ekg--refresh-notes-buffers)
          (z/ekg--refresh-edit-buffers from-subtags to-subtags)
          (message "成功将 %d 个标签前缀从 '%s' 重命名为 '%s'"
                   (length selected) from-tag to-tag))))))

;;;###autoload
(defun z/ekg-global-rename-tag (&optional from-tag to-tag)
  "Rename FROM-TAG to TO-TAG.
This can be done whether TO-TAG already exists or not. This
renames all instances of the tag globally, and all notes with
FROM-TAG will use TO-TAG."
  (interactive)
  (ekg-connect)
  (let* ((hrchy-tags (z/ekg-tags))
         (from-tag (or from-tag (completing-read "From tag: " hrchy-tags nil t)))
         (from-subtags (z/ekg-tags-get-hierarchy-tags from-tag)))

    ;; 优化 1：防御机制。如果输入的标签没有任何子标签或其自身不存在，优雅报错退出
    (unless from-subtags
      (user-error "标签 '%s' 不存在或未绑定任何笔记" from-tag))

    (let* ((from-w (1+ (max (length "FROM")
                            (seq-max (mapcar #'length from-subtags)))))
           (to-tag (ekg--normalize-tag
                    (or to-tag (completing-read
                                (format "from tag \"%s\" -> To tag: " from-tag)
                                hrchy-tags nil nil from-tag))))
           (to-subtags (seq-map (lambda (from-subtag)
                                  (replace-regexp-in-string
                                   (rx (seq bos (literal from-tag))) to-tag
                                   from-subtag))
                                from-subtags))
           (to-w (max (length "TO")
                      (seq-max (mapcar #'length to-subtags))))
           (counts (mapcar (lambda (from)
                             (length (plist-get (triples-get-type ekg-db from 'tag) :tagged)))
                           from-subtags))
           (count-w (max (length "COUNT")
                         (length (number-to-string (or (seq-max counts) 0)))))
           (bottom-line (make-string (+ from-w 4 to-w 2 count-w 1) ?\s)))
      ;; preview for confirmation
      (when (y-or-n-p
             (concat "Are you sure to make the following changes?\n\n"
                     (propertize
                      (concat (string-pad " FROM" from-w)
                              "    "
                              (string-pad "TO" to-w)
                              "  "
                              (concat (make-string (- count-w (length "COUNT")) ?\s)
                                      "COUNT")
                              "\n")
                      'face (list :weight 'bold :overline t :underline t))
                     (mapconcat #'identity
                                (seq-mapn
                                 (lambda (from to count)
                                   (concat
                                    (propertize (string-pad (concat " " from) from-w)
                                                'face (list 'error :weight 'bold))
                                    " -> "
                                    (propertize (string-pad to to-w)
                                                'face (list 'match :weight 'bold :slant 'italic))
                                    "  "
                                    (propertize (format (format "%%%dd" count-w) count)
                                                'face (list 'font-lock-comment-face :weight 'bold))))
                                 from-subtags to-subtags counts)
                                "\n")
                     "\n"
                     (propertize bottom-line 'face (list :overline t))
                     "\n"))

        ;; CRITICAL ADDITION: Backup ekg forcibly the split-second user confirms changes
        (ekg-backup t)

        (dolist (from-subtag from-subtags)
          (let* ((to-subtag (replace-regexp-in-string (rx (seq bos (literal from-tag))) to-tag from-subtag)))
            (triples-with-transaction
              ekg-db
              (let ((old-tag-ids (plist-get (triples-get-type ekg-db from-subtag 'tag) :tagged)))
                (pcase triples-sqlite-interface
                  ('builtin (sqlite-execute
                             ekg-db
                             "UPDATE triples SET object = ? WHERE object = ? AND predicate = 'tagged/tag'"
                             (list (triples-standardize-val to-subtag) (triples-standardize-val from-subtag))))
                  ('emacsql (emacsql ekg-db [:update triples :set (= object $s1) :where (= object $s2) :and (= predicate 'tagged/tag)]
                                     to-subtag from-subtag)))
                (triples-remove-type ekg-db from-subtag 'tag)
                (triples-set-type ekg-db to-subtag 'tag)
                (mapc #'ekg-fix-renamed-dup-tags old-tag-ids)))))

        ;; 优化 2：UI 实时同步。在数据库底层修改完成后，强制刷新所有打开的 EKG 看板视图
        (ekg--refresh-notes-buffers)
        (z/ekg--refresh-edit-buffers from-subtags to-subtags)
        (message "成功将标签前缀从 '%s' 重命名为 '%s'" from-tag to-tag)))))

(provide 'ekg-tags-renaming)
;;; ekg-tags-renaming.el ends here
