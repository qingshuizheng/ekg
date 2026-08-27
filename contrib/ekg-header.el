;;; ekg-header.el --- Revamp header line -*- lexical-binding: t -*-

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

(require 'svg)
(require 'cl-lib)

;; ==================== 1. 用户自定义微调参数 ====================
(defvar ekg-svg-header-theme 'fw-pitaya
  "选择 ekg 多行 SVG header 的配色主题。默认直接启动：阜外手术室火龙果色 ('fw-pitaya)。")

(defvar ekg-svg-header-use-padding nil
  "是否为多行 SVG header 开启左侧和上下的呼吸内边距。")

(defvar ekg-svg-header-font-size 13
  "ekg 多行 SVG header 的基准字体大小（像素值）。")

(defvar ekg-svg-header-line-spacing 6
  "ekg 多行每一行之间的额外上下间距（像素值）。")

(defvar ekg-svg-header-order-weights '(tags functional-tags ref title other)
  "定义 header 各行（包含功能标签行）的显示顺序。")

;; ==================== 2. 中央色彩配置字典 (çTags 与 ƒTags 全面加深加亮版) ====================
(defconst ekg-svg-header-theme-palette
  '((fw-pitaya      . ((bg . "#A91B60") (tags . "#FFC72C") (func-tags . "#A8FFDE") (title . "#FFFFFF") (ref . "#E2C2D4") (other . "#FFFFFF")))
    (fuwai          . ((bg . "#9E1B26") (tags . "#E6A100") (func-tags . "#FFE082") (title . "#FFFFFF") (ref . "#E5C5C8") (other . "#FFFFFF")))
    (lakers         . ((bg . "#552583") (tags . "#FDB927") (func-tags . "#00FFCC") (title . "#FDB927") (ref . "#FFFFFF") (other . "#EAEAEA")))
    (pumch          . ((bg . "#0D5245") (tags . "#FFD700") (func-tags . "#E0F2F1") (title . "#F5F7F6") (ref . "#B2C7C4") (other . "#F5F7F6")))
    (pumch-green    . ((bg . "#1B7A60") (tags . "#FFFFFF") (func-tags . "#B9F6CA") (title . "#FFFFFF") (ref . "#A3D1C4") (other . "#FFFFFF")))
    (pumch-blue     . ((bg . "#2A7EB2") (tags . "#FFAA00") (func-tags . "#E0F7FA") (title . "#FFFFFF") (ref . "#BCE0F5") (other . "#FFFFFF")))
    (cyber-cyan     . ((bg . "#004D5A") (tags . "#00FFC4") (func-tags . "#00FFFF") (title . "#FDFEFF") (ref . "#90A4AE") (other . "#FDFEFF")))
    (cyber-lavender . ((bg . "#7B68EE") (tags . "#FF69B4") (func-tags . "#FFFF00") (title . "#FFFFFF") (ref . "#D1C4E9") (other . "#FFFFFF")))
    (default        . ((bg . "#2B2D42") (tags . "#EDF2F4") (func-tags . "#B0C4DE") (title . "#EDF2F4") (ref . "#8D99AE") (other . "#EDF2F4"))))
  "中央色彩分配表。已全面将 func-tags（ƒTags 所在行）的色彩拉高了亮度与对比度，确保第二行系统状态清晰可见。")

;; ==================== 3. 色彩安全检索核心 ====================
(defun ekg-svg-header--get-color (role)
  "根据当前激活的主题，安全抓取指定角色 ROLE 的 Hex 颜色。"
  (let* ((theme-config (or (alist-get ekg-svg-header-theme ekg-svg-header-theme-palette)
                           (alist-get 'default ekg-svg-header-theme-palette)))
         (color-hex (alist-get role theme-config)))
    (or color-hex "white")))

;; ==================== 4. 动态全屏版 SVG 渲染引擎 (终极修复：超长文本导致空白顶栏 Bug) ====================
(defun ekg-svg-dynamic-multi-line-header (lines face-or-colors target-width)
  "根据 LINES 动态生成多行 header 的 SVG 对象。
通过给内容的 tspan 标签注入 dx='0.5em' 相对横向位移，强行在冒号后撑开完美空格。
【核心修正】固定画布宽度与窗口一致，防止因超长标题导致 SVG 像素超出缓冲区上限而不渲染。"
  (let* ((font-size ekg-svg-header-font-size)
         (line-height (+ font-size ekg-svg-header-line-spacing))
         (total-lines (length lines))
         (bg-color (ekg-svg-header--get-color 'bg))
         ;; 1. 【核心修复】画布宽度紧密贴合传入的窗口全宽，不盲目作恶性加法，彻底消灭渲染死机空白
         (svg-width target-width)
         (svg-height (ceiling (if ekg-svg-header-use-padding
                                  (+ (* total-lines line-height) 8)
                                (* total-lines line-height))))
         (font-family (face-attribute 'default :family))
         (svg (svg-create svg-width svg-height))
         (current-y (if ekg-svg-header-use-padding (+ font-size 6) font-size))
         (padding-left (if ekg-svg-header-use-padding 8 0))
         ;; 动态估算当前窗口能容纳的最大字符长度，用于长文本安全截断
         (max-allowed-chars (ceiling (/ target-width (* font-size 0.62) 1.15)))
         (idx 0))

    ;; 绘制拉满当前全屏像素宽度的硬背景
    (svg-rectangle svg 0 0 svg-width svg-height :fill bg-color :rx 0 :ry 0)

    (dolist (line lines)
      (let* ((item (or (nth idx face-or-colors) 'header-line))
             (color (if (stringp item) item (face-attribute item :foreground nil 'default)))
             ;; 2. 【安全防护】如果某一行（如超长标题）太长，强制进行宽度限制并优雅打包为 ... 截断
             (safe-line (if (> (length line) max-allowed-chars)
                            (truncate-string-to-width line max-allowed-chars 0 nil "...")
                          line)))

        ;; 正则拆分前缀（包含冒号）与具体内容
        (if (string-match "\\`\\([^:]+:\\)\\(.*\\)\\'" safe-line)
            (let ((prefix (match-string 1 safe-line))
                  (content (string-trim (match-string 2 safe-line))))
              (svg--append
               svg
               `(text ((x . ,(number-to-string padding-left))
                       (y . ,(number-to-string current-y))
                       (font-family . ,font-family)
                       (font-size . ,(number-to-string font-size))
                       (fill . ,color))
                      ;; 1. 加粗的前缀（保留你的 ç 和 ƒ）
                      (tspan ((font-weight . "bold")) ,prefix)
                      ;; 2. 内容部分赋予 dx 属性，强制推开横向位移
                      (tspan ((font-weight . "normal") (dx . "0.5em")) ,content))))
          ;; 兜底渲染
          (svg-text svg safe-line
                    :font-family font-family
                    :font-size font-size
                    :fill (if (or (eq color 'unspecified) (null color)) "white" color)
                    :x padding-left :y current-y)))

      (setq current-y (+ current-y line-height))
      (setq idx (1+ idx)))
    svg))

;; ==================== 5. 核心逻辑：智能判定功能性标签（纯文本安全比对） ====================
(defun ekg-svg-header--functional-tag-p (tag-str)
  "判定【字符串格式】的 TAG-STR 是否属于系统功能或 ekg-org 状态标签。"
  (let* (;; 1. 获取基础 ekg 自带的功能标签并强制转换为纯字符串
         (base-functional-tags (list (and (boundp 'ekg-function-tag) ekg-function-tag)
                                     (and (boundp 'ekg-template-tag) ekg-template-tag)
                                     (and (boundp 'ekg-trash-tag) ekg-trash-tag)
                                     (and (boundp 'ekg-draft-tag) ekg-draft-tag)))
         (base-strs (mapcar (lambda (x) (and x (format "%s" x))) base-functional-tags))
         ;; 2. 前置兼容字符串
         (archive-tag-target (if (boundp 'ekg-org-archive-tag) (format "%s" ekg-org-archive-tag) "org/archive"))
         (task-tag-target (if (boundp 'ekg-org-task-tag) (format "%s" ekg-org-task-tag) "org/task"))
         ;; 3. 状态前缀
         (prefix-target (if (boundp 'ekg-org-state-tag-prefix) ekg-org-state-tag-prefix "org/state/")))
    (or
     (member tag-str base-strs)
     (string= tag-str archive-tag-target)
     (string= tag-str task-tag-target)
     (string-prefix-p prefix-target tag-str))))

;; ==================== 6. 重写 ekg 顶栏属性解析与组装逻辑 (通用多值扁平化算法：除 Tags 外万物多值各占一行) ====================
(defun ekg-svg-header--refresh-now ()
  "强制重组当前 buffer 的 ekg 顶栏并刷新界面视图。"
  (when (and (boundp 'ekg-note) ekg-note)
    (setq header-line-format (prog1 (ekg--header-line-format)
                               (make-local-variable 'header-line-format)))
    (force-mode-line-update)))

(define-advice ekg--header-line-format (:override () ekg-svg-fully-dynamic)
  "使用 SVG 动态多行技术。全屏无缝铺满，除 çTags 外，内存 Properties 中的任何自定义属性只要存在多值，一律拆分各占一行显示。"
  (when ekg-note
    (let* ((bucket-tags '()) (bucket-func-tags '()) (bucket-ref '()) (bucket-title '()) (bucket-other '())
           (win (get-buffer-window (current-buffer)))
           (padding-offset (if ekg-svg-header-use-padding 100 50))
           (win-char-width (+ (if (window-live-p win) (window-width win) 80) padding-offset))
           (frame-width-px (frame-pixel-width)))

      ;; 1. 提取并清洗标签（完美保留并捍卫你的 ç 和 ƒ 自定义符号，保持单行逗号隔开）
      (let* ((all-raw-tags (ekg-note-tags ekg-note))
             (cleaned-str-tags (cl-remove-if
                                (lambda (t-str) (string-empty-p t-str))
                                (mapcar (lambda (raw-t) (string-trim (format "%s" raw-t))) all-raw-tags)))
             (func-tags-list (cl-remove-if-not #'ekg-svg-header--functional-tag-p cleaned-str-tags))
             (normal-tags-list (cl-remove-if #'ekg-svg-header--functional-tag-p cleaned-str-tags)))

        (let ((normal-str (if normal-tags-list (mapconcat #'identity normal-tags-list ", ") "None")))
          (push (cons (concat "çTags: " normal-str) (ekg-svg-header--get-color 'tags)) bucket-tags))

        (when func-tags-list
          (let ((func-str (mapconcat #'identity func-tags-list ", ")))
            (push (cons (concat "ƒTags: " func-str) (ekg-svg-header--get-color 'func-tags)) bucket-func-tags))))

      ;; 2. 提取 Resource —— 只保留底层原生自带的唯一数字主键 ID
      (when (ekg-should-show-id-p (ekg-note-id ekg-note))
        (let ((note-id (format "%s" (ekg-note-id ekg-note))))
          (unless (string-empty-p (string-trim note-id))
            (push (cons (concat "Resource: " note-id) (ekg-svg-header--get-color 'ref)) bucket-ref))))

      ;; 3. 动态提取所有内存属性 —— 【终极重构：通用属性多值一律平铺各占一行算法】
      (map-do (lambda (prop value)
                (let* ((prop-key-str (downcase (format "%s" prop)))
                       (prop-name (ekg-property-name-for prop))
                       (formatter (alist-get prop ekg-property-format-functions)))

                  ;; 只要有值，且不属于黑名单隐藏过滤范围（特赦所有被用户明确写入的多值字段）
                  (unless (or (and (not (member prop-key-str '("ref" "title")))
                                   (memq prop ekg-header-hidden-properties))
                              (null value))

                    ;; 【通用扁平化提取】不管是标准 List 列表、Vector 向量，统统直接碾平为最干净的原子值列表
                    (let ((val-list (cond
                                     ((stringp value) (list value))
                                     ((vectorp value) (append value nil))
                                     ((and (listp value) (not (keywordp (car value)))) value)
                                     (t (list value)))))

                      ;; 动态遍历每一个具体的原子单值，强迫它们每个人独立自成一整行
                      (dolist (single-val val-list)
                        (let ((formatted-value
                               (if (and formatter (not (listp single-val)))
                                   ;; 格式化安全保护：将单值送入 Formatter 计算，防止 Formatter 遇到对象群直接崩溃
                                   (funcall formatter single-val)
                                 (format "%s" single-val))))

                          (when (and prop-name (not (string-empty-p (string-trim formatted-value))))
                            ;; 根据属性角色的文本键名，动态归类并染色送入不同的组装队列中
                            (cond
                             ((string= prop-key-str "title")
                              (push (cons (concat "Title: " formatted-value) (ekg-svg-header--get-color 'title)) bucket-title))

                             ((string= prop-key-str "ref")
                              (push (cons (concat "Ref: " formatted-value) (ekg-svg-header--get-color 'ref)) bucket-ref))

                             ;; 【通用爆发】除 tags、title、ref 之外的任何用户自定义多值字段（如 author、url 等），也在这里被解包各占一行！
                             (t
                              (push (cons (concat prop-name ": " formatted-value) (ekg-svg-header--get-color 'other)) bucket-other))))))))))
              (ekg-note-properties ekg-note))

      ;; 所有队列统一执行反转，完美挽回由于 push 导致的物理顺序倒置，还原最纯正的数据物理出场先后顺序
      (setq bucket-ref (nreverse bucket-ref))
      (setq bucket-title (nreverse bucket-title))
      (setq bucket-other (nreverse bucket-other))

      ;; 4. 按照用户定义的显示顺序动态组装所有展开后的多行数据
      (let ((dynamic-lines '()) (dynamic-colors '()))
        (dolist (type ekg-svg-header-order-weights)
          (cond
           ((eq type 'tags)
            (dolist (item bucket-tags) (push (car item) dynamic-lines) (push (cdr item) dynamic-colors)))
           ((eq type 'functional-tags)
            (dolist (item bucket-func-tags) (push (car item) dynamic-lines) (push (cdr item) dynamic-colors)))
           ((eq type 'ref)
            (dolist (item bucket-ref) (push (car item) dynamic-lines) (push (cdr item) dynamic-colors)))
           ((eq type 'title)
            (dolist (item bucket-title) (push (car item) dynamic-lines) (push (cdr item) dynamic-colors)))
           ((eq type 'other)
            (dolist (item bucket-other) (push (car item) dynamic-lines) (push (cdr item) dynamic-colors)))))

        (setq dynamic-lines (nreverse dynamic-lines))
        (setq dynamic-colors (nreverse dynamic-colors))

        ;; 5. 使用极其精准的全窗口像素宽 frame-width-px 进行不溢出的色块硬背景生成
        (propertize (make-string win-char-width ?\ )
                    'display (svg-image (ekg-svg-dynamic-multi-line-header
                                         dynamic-lines dynamic-colors frame-width-px)
                                        :ascent 'center))))))

;; ==================== 7. 全自动推导的补全选择菜单与快捷工具包 ====================
(defun ekg-svg-header-select-theme ()
  "交互式切换 ekg 配色主题。无需任何硬编码，直接从中央字典自动推导并渲染补全菜单。"
  (interactive nil ekg-capture-mode ekg-edit-mode)
  (let* (;; 1. 过滤掉 default 兜底项，并生成具有可读性的首字母大写补全候选表
         (theme-symbols (cl-remove 'default (mapcar #'car ekg-svg-header-theme-palette)))
         (completion-table (mapcar (lambda (sym)
                                     (cons (capitalize (symbol-name sym)) sym))
                                   theme-symbols))
         ;; 2. 呼出 Emacs 标准补全菜单 (无缝兼容 Vertico, Ivy, Helm 等)
         (choice (completing-read "选择 ekg 顶栏主题: " (mapcar #'car completion-table) nil t))
         (selected-theme (cdr (assoc choice completion-table))))

    (when selected-theme
      ;; 3. 动态应用主题并执行静默无感知局部刷新
      (setq ekg-svg-header-theme selected-theme)
      (message "已成功切换 ekg 顶栏主题至：%s" choice)
      (ekg-svg-header--refresh-now))))

(defun ekg-svg-header-toggle-padding ()
  "交互式一键开关多行顶栏的呼吸内边距，并实时刷新当前渲染状态。"
  (interactive nil ekg-capture-mode ekg-edit-mode)
  (setq ekg-svg-header-use-padding (not ekg-svg-header-use-padding))
  (message "ekg 顶栏呼吸内边距已 [%s]"
           (if ekg-svg-header-use-padding "开启 - 优雅卡片模式" "关闭 - 极致紧凑模式"))
  (ekg-svg-header--refresh-now))

(provide 'ekg-header)
;;; ekg-header.el ends here
