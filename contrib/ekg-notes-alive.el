;;; ekg-notes-alive.el --- Live notes buffer: keep instance, diff re-render -*- lexical-binding: t; -*-

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

;; Refactor of the ekg-show notes buffer (`ekg-notes-mode') onto vui's
;; live-update model, implemented as a contrib module so upstream
;; `ekg.el' stays untouched:
;;
;; 1. Instance-alive refresh.  `ekg--notes-mount' is advised so that,
;;    when the current buffer already holds a live vui instance, a
;;    refresh calls `vui-update-props' (diff re-render) instead of
;;    erasing the buffer and re-creating the whole tree.  vui's
;;    `vui--rerender-buffer' already saves and restores the cursor and
;;    the window-start, so `ekg-notes-refresh' no longer loses
;;    point / scroll position.  The advice additionally re-anchors
;;    point onto the same note by `:ekg-note-id', so the semantic
;;    anchor survives content shifts (a note inserted above).
;;
;; 2. Contiguous note identity.  `ekg-notes-root' is redefined so each
;;    note is rendered as ONE `vui-text' that includes the trailing
;;    newline(s); the `:ekg-note-id' text property therefore covers the
;;    whole note region and there are no property gaps between notes,
;;    which turns `ekg--notes-fill-id-gaps' into a no-op.
;;
;; 3. Per-note error isolation.  `ekg-display-note' runs inside a
;;    per-note `condition-case' so one broken note degrades to an
;;    inline placeholder instead of blanking the whole buffer.  A
;;    top-level `vui-error-boundary' catches render failures of the
;;    whole root as a safety net.
;;
;; 4. In-buffer tag filter.  `f' filters the current list to one tag
;;    (prompted from the tags present in the list), `F' clears the
;;    filter.  The active filter is shown in the header line with a
;;    clickable clear action.  Filtering happens by wrapping the notes
;;    fetch function in `ekg--notes-mount', so it applies on first
;;    mount and on every refresh, and composes with the live
;;    re-render.
;;
;; 5. Auto-refresh.  When `ekg-notes-alive-auto-refresh' is non-nil
;;    (default), a file-notify watch is kept on the current database
;;    file (its truename, so Dropbox symlinks resolve to the real
;;    file).  On change events, all live `ekg-notes-mode' buffers are
;;    refreshed after `ekg-notes-alive-auto-refresh-delay' seconds of
;;    debounce, reusing the upstream `ekg--refresh-notes-buffers'.
;;    This makes e.g. note edits elsewhere or an external writer show
;;    up without losing point / scroll.

;;; Code:

(require 'ekg)
(require 'filenotify)

(defcustom ekg-notes-alive-auto-refresh t
  "When non-nil, refresh all live `ekg-notes-mode' buffers when the
database file changes on disk."
  :type 'boolean
  :group 'ekg)

(defcustom ekg-notes-alive-auto-refresh-delay 0.5
  "Debounce seconds before auto-refreshing after a database change."
  :type 'number
  :group 'ekg)

(defvar-local ekg-notes-alive--filter-tag nil
  "When non-nil, only notes tagged with this tag are shown in the
current notes buffer.")

(defvar ekg-notes-alive--watch nil
  "Cons (descriptor . db-file) of the active file-notify watch, or nil.")

(defvar ekg-notes-alive--refresh-timer nil
  "Pending debounce timer for auto-refresh.")

(defun ekg-notes-alive--db-file ()
  "Return the truename of the current EKG database file."
  (file-truename
   (expand-file-name (or (ekg-db-file) triples-default-database-filename)
                     user-emacs-directory)))

(defun ekg-notes-alive--note-line (note)
  "Return NOTE's display text plus a trailing blank line.
The trailing newlines belong to NOTE so that the `:ekg-note-id' text
property is contiguous between notes."
  (concat (string-trim-right (ekg-display-note note ekg-display-note-template))
          "\n\n"))

(vui-defcomponent ekg-notes-root (name notes-func)
  "Notes list root, redefined by `ekg-notes-alive' for live updates."
  :render
  (let* ((fetch (condition-case err
                    (cons t (funcall notes-func))
                  (error (cons nil (error-message-string err)))))
         (fetch-ok (car fetch))
         (notes (cdr fetch)))
    (if (not fetch-ok)
        (vui-text (format "[ekg-notes-root: fetch notes error: %s]" notes)
          :face 'error)
      (vui-error-boundary
       :id 'ekg-notes-root
       :fallback
       (lambda (err)
         (vui-text (format "[ekg-notes-root render error: %s]"
                           (error-message-string err))
           :face 'error))
       :children
       (list
        (apply #'vui-vstack
               :spacing 0
               (mapcar (lambda (note)
                         (let ((id (ekg-note-id note)))
                           (condition-case err
                               (vui-text (ekg-notes-alive--note-line note)
                                 :key (intern (format "note-%s" id))
                                 :ekg-note-id id)
                             (error
                              (vui-text (format "[note %s display error: %s]"
                                                id (error-message-string err))
                                :face 'error
                                :key (intern (format "note-%s" id))
                                :ekg-note-id id)))))
                       notes)))))))

(defun ekg-notes-alive--wrap-notes-func (notes-func)
  "Wrap NOTES-FUNC so the current tag filter is applied to its result."
  (if-let* ((tag ekg-notes-alive--filter-tag))
      (lambda ()
        (seq-filter (lambda (note)
                      (member tag (ekg-note-tags note)))
                    (funcall notes-func)))
    notes-func))

(defun ekg-notes-alive--update-header ()
  "Show the active filter in the notes buffer header line."
  (when (derived-mode-p 'ekg-notes-mode)
    (let* ((name (or (bound-and-true-p ekg-notes-name) ""))
           (tag ekg-notes-alive--filter-tag))
      (setq header-line-format
            (if (null tag)
                (propertize (concat " " name) 'face 'bold)
              (let ((map (make-sparse-keymap)))
                (define-key map (kbd "RET") #'ekg-notes-alive-clear-filter)
                (define-key map [mouse-1]
                            (lambda (event)
                              (interactive "e")
                              (posn-set-point (event-end event))
                              (ekg-notes-alive-clear-filter)))
                (propertize (concat " " name
                                    (format "  [filter: %s  × 清除]" tag))
                            'face 'bold
                            'keymap map
                            'mouse-face 'highlight
                            'help-echo "RET/左键: 清除过滤")))))))

(defun ekg-notes-alive--notes-tags ()
  "Return all tags present in the current (unfiltered) notes list."
  (when (functionp ekg-notes-fetch-notes-function)
    (delete-dups
     (seq-mapcat #'ekg-note-tags (funcall ekg-notes-fetch-notes-function)))))

(defun ekg-notes-alive-set-filter ()
  "Filter the current notes buffer to one tag.
The tag is prompted for, from the tags present in the current list."
  (interactive nil ekg-notes-mode)
  (let ((tag (completing-read "Filter tag: " (ekg-notes-alive--notes-tags))))
    (setq ekg-notes-alive--filter-tag (and (not (string-empty-p tag)) tag))
    (ekg-notes-refresh)))

(defun ekg-notes-alive-clear-filter ()
  "Clear the tag filter in the current notes buffer."
  (interactive nil ekg-notes-mode)
  (setq ekg-notes-alive--filter-tag nil)
  (ekg-notes-refresh))

(define-key ekg-notes-mode-map "f" #'ekg-notes-alive-set-filter)
(define-key ekg-notes-mode-map "F" #'ekg-notes-alive-clear-filter)

(defun ekg-notes-alive--schedule-refresh ()
  "Debounce and then refresh all live notes buffers."
  (when ekg-notes-alive--refresh-timer
    (cancel-timer ekg-notes-alive--refresh-timer))
  (setq ekg-notes-alive--refresh-timer
        (run-with-timer ekg-notes-alive-auto-refresh-delay nil
                        #'ekg-notes-alive--maybe-refresh-all)))

(defun ekg-notes-alive--maybe-refresh-all ()
  "Refresh all live `ekg-notes-mode' buffers (used by the debounce)."
  (setq ekg-notes-alive--refresh-timer nil)
  (when ekg-notes-alive-auto-refresh
    (ekg--refresh-notes-buffers)))

(defun ekg-notes-alive--on-file-change (event)
  "Handle a file-notify EVENT on the watched database file."
  (when (and ekg-notes-alive-auto-refresh (listp event))
    (let ((desc (car event))
          (action (nth 1 event)))
      (when (and desc (eq desc (car-safe ekg-notes-alive--watch)))
        (cond
         ((memq action '(stopped deleted renamed))
          ;; Watch died (e.g. Dropbox replaced the file): re-register.
          (ekg-notes-alive--ensure-watch))
         ((memq action '(created changed attribute-changed))
          (ekg-notes-alive--schedule-refresh)))))))

(defun ekg-notes-alive--ensure-watch ()
  "Register (or re-register) a file-notify watch on the database file."
  (when ekg-notes-alive-auto-refresh
    (let ((file (ekg-notes-alive--db-file)))
      (when (and file (file-exists-p file))
        (unless (and ekg-notes-alive--watch
                     (equal (cdr ekg-notes-alive--watch) file)
                     (file-notify-valid-p (car ekg-notes-alive--watch)))
          (when (car-safe ekg-notes-alive--watch)
            (ignore-errors
              (file-notify-rm-watch (car ekg-notes-alive--watch))))
          (setq ekg-notes-alive--watch
                (cons (file-notify-add-watch file '(change)
                                             #'ekg-notes-alive--on-file-change)
                      file)))))))

(defun ekg-notes-alive--mount (orig-fn name notes-func)
  "Around-advice for `ekg--notes-mount'.

When the current buffer already holds a live vui instance, update its
props in place (diff re-render) instead of erasing the buffer and
re-creating the whole tree.  Falls back to ORIG-FN for the first
mount in a buffer.  The tag filter is applied by wrapping NOTES-FUNC;
the header line and the database watch are kept in sync afterwards."
  (let* ((notes-func (ekg-notes-alive--wrap-notes-func notes-func))
         (instance (and (derived-mode-p 'ekg-notes-mode)
                        (bound-and-true-p ekg-notes--instance)
                        (let ((buf (vui-instance-buffer ekg-notes--instance)))
                          (and (buffer-live-p buf)
                               (eq buf (current-buffer))
                               ekg-notes--instance))))
         (note-id (and instance (get-text-property (point) :ekg-note-id))))
    (if (null instance)
        (funcall orig-fn name notes-func)
      ;; Live path: re-render in place; vui restores cursor + viewport.
      (let ((inhibit-read-only t))
        (vui-update-props instance (list :name name :notes-func notes-func))
        ;; Fill any property gaps the re-render may have left (vui's
        ;; commit path can insert separator runs without :ekg-note-id),
        ;; restoring the invariant the navigation code relies on.
        (ekg--notes-fill-id-gaps)
        ;; Re-anchor point on the same note.
        (when note-id
          (let ((pos (text-property-any (point-min) (point-max)
                                        :ekg-note-id note-id)))
            (when pos (goto-char pos)))))
      instance)
    (ekg-notes-alive--update-header)
    (ekg-notes-alive--ensure-watch)))

(advice-add 'ekg--notes-mount :around #'ekg-notes-alive--mount)

(provide 'ekg-notes-alive)
;;; ekg-notes-alive.el ends here
