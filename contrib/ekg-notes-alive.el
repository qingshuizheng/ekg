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

;;; Code:

(require 'ekg)

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

(defun ekg-notes-alive--mount (orig-fn name notes-func)
  "Around-advice for `ekg--notes-mount'.

When the current buffer already holds a live vui instance, update its
props in place (diff re-render) instead of erasing the buffer and
re-creating the whole tree.  Falls back to ORIG-FN for the first
mount in a buffer."
  (let* ((instance (and (derived-mode-p 'ekg-notes-mode)
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
      instance)))

(advice-add 'ekg--notes-mount :around #'ekg-notes-alive--mount)

(provide 'ekg-notes-alive)
;;; ekg-notes-alive.el ends here
