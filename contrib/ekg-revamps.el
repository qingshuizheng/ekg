;;; ekg-revamps.el --- Codes not pushed to upstream -*- lexical-binding: t -*-

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

(require 'ekg)
(require 'triples)
(require 'seq)

;;; --- EKG & Triples Timestamped Backup Setup ---

(defvar my/ekg-backup-directory
  (expand-file-name "ekg-backups/" user-emacs-directory)
  "Directory to store timestamped backups of the ekg/triples database.")

(defcustom my/ekg-backup-num-to-keep 30
  "Maximum number of timestamped backup files to retain."
  :type 'integer)

(define-advice triples-backup (:override (_ filename num-to-keep) timestamped-custom)
  "Override triples-backup' to route custom-timestamped files into an
isolated folder. Files are named like ekg.db-YYYYMMDDTHHMMSS. If
NUM-TO-KEEP matches most-positive-fixnum' (forced), '_force' is appended
to the filename."
  (let* ((base-path (expand-file-name (or filename triples-default-database-filename)))
         (base-name (file-name-nondirectory base-path))
         (dest-dir (file-name-as-directory my/ekg-backup-directory))
         ;; Use hyphen inside the time-string layout to be safe
         (timestamp (format-time-string "_%Y%m%dT%H%M%S"))
         ;; Check if this backup was forced by ekg-backup
         (forced-p (eq num-to-keep most-positive-fixnum))
         (suffix (concat timestamp (when forced-p "_force")))
         (backup-dest (expand-file-name (concat dest-dir base-name suffix)))
         (num-keep (if forced-p
                       most-positive-fixnum
                     (or num-to-keep my/ekg-backup-num-to-keep))))

    ;; 1. Ensure target backup directory exists
    (unless (file-directory-p dest-dir)
      (make-directory dest-dir t))

    ;; 2. Execute SQLite online backup safely via standard input (stdin)
    (let ((sqlite-exe (pcase (bound-and-true-p triples-sqlite-interface)
                        ('builtin (bound-and-true-p triples-sqlite-executable))
                        ('emacsql (bound-and-true-p emacsql-sqlite-executable))
                        (_ (executable-find "sqlite3")))))
      (when sqlite-exe
        (with-temp-buffer
          (insert (format ".backup '%s'\n" backup-dest))
          (call-process-region (point-min) (point-max) sqlite-exe nil nil nil base-path))))

    ;; 3. Prune old backups matching the new format (excluding _force from pruning)
    (let* ((pattern (concat "^" (regexp-quote base-name) "_[0-9]\{8\}T[0-9]\{6\}$"))
           (backup-files (and (file-directory-p dest-dir)
                              (directory-files dest-dir t pattern t))))
      (when (> (length backup-files) num-keep)
        (cl-loop for old-file in (nthcdr num-keep backup-files)
                 do (when (file-exists-p old-file)
                      (delete-file old-file)))))))

;;; --- EKG Upgrade Optimization Advice ---

;;;###autoload
(define-advice ekg-upgrade-db (:override (from-version) optimized-custom)
  "Optimized version of `ekg-upgrade-db' that unifies backups and structures FTS setup.
Applies conditional database schema upgrades safely using transaction boundaries."
  (let ((need-fts-upgrade
         (or (null from-version)
             (version-list-< from-version '(0 7 0))))
        (need-trash-upgrade
         (or (null from-version)
             ;; Version 0.5.0 changed how trashed tags are handled.
             (version-list-< from-version '(0 5 0))))
        (need-triple-0.3-upgrade
         (or (null from-version)
             ;; We have done upgrades to 0.3.1, but we want to re-do them for
             ;; additional bugfixes.
             (version-list-< from-version '(0 3 2))))
        (need-type-removal-upgrade
         (or (null from-version)
             (version-list-< from-version '(0 6 3)))))

    (ekg-connect)

    ;; 改进 1: 统一触发备份。只要满足任意一个需要写入的升级条件，就【只备份一次】
    (when (or need-type-removal-upgrade
              need-triple-0.3-upgrade
              need-trash-upgrade)
      (ekg-backup t))

    ;; 改进 2: 去掉重复的 triples-fts-setup 调用。
    (when need-fts-upgrade
      (triples-fts-setup ekg-db))

    (when need-type-removal-upgrade
      (triples-remove-schema-type ekg-db 'person)
      (triples-remove-schema-type ekg-db 'email))

    (when need-triple-0.3-upgrade
      ;; This converts all string integers in subjects and objects to real integers.
      (triples-upgrade-to-0.3 ekg-db)
      ;; Convert tag subjects back to strings if they became numerical integers.
      (cl-loop for tag in (triples-subjects-of-type ekg-db 'tag) do
               (when (numberp tag)
                 (ekg-global-rename-tag tag (format "%d" tag)))))

    (when need-trash-upgrade
      (let* ((trash-ids (ekg-tags-with-prefix "trash/"))
             (note-ids (mapcan (lambda (tag) (plist-get (triples-get-type ekg-db tag 'tag) :tagged))
                               trash-ids)))
        ;; 将整个写操作包裹在数据库事务中，保证数据一致性
        (triples-with-transaction ekg-db
                                  (cl-loop for id in note-ids do
                                           (let ((note (ekg-get-note-with-id id)))
                                             (cond
                                              ((null note)
                                               (message "ekg-upgrade-db: Note %s has a trashed tag but doesn't exist!" id))
                                              ((seq-some (lambda (tag)
                                                           (not (string-match-p (rx (literal "trash/")) tag)))
                                                         (ekg-note-tags note))
                                               (message "ekg-upgrade-db: Note %s has mixed trash/normal tags. Use ekg-show-notes-with-tag-prefix to fix." id))
                                              (t
                                               (message "ekg-upgrade-db: Moving note %s from trash tags to trash tag" id)
                                               (ekg-note-trash note)))))
                                  (cl-loop for tag in trash-ids do
                                           (triples-remove-type ekg-db tag 'tag)
                                           (triples-set-type ekg-db ekg-trash-tag 'tag))))))

  ;; Always ensure core note types are registered.
  ;; Added reffed' for custom ekg-ref' functionality
  (dolist (type '(text time-tracked inline titled tagged reffed))
    (triples-set-type ekg-db type 'ekg-note-type)))

;; --- Don't Display Empty Lines ---

(define-advice ekg-display--format (:around (orig-fun text numwords format) filter-empty-text)
  "If TEXT is empty or only contains whitespaces, return an empty string immediately.
Otherwise, proceed with the original `ekg-display--format' behavior."
  (if (string-empty-p (string-trim text))
      ""
    (funcall orig-fun text numwords format)))

(provide 'ekg-revamps)
;;; ekg-revamps.el ends here
