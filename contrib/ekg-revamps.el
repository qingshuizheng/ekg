;;; ekg-revamps.el --- Local ekg/triples tweaks not pushed upstream -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qingshui Zheng

;; Author: Qingshui Zheng <qingshuizheng@outlook.com>

;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Two local tweaks that upstream ekg/triples do not provide:
;;
;; 1. Timestamped backups.  `triples-backup' writes a copy of the database
;;    into `z/ekg-backup-directory' as "<db>_YYYYMMDDTHHMMSS", keeps the last
;;    `z/ekg-backup-num-to-keep' of them, and marks the forced backups that
;;    `ekg-backup' makes before a migration with a "_force" suffix.  Forced
;;    backups are never pruned.
;;
;; 2. No blank lines for notes without text.  `ekg-display--format' returns an
;;    empty string instead of a lone newline when the text is blank.
;;
;; It deliberately does *not* override `ekg-upgrade-db'.  Replacing the upgrade
;; function freezes a snapshot of the migrations, and that snapshot goes
;; stale as soon as upstream adds a migration -- the 0.10.0 hierarchy upgrade
;; (`org/parent' -> `hierarchy/parent'), added later, was silently skipped by
;; the old override here.  The "single backup per upgrade run" and "one
;; `triples-fts-setup' call" savings it bought are not worth that.
;;
;; Schema registration for contrib modules belongs in their own
;; `ekg-add-schema-hook' function, not in an upgrade function: the hook runs on
;; every `ekg-connect' (so it also covers a database switch), while an upgrade
;; runs once per version change.

;;; Code:

(require 'cl-lib)
(require 'ekg)
(require 'triples)

;;; --- Timestamped backups ---

(defcustom z/ekg-backup-directory
  (expand-file-name "ekg-backups/" user-emacs-directory)
  "Directory holding the timestamped ekg/triples database backups."
  :type 'directory)

(defcustom z/ekg-backup-num-to-keep 30
  "Number of timestamped backups to keep.
This takes precedence over the NUM-TO-KEEP that
`triples-backups-maybe-backup' passes down, which comes from
`ekg-default-num-backups' (a `defconst' of 5)."
  :type 'integer)

(define-advice triples-backup (:override (_ filename num-to-keep) timestamped-custom)
  "Write a timestamped copy of the database into `z/ekg-backup-directory'.
The file is named after the database plus _YYYYMMDDTHHMMSS.  A forced backup
\(the one `ekg-backup' makes before a migration, where NUM-TO-KEEP is
`most-positive-fixnum') additionally gets a _force suffix and is never pruned.
Timestamped backups beyond `z/ekg-backup-num-to-keep' are deleted."
  (let* ((base-path (expand-file-name (or filename triples-default-database-filename)))
         (base-name (file-name-nondirectory base-path))
         (dest-dir (file-name-as-directory z/ekg-backup-directory))
         (timestamp (format-time-string "_%Y%m%dT%H%M%S"))
         (forced-p (eq num-to-keep most-positive-fixnum))
         (suffix (concat timestamp (when forced-p "_force")))
         (backup-dest (expand-file-name (concat dest-dir base-name suffix)))
         (num-keep (if forced-p
                       most-positive-fixnum
                     z/ekg-backup-num-to-keep)))

    ;; 1. Ensure the backup directory exists.
    (unless (file-directory-p dest-dir)
      (make-directory dest-dir t))

    ;; 2. Let SQLite back up the live database (`.backup' via stdin), instead
    ;; of copying the file behind the connection's back.
    (let ((sqlite-exe (pcase (bound-and-true-p triples-sqlite-interface)
                        ('builtin (bound-and-true-p triples-sqlite-executable))
                        ('emacsql (bound-and-true-p emacsql-sqlite-executable))
                        (_ (executable-find "sqlite3")))))
      (when sqlite-exe
        (with-temp-buffer
          (insert (format ".backup '%s'\n" backup-dest))
          (call-process-region (point-min) (point-max) sqlite-exe nil nil nil base-path))))

    ;; 3. Prune the old timestamped backups (never the `_force' ones).  Note the
    ;; doubled backslashes: the interval is `\{8\}' in regexp syntax.
    (let* ((pattern (concat "^" (regexp-quote base-name) "_[0-9]\\{8\\}T[0-9]\\{6\\}$"))
           (backup-files (and (file-directory-p dest-dir)
                              (directory-files dest-dir t pattern t))))
      (when (> (length backup-files) num-keep)
        (cl-loop for old-file in (nthcdr num-keep backup-files)
                 do (when (file-exists-p old-file)
                      (delete-file old-file)))))))

;;; --- No blank lines for notes without text ---

(define-advice ekg-display--format (:around (orig-fun text numwords format) filter-empty-text)
  "Return an empty string when TEXT is empty or only whitespace.
Otherwise behave like `ekg-display--format'."
  (if (string-empty-p (string-trim text))
      ""
    (funcall orig-fun text numwords format)))

(provide 'ekg-revamps)
;;; ekg-revamps.el ends here
