;;; ekg-db.el --- ekg database API -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024, Qingshui Zheng <qingshuizheng@outlook.com>
;;
;; Author: Qingshui Zheng <qingshuizheng@outlook.com>
;; Maintainer: Qingshui Zheng <qingshuizheng@outlook.com>
;;
;; Created: 13 Jan 2024
;;
;; URL: https://github.com/qingshuizheng/zemacs/tree/master/emacs
;;
;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(require 'triples)

(defgroup ekg nil nil)

(defun ekg-db--rows-with-pred (pred)
  (ekg-connect)
  (triples-db-select ekg-db nil pred))

;; (ekg-db--rows-with-pred 'tagged/tag)

(defun ekg-db--row-subs-with-pred (pred)
  (seq-uniq (mapcar #'car (ekg-db--rows-with-pred pred))))

;; (ekg-db--row-subs-with-pred 'titled/title)



(defun ekg-db--rows-with-pred-and-objs (pred objs)
  (ekg-connect)
  (let ((objs (if (listp objs)
                  objs
                (list objs))))
    (mapcan (lambda (obj) (triples-db-select ekg-db nil pred obj))
            objs)))

;; (ekg-db--rows-with-pred-and-objs 'tagged/tag (list "test"))

(defun ekg-db--row-subs-with-pred-and-objs (pred objs)
  (seq-uniq (mapcar #'car (ekg-db--rows-with-pred-and-objs pred objs))))

;; (ekg-db--row-subs-with-pred-and-objs 'tagged/tag (list "test"))



(defun ekg-db--rows-with-pred-and-obj-matching (pred op re)
  (ekg-connect)
  (triples-db-select-pred-op ekg-db 'tagged/tag op re))

;; (ekg-db--rows-with-pred-and-obj-matching 'tagged/tag 'like (format "%s%%" "trash/"))

(defun ekg-db--row-subs-with-pred-and-obj-matching (pred op re)
  (seq-uniq (mapcar #'car (triples-db-select-pred-op ekg-db 'tagged/tag op re))))

;; (ekg-db--row-subs-with-pred-and-obj-matching 'tagged/tag 'like (format "%s%%" "trash/"))




(defun ekg-db--rows-of-pred/target-from-pred/src (target src src-vals)
  (ekg-connect)
  (let* (;; (src-ids (ekg-db--row-subs-with-pred-and-objs src src-vals))
         (src-vals (if (listp src-vals) src-vals (list src-vals)))
         (src-ids (mapcan (lambda (sub)
                            (triples-subjects-with-predicate-object
                             ekg-db src sub))
                          src-vals)))
    (mapcan (lambda (id) (triples-db-select ekg-db id target))
            src-ids)))

;; (benchmark-elapse-and-ratio
;;  (ekg-db--rows-of-pred/target-from-pred/src 'titled/title 'tagged/tag '("test")))

;; (benchmark-elapse-and-ratio
;;  (ekg-db--rows-of-pred/target-from-pred/src 'tagged/tag 'reffed/ref '("test")))

;; (benchmark-elapse-and-ratio
;;  (ekg-db--row-subs-with-pred-and-objs 'tagged/tag '("test"))
;;  (let* (;; (src-ids (ekg-db--row-subs-with-pred-and-objs src src-vals))
;;         (src-vals (list "test"))
;;         (src-vals (if (listp src-vals) src-vals (list src-vals)))
;;          (src-ids (mapcan (lambda (sub)
;;                             (triples-subjects-with-predicate-object
;;                              ekg-db 'tagged/tag sub))
;;                           src-vals))) src-ids))











(provide 'ekg-db)
;;; ekg-db.el ends here
