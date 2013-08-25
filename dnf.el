;;; dnf.el --- Arithmetic with an infinity "Did Not Finish" value

;;; Copyright (C) 2013 Raphaël Cauderlier <cauderlier@crans.org>

;;; Author: Raphaël Cauderlier <cauderlier@crans.org>

;;; Version: 0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; This file is *NOT* part of GNU Emacs.

;;; Commentary:
;; Installation:
;; Add these lines to your emacs init file :
; (add-to-list 'load-path "<path-to-dnf>/")
; (require 'dnf)

;; Description:
;; This file implements a few operations on numbers extended by an
;; infinity symbol called dnf (meaning "Did Not Finish" as usual in
;; speedcubing vocabulary).

;;; Code:

(defun dnf< (a b)
  "Return t if first arg is less than second arg.
Any number is considered less than dnf."
  (and
   (not (eq a 'dnf))
   (or (eq b 'dnf)
       (< a b))))

(defun dnf-min (&rest args)
  "Return the smallest element of ARGS according to dnf<."
  (if args
      (car (sort args 'dnf<))
    'dnf))

(defun dnf-mean (&rest args)
  "Return the arithmetic mean of ARGS.
If dnf is an element of ARGS, the result is also dnf."
  (if (or (null args) (memq 'dnf args))
    'dnf
    (/ (apply '+ args) (length args))))

(defun dnf-avg (&rest args)
  "Same as dnf-mean except that the smallest and biggest elements of ARGS are ignored."
  (apply 'dnf-mean (butlast (cdr (sort args 'dnf<)))))

(provide 'dnf)
