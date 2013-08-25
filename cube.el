;;; cube.el --- A speedcube timer

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
; (add-to-list 'load-path "<path-to-cube>/")
; (require 'cube)

;; Description:
;; This file implements a speedcube timer for Emacs.

;;; Code:

(add-to-list 'load-path "./")

(require 'timer)
(require 'dnf)

(defvar cube-time-list nil
  "The list of already measured times.")

(defgroup cube nil
  "A speedcube timer."
  :group 'applications
  )

(defcustom cube-buffer-name "*Cube*"
  "Name of the buffer in which the speedcube programm runs."
  :group 'cube
  :type 'string
  )

(defun cube-buffer ()
  "Returns the buffer in which the cube stats are displayed.
If it doesn't exist yet, it is created and switched to."
  (or (get-buffer cube-buffer-name)
      (switch-to-buffer cube-buffer-name)))

(setq cube-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "s" 'cube-next-time)
    (define-key map "d" 'cube-dnf)
    (define-key map (kbd "DEL") 'cube-clear-list)
    map)
;  "Keymap for `timer-mode'."
  )

(define-derived-mode
  cube-mode
  special-mode
  "Cube"
  :group 'cube
  :keymap 'cube-mode-map
  )

(defun cube-display ()
  "Display some statistics on registered times in the cube buffer."
  (with-current-buffer (cube-buffer)
    (let ((buffer-read-only))
      (erase-buffer)
      (insert (format "%d times\n" (length cube-time-list)))
      (insert (format "avg: %s\n" (apply 'dnf-avg cube-time-list)))
      (insert (format "best: %s\n" (apply 'dnf-min cube-time-list)))
      (insert (format "mean: %s\n" (apply 'dnf-mean cube-time-list)))
      (insert "\n")
      (insert (format "All times:\n%s" cube-time-list))
      )))

(defun cube-next-time ()
  "Run the timer and store the result in cube-time-list."
  (interactive)
  (setq cube-time-list (cons (timer-start) cube-time-list))
  (cube-display)
  )

(defun cube-dnf ()
  "Replace the last registered time by the symbol dnf."
  (interactive)
  (setq cube-time-list (cons 'dnf (cdr cube-time-list)))
  (cube-display)
  )

(defun cube-clear-list ()
  "Empty the list cube-time-list."
  (interactive)
  (setq cube-time-list nil)
  (cube-display)
  (timer-simple-display 0)
  )

(defun cube ()
  "Switch to the cube buffer."
  (interactive)
  (timer)
  (pop-to-buffer (cube-buffer))
  (cube-mode)
  (cube-display)
  )

(provide 'cube)
