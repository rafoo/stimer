;;; timer.el --- A simple timer for Emacs

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
; (add-to-list 'load-path "<path-to-timer>/")
; (require 'timer)

;; run this programme by the command
; M-x timer-start

;; Description:
;; This file implements a simple timer for Emacs.

;;; Code:

(defgroup timer nil
  "A simple timer."
  :group 'applications
  )

(defcustom timer-buffer "*Timer*"
  "Name of the buffer in which the timer is run."
  :group 'timer
  :type 'string
  )

(defvar timer-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "s" 'timer-start)
    (define-key map " " 'timer-stop)
    map)
  "Keymap for `timer-mode'."
  )

(defvar timer-elapsed 0
  "Time elapsed since last start of the timer-run function.")

(define-derived-mode
  timer-mode
  special-mode
  "Timer"
  :group 'timer
  :keymap 'timer-mode-map
  )

(add-hook 'timer-mode-hook 'buffer-disable-undo)

(defun timer-stop ()
  "Stop the timer.
This command should be bound to some easy key-binding like SPC."
  (interactive)
  (ignore)
  )

(defun timer-display (running)
  "Display the current value of timer-elapsed."
  (let ((buffer (or (get-buffer timer-buffer) (switch-to-buffer timer-buffer))))
    (with-current-buffer buffer
      (let ((buffer-read-only))
        (erase-buffer)
        (insert (format "%f\n" timer-elapsed))
        (if running (insert "Running.") (insert "Not running."))))))

(defun timer-start ()
  "Run the timer until an event occur."
  (interactive)
  (let* ((start (current-time))
         diff)
    (switch-to-buffer timer-buffer)
    (timer-mode)
    (while (sit-for 0.0001) ; stoped by any event
      (setq diff (time-subtract (current-time) start))
      (setq timer-elapsed (+ (cadr diff) (/ (caddr diff) 1000000.0)))
      (timer-display t)
      )
    (timer-display nil)
    timer-elapsed))

(provide 'timer)
