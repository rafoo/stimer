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

(defcustom timer-buffer-name "*Timer*"
  "Name of the buffer in which the timer is run."
  :group 'timer
  :type 'string
  )

(setq timer-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "s" 'timer-start)
    map)
  )

(define-derived-mode
  timer-mode
  special-mode
  "Timer"
  :group 'timer
  :keymap 'timer-mode-map
  )

(add-hook 'timer-mode-hook 'buffer-disable-undo)

(defun timer-buffer ()
  "Returns the buffer in which the timer is displayed.
If it doesn't exist yet, it is created and switched to."
  (or (get-buffer timer-buffer-name)
      (switch-to-buffer timer-buffer-name)))

(defun timer-simple-display (elapsed)
  "Display the value of elapsed in the timer buffer."
  (with-current-buffer (timer-buffer)
    (let ((buffer-read-only))
      (erase-buffer)
      (insert (format "%.2f\n" elapsed)))))

(defun timer-countdown-display (elapsed init)
  "Display the value of init - elapsed in the timer buffer."
  (with-current-buffer (timer-buffer)
    (let ((buffer-read-only))
      (erase-buffer)
      (insert (format "%.2f\n" (- init elapsed))))))

(defun timer-start (&optional display &rest args)
  "Run the timer until an event occur.
The argument DISPLAY is a function called with the time to display as first argument and ARGS as rest arguments; it defaults to timer-simple-display."
  (interactive)
  (let* ((start (current-time))
         (display (or display 'timer-simple-display))
         diff elapsed)
    (with-current-buffer (timer-buffer)
      (timer-mode))
    (while (sit-for 0.0001) ; stoped by any event
      (setq diff (time-subtract (current-time) start))
      (setq elapsed (+ (cadr diff) (/ (caddr diff) 1000000.0)))
      (apply display elapsed args)
      )
    (apply display elapsed args)
    elapsed))

(defun timer ()
  "Switch to the timer buffer."
  (interactive)
  (switch-to-buffer (timer-buffer))
  (timer-mode)
  (timer-simple-display 0)
  )

(provide 'timer)
