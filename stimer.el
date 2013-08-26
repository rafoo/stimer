;;; stimer.el --- A simple timer for Emacs

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
; (add-to-list 'load-path "<path-to-stimer>/")
; (require 'stimer)

;; run this programme by the command
; M-x stimer-start

;; Description:
;; This file implements a simple timer for Emacs.

;;; Code:

(defgroup stimer nil
  "A simple timer."
  :group 'applications
  )

(defcustom stimer-buffer-name "*STimer*"
  "Name of the buffer in which the stimer is run."
  :group 'stimer
  :type 'string
  )

(setq stimer-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "s" 'stimer-start)
    map)
  )

(define-derived-mode
  stimer-mode
  special-mode
  "STimer"
  :group 'stimer
  :keymap 'stimer-mode-map
  )

(add-hook 'stimer-mode-hook 'buffer-disable-undo)

(defun stimer-buffer ()
  "Returns the buffer in which the stimer is displayed.
If it doesn't exist yet, it is created and switched to."
  (or (get-buffer stimer-buffer-name)
      (switch-to-buffer stimer-buffer-name)))

(defun stimer-simple-display (elapsed)
  "Display the value of elapsed in the stimer buffer."
  (with-current-buffer (stimer-buffer)
    (let ((buffer-read-only))
      (erase-buffer)
      (insert (format "%.2f" elapsed)))))

(defun stimer-countdown-display (elapsed init)
  "Display the value of init - elapsed in the stimer buffer."
  (with-current-buffer (stimer-buffer)
    (let ((buffer-read-only))
      (erase-buffer)
      (insert (format "%.2f" (- init elapsed))))))

(defun stimer-start (&optional display &rest args)
  "Run the stimer until an event occur.
The argument DISPLAY is a function called with the time to display as first argument and ARGS as rest arguments; it defaults to stimer-simple-display."
  (interactive)
  (let* ((start (current-time))
         (display (or display 'stimer-simple-display))
         diff elapsed)
    (with-current-buffer (stimer-buffer)
      (stimer-mode))
    (while (sit-for 0.0001) ; stoped by any event
      (setq diff (time-subtract (current-time) start))
      (setq elapsed (+ (cadr diff) (/ (caddr diff) 1000000.0)))
      (apply display elapsed args)
      )
    (apply display elapsed args)
    elapsed))

(defun stimer ()
  "Switch to the stimer buffer."
  (interactive)
  (switch-to-buffer (stimer-buffer))
  (stimer-mode)
  (stimer-simple-display 0)
  )

(provide 'stimer)
