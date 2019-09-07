;;; aria2-refresh.el --- Control aria2c commandline tool from Emacs -*- lexical-binding: t; -*-

;; Copyright (c) 2014-2019 Łukasz Gruner

;; Author: Łukasz Gruner <lukasz@gruner.lu>
;; Maintainer: Łukasz Gruner <lukasz@gruner.lu>
;; Version: 3
;; Package-Requires: ((emacs "26.2"))
;; URL: https://gitlab.com/ukaszg/aria2/
;; Created: 19/10/2014
;; Keywords: download bittorrent aria2

;; This file is not part of Emacs.


;; This work ‘as-is’ we provide.
;; No warranty express or implied.
;; We’ve done our best,
;; to debug and test.
;; Liability for damages denied.
;;
;; Permission is granted hereby,
;; to copy, share, and modify.
;; Use as is fit,
;; free or for profit.
;; These rights, on this notice, rely.


;;; Commentary:

;;; Code:

(defcustom aria2-refresh-fast 3
    "Timeout after list is refreshed, when it has focus."
    :group 'aria2
    :group 'timer
    :type  '(integer :tag "Number of seconds"))

(defcustom aria2-refresh-normal 8
    "Timeout after list is refreshed, when it doesn't have focus, but its buffer is visible."
    :group 'aria2
    :group 'timer
    :type  '(integer :tag "Number of seconds"))

(defcustom aria2-refresh-slow 20
    "Timeout after list is refreshed, when it doesn't have focus and it's buffer is not visible."
    :group 'aria2
    :group 'timer
    :type  '(integer :tag "Number of seconds"))

(defvar aria2--current-buffer-refresh-speed nil
    "One of :fast :normal :slow or nil if not refreshing. Used to manage refresh timers.")

(defvar aria2--master-timer nil
    "Holds a timer object that dynamically manages frequency of `aria2--refresh-timer', depending on visibility and focused state.")

(defvar aria2--refresh-timer nil
    "Holds a timer object that refreshes downloads list periodically.")

(defun aria2--stop-timers ()
    "Stop all timers."
    (when (timerp aria2--master-timer)  (cancel-timer aria2--master-timer))
    (when (timerp aria2--refresh-timer) (cancel-timer aria2--refresh-timer))
    (setq aria2--refresh-timer nil
        aria2--master-timer nil))

(defun aria2--start-timers ()
    "Start all timers."
    (unless (timerp aria2--master-timer)
        (setq aria2--master-timer (run-at-time t 5 #'aria2--manage-refresh-timer))
        (aria2--manage-refresh-timer)))

(defun aria2--refresh ()
    "Refreshes download list buffer."
    (when-let ((buf (get-buffer aria2-list-buffer-name)))
        (with-current-buffer buf (revert-buffer))))

(defun aria2--manage-refresh-timer ()
    "Restarts `aria2--refresh-timer' on different intervals, depending on focus and buffer visibility."
    (when-let ((buf (get-buffer aria2-list-buffer-name)))
        (cl-flet ((retimer (refresh speed)
                      (when (timerp aria2--refresh-timer) (cancel-timer aria2--refresh-timer))
                      (setq
                          aria2--refresh-timer (run-at-time t refresh #'aria2--refresh)
                          aria2--current-buffer-refresh-speed speed)))
            (cond
                ((eq buf (window-buffer (selected-window))) ; when list has focus
                    (unless (eq aria2--current-buffer-refresh-speed :fast)
                        (retimer aria2-refresh-fast :fast)))
                ((get-buffer-window buf) ; list visible but without focus
                    (unless (eq aria2--current-buffer-refresh-speed :normal)
                        (retimer aria2-refresh-normal :normal)))
                (t (unless (eq aria2--current-buffer-refresh-speed :slow) ; list is in the background
                       (retimer aria2-refresh-slow :slow)))))))

(provide 'aria2-refresh)

;; Local Variables:
;; coding: utf-8-unix
;; indent-tabs-mode: nil
;; End:

;;; aria2-refresh.el ends here
