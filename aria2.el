;;; aria2.el --- Control aria2c commandline tool from Emacs -*- lexical-binding: t; -*-

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

(defgroup aria2 nil
    "An interface for aria2 command."
    :group 'external
    :group 'execute
    :prefix "aria2-")

(defcustom aria2-mode-hook nil
    "Hook ran afer enabling `aria2-mode'."
    :type 'hook
    :group 'aria2)

(defcustom aria2-add-evil-quirks (not (string-empty-p (locate-library "evil")))
    "If t adds aria2-mode to emacs states, and binds \C-w."
    :type 'boolean
    :group 'aria2)

(require 'aria2-faces)
(require 'aria2-tview)
(require 'aria2-refresh)
(require 'aria2-exe)
(require 'aria2-dialog)

(defsubst aria2--is-paused-p ()
    (string= "paused" (car (elt (tabulated-list-get-entry) 1))))

(defun aria2-maybe-do-stuff-on-emacs-exit ()
    "Hook ran when quitting emacs."
    (when (and aria2--bin (Running? aria2--bin))
        (SaveSession aria2--bin)
        (Save aria2--bin)
        (when aria2-kill-process-on-emacs-exit
            (message "Stopping: %s" aria2-executable)
            (Shutdown! aria2--bin t))))

(defun aria2-maybe-add-evil-quirks ()
    "Keep aria2-mode in EMACS state since we already define j/k movement and add C-w * commands."
    (when aria2-add-evil-quirks
        (with-eval-after-load 'evil-states
            (add-to-list 'evil-emacs-state-modes 'aria2-mode)
            (add-to-list 'evil-emacs-state-modes 'aria2-dialog-mode))
        (with-eval-after-load 'evil-maps
            (define-key aria2-mode-map "\C-w" 'evil-window-map))))

(add-hook 'aria2-mode-hook #'aria2-maybe-add-evil-quirks)

;; Interactive commands start here

;;;###autoload
(defun aria2-print-daemon-commandline ()
    "Prints full commandline for aria2."
    (interactive)
    (let ((options (aria2-start-cmd)))
        (message "# %s %s" aria2-executable (string-join options " "))))

(defun aria2-pause (all)
    "Pause download."
    (interactive "P")
    (when-let ((gid (tabulated-list-get-id)))
        (Pause aria2--bin gid all)
        (message "Pausing download. This may take a moment...")
        (aria2--refresh)))

(defun aria2-resume (all)
    "Resume paused download."
    (interactive "P")
    (when-let ((gid (tabulated-list-get-id)))
        (Unpause aria2--bin gid all)
        (aria2--refresh)))

(defun aria2-toggle-pause (all)
    "Toggle 'paused' status for download."
    (interactive "P")
    (if (aria2--is-paused-p)
        (aria2-resume all)
        (aria2-pause all))
    (aria2--refresh))

(defun aria2-remove-download ()
    "Set download status to 'removed'."
    (interactive)
    (let ((gid (tabulated-list-get-id)))
        (when (and gid (y-or-n-p "Really remove download? "))
            (RemoveResult aria2--bin gid)
            (revert-buffer))))

(defun aria2-clean-removed-download ()
    "Clean download with 'removed/completed/error' status.
With prefix remove all applicable downloads."
    (interactive)
    (PurgeResult aria2--bin)
    (aria2--refresh))

(defun aria2-move-up-in-list (arg)
    "Move item one row up, with prefix move to beginning of list."
    (interactive "P")
    (when-let ((gid (tabulated-list-get-id)))
        (GidPosition aria2--bin gid (if (equal nil arg) -1 0) (if (equal nil arg) "POS_CUR" "POS_SET"))
        (revert-buffer)))

(defun aria2-move-down-in-list (arg)
    "Move item one row down, with prefix move to end of list."
    (interactive "P")
    (when-let ((gid (tabulated-list-get-id)))
        (GidPosition aria2--bin gid (if (equal nil arg) 1 0) (if (equal nil arg) "POS_CUR" "POS_END"))
        (revert-buffer)))

(defun aria2-terminate (arg)
    "Stop aria2 process and kill buffers."
    (interactive "P")
    (when (y-or-n-p (format "Are you sure yo want to terminate aria2 (pid:%s) process? " (or (oref aria2--bin pid) "?")))
        (Shutdown! aria2--bin arg)
        (kill-buffer aria2-list-buffer-name)
        (aria2--stop-timers)
        (remove-hook 'kill-emacs-hook #'aria2-maybe-do-stuff-on-emacs-exit)))

;; Mode line format starts here

(defvar aria2-mode-line-format
    (list
        (propertize "%b" 'face 'mode-line-buffer-id)
        " "
        (propertize
            (concat "[" (propertize "f" 'face 'aria2-modeline-key-face) "]:add file")
            'local-map (make-mode-line-mouse-map 'down-mouse-1 #'aria2-add-file)
            'mouse-face 'aria2-modeline-mouse-face)
        " "
        (propertize
            (concat "[" (propertize "u" 'face 'aria2-modeline-key-face) "]:add url")
            'local-map (make-mode-line-mouse-map 'down-mouse-1 #'aria2-add-uris)
            'mouse-face 'aria2-modeline-mouse-face)
        " "
        (propertize
            (concat "[" (propertize "D" 'face 'aria2-modeline-key-face) "]:remove download")
            'local-map (make-mode-line-mouse-map 'down-mouse-1 #'aria2-remove-download)
            'mouse-face 'aria2-modeline-mouse-face)
        " "
        (propertize
            (concat "[" (propertize "C" 'face 'aria2-modeline-key-face) "]:clear finished")
            'local-map (make-mode-line-mouse-map 'down-mouse-1 #'aria2-clean-removed-download)
            'mouse-face 'aria2-modeline-mouse-face)
        " "
        (propertize
            (concat "[" (propertize "q" 'face 'aria2-modeline-key-face) "]:quit window")
            'local-map (make-mode-line-mouse-map 'down-mouse-1 #'quit-window)
            'mouse-face 'aria2-modeline-mouse-face)
        " "
        (propertize
            (concat "[" (propertize "Q" 'face 'aria2-modeline-key-face) "]:kill aria2")
            'local-map (make-mode-line-mouse-map 'down-mouse-1 #'aria2-terminate)
            'mouse-face 'aria2-modeline-mouse-face))
    "Custom mode-line-format for use with `aria2-mode'.")

;;; Major mode starts here

(defvar aria2-mode-map
    (let ((map (make-sparse-keymap)))
        (define-key map "j" 'next-line)
        (define-key map "n" 'next-line)
        (define-key map [down] 'next-line)
        (put 'next-line :advertised-binding "n")
        (define-key map "k" 'previous-line)
        (define-key map "p" 'previous-line)
        (define-key map [up] 'previous-line)
        (put 'previous-line :advertised-binding "p")
        (define-key map "=" 'aria2-move-up-in-list)
        (define-key map "+" 'aria2-move-up-in-list)
        (put 'aria2-move-up-in-list :advertised-binding "=")
        (define-key map "-" 'aria2-move-down-in-list)
        (define-key map "_" 'aria2-move-down-in-list)
        (put 'aria2-move-down-in-list :advertised-binding "-")
        (define-key map "g" 'revert-buffer)
        (put 'revert-buffer :advertised-binding "g")
        (define-key map "q" 'quit-window)
        (define-key map "Q" 'aria2-terminate)
        (define-key map "p" 'aria2-toggle-pause)
        (define-key map "f" 'aria2-add-file)
        (define-key map "u" 'aria2-add-uris)
        (define-key map "D" 'aria2-remove-download)
        (define-key map "C" 'aria2-clean-removed-download)
        map)
    "Keymap for `aria2-mode'.")

(define-derived-mode aria2-mode tabulated-list-mode "Aria2"
    "Mode for controlling aria2 downloader.
\\{aria2-mode-map}"
    :group 'aria2
    (unless (file-executable-p aria2-executable)
        (signal 'aria2-err-no-executable nil))
    (unless aria2--bin
        (condition-case nil
            (setq aria2--bin (eieio-persistent-read aria2--bin-file 'aria2-exe))
            (error (setq aria2--bin (make-instance 'aria2-exe
                                         "aria2-exe"
                                         :pid (or (aria2-find-pid) -1)
                                         :file aria2--bin-file)))))
    (add-hook 'kill-emacs-hook #'aria2-maybe-do-stuff-on-emacs-exit)
    (setq-local mode-line-format aria2-mode-line-format)
    (setq tabulated-list-format aria2--list-format)
    (setq tabulated-list-entries #'aria2--list-entries)
    (tabulated-list-init-header)
    (hl-line-mode 1)
    (tabulated-list-print)
    (aria2--start-timers))

;;;###autoload
(defun aria2-downloads-list ()
    "Display aria2 downloads list.  Enable `aria2-mode' to controll the process."
    (interactive)
    (with-current-buffer (pop-to-buffer aria2-list-buffer-name)
        (aria2-mode))
    (message
        (substitute-command-keys
            "Type \\<aria2-mode-map>\\[quit-window] to quit, \\[aria2-terminate] to kill aria, \\[describe-mode] for help")))

(provide 'aria2)

;; Local Variables:
;; coding: utf-8-unix
;; indent-tabs-mode: nil
;; End:

;;; aria2.el ends here
