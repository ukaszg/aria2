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

(require 'eieio-base)
(require 'subr-x)
(require 'tabulated-list)

(defgroup aria2 nil
    "An interface for aria2c command."
    :group 'external
    :group 'execute
    :prefix "aria2-")

(defcustom aria2-mode-hook nil
    "Hook ran afer enabling `aria2-mode'."
    :type 'hook
    :group 'aria2)

(require 'aria2-exe)
(require 'aria2-refresh)
(require 'aria2-faces)
(require 'aria2-tview)

(defvar aria2--debug nil
    "Should json commands and replies be printed.")

(defcustom aria2-add-evil-quirks (not (string-empty-p (locate-library "evil")))
    "If t adds aria2-mode to emacs states, and binds \C-w.")

;;;###autoload
(defun aria2-maybe-add-evil-quirks ()
    "Keep aria2-mode in EMACS state, as we already define j/k movement and add C-w * commands."
    (when aria2-add-evil-quirks
        (with-eval-after-load 'evil-states
            (add-to-list 'evil-emacs-state-modes 'aria2-mode)
            (add-to-list 'evil-emacs-state-modes 'aria2-dialog-mode))
        (with-eval-after-load 'evil-maps
            (define-key aria2-mode-map "\C-w" 'evil-window-map))))

(add-hook 'aria2-mode-hook #'aria2-maybe-add-evil-quirks)

;; Interactive commands start here

(defsubst aria2--is-paused-p ()
    (string= "paused" (car (elt (tabulated-list-get-entry) 1))))

(defun aria2-pause (all)
    "Pause download."
    (interactive "P")
    (Pause aria2c--bin (tabulated-list-get-id) all)
    (message "Pausing download. This may take a moment..."))

(defun aria2-resume (all)
    "Resume paused download."
    (interactive "P")
    (Unpause aria2c--bin (tabulated-list-get-id) all)
    (revert-buffer))

(defun aria2-toggle-pause (all)
    "Toggle 'paused' status for download."
    (interactive "P")
    (if (aria2--is-paused-p)
        (aria2-resume all)
        (aria2-pause all)))

(defconst aria2-supported-file-extension-regexp "\\.\\(?:meta\\(?:4\\|link\\)\\|torrent\\)$"
    "Regexp matching .torrent .meta4 and .metalink files.")

(defun aria2--supported-file-type-p (f)
    "Supported file predicate. Also allows directories to enable path navigation."
    (or (file-directory-p f)
        (string-match-p aria2-supported-file-extension-regexp f)))

;;;###autoload
(defun aria2-add-file (arg)
    "Prompt for a file and add it. Supports .torrent .meta4 and .metalink files."
    (interactive "P")
    (let ((chosen-file
              (expand-file-name
                  (read-file-name
                      "Choose .meta4, .metalink or .torrent file: "
                      default-directory nil nil nil 'aria2--supported-file-type-p))))
        (if (or (string-blank-p chosen-file)
                (not (file-exists-p chosen-file)))
            (message "No file selected.")
            (if (string-match-p "\\.torrent$" chosen-file)
                (Torrent aria2c--bin chosen-file)
                (Metalink aria2c--bin chosen-file))))
    (revert-buffer))

(defvar aria2-dialog-map
    (let ((map (make-sparse-keymap)))
        (set-keymap-parent map widget-keymap)
        (define-key map [mouse-1] 'widget-button-click)
        map))

(defvar aria2--url-list-widget nil)

(defconst aria2-supported-url-protocols-regexp "\\(?:ftp://\\|http\\(?:s?://\\)\\|magnet:\\)"
    "Regexp matching frp, http, https and magnet urls.")

(defconst aria2-url-list-buffer-name  "*aria2: Add http/https/ftp/magnet url(s)*"
    "Name of a buffer for inputting url's to download.")

;;;###autoload
(define-derived-mode aria2-dialog-mode fundamental-mode "Add urls"
    "Major mode for adding download urls.")

;;;###autoload
(defun aria2-add-uris ()
    "Display a form for inputting a list of http/https/ftp/magnet URLs."
    (interactive)
    (switch-to-buffer (get-buffer-create aria2-url-list-buffer-name))
    (kill-all-local-variables)
    (aria2-dialog-mode)
    (let ((inhibit-read-only t)) (erase-buffer))
    (remove-overlays)
    (widget-insert "Please input urls to download.\n\n")
    (widget-insert "Non \"magnet:\" urls must be mirrors pointing to the same file.\n\n")
    (setq aria2--url-list-widget
        (widget-create 'editable-list
            :entry-format "%i %d %v"
            :value '("")
            '(editable-field
                 :valid-regexp aria2-supported-url-protocols-regexp
                 :error "Url does not match supported type."
                 :value "")))
    (widget-insert "\n\n")
    (widget-create 'push-button
        :notify (lambda (&rest ignore)
                    (setq aria2--url-list-widget nil)
                    (switch-to-buffer aria2-list-buffer-name)
                    (kill-buffer aria2-url-list-buffer-name))
        "Cancel")
    (widget-insert "  ")
    (widget-create 'push-button
        :notify (lambda (&rest ignore)
                    (Uri aria2c--bin (widget-value aria2--url-list-widget))
                    (setq aria2--url-list-widget nil)
                    (switch-to-buffer aria2-list-buffer-name)
                    (kill-buffer aria2-url-list-buffer-name))
        "Download")
    (widget-insert "\n")
    (use-local-map aria2-dialog-map)
    (widget-setup)
    (goto-char (point-min))
    (widget-forward 3))

(defun aria2-remove-download (arg)
    "Set download status to 'removed'."
    (interactive "P")
    (when (y-or-n-p "Really remove download? ")
        (remove-download aria2c--bin (tabulated-list-get-id) (not (equal nil arg)))
        (tabulated-list-delete-entry)))

(defun aria2-clean-removed-download (arg)
    "Clean download with 'removed/completed/error' status.
With prefix remove all applicable downloads."
    (interactive "P")
    (if (equal nil arg)
        (progn
            (removeDownloadResult aria2c--bin (tabulated-list-get-id))
            (revert-buffer))
        (PurgeResult aria2c--bin)
        (revert-buffer)))

(defun aria2-move-up-in-list (arg)
    "Move item one row up, with prefix move to beginning of list."
    (interactive "P")
    (GidPosition aria2c--bin (tabulated-list-get-id) (if (equal nil arg) -1 0) (if (equal nil arg) "POS_CUR" "POS_SET"))
    (revert-buffer))

(defun aria2-move-down-in-list (arg)
    "Move item one row down, with prefix move to end of list."
    (interactive "P")
    (GidPosition aria2c--bin (tabulated-list-get-id) (if (equal nil arg) 1 0) (if (equal nil arg) "POS_CUR" "POS_END"))
    (revert-buffer))

(defun aria2-terminate ()
    "Stop aria2c process and kill buffers."
    (interactive)
    (when (y-or-n-p "Are you sure yo want to terminate aria2 process? ")
        (Shutdown! aria2c--bin)
        (kill-buffer aria2-list-buffer-name)
        (aria2--stop-timer)))

;; Mode line format starts here

(defvar aria2-mode-line-format
    (list
        (propertize "%b" 'face 'mode-line-buffer-id)
        " "
        (propertize
            (concat "[" (propertize "f" 'face 'aria2-modeline-key-face) "]:add file")
            'local-map (make-mode-line-mouse-map 'mouse-1 'aria2-add-file)
            'mouse-face 'aria2-modeline-mouse-face)
        " "
        (propertize
            (concat "[" (propertize "u" 'face 'aria2-modeline-key-face) "]:add url")
            'local-map (make-mode-line-mouse-map 'mouse1 'aria2-add-uris)
            'mouse-face 'aria2-modeline-mouse-face)
        " "
        (propertize
            (concat "[" (propertize "D" 'face 'aria2-modeline-key-face) "]:remove download")
            'local-map (make-mode-line-mouse-map 'mouse1 'aria2-remove-download)
            'mouse-face 'aria2-modeline-mouse-face)
        " "
        (propertize
            (concat "[" (propertize "C" 'face 'aria2-modeline-key-face) "]:clear finished")
            'local-map (make-mode-line-mouse-map 'mouse1 'aria2-clean-removed-download)
            'mouse-face 'aria2-modeline-mouse-face)
        " "
        (propertize
            (concat "[" (propertize "q" 'face 'aria2-modeline-key-face) "]:quit window")
            'local-map (make-mode-line-mouse-map 'mouse1 'aria2-quit)
            'mouse-face 'aria2-modeline-mouse-face)
        " "
        (propertize
            (concat "[" (propertize "Q" 'face 'aria2-modeline-key-face) "]:kill aria2")
            'local-map (make-mode-line-mouse-map 'mouse1 'aria2-terminate)
            'mouse-face 'aria2-modeline-mouse-face))
    "Custom mode-line for use with `aria2-mode'.")

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

(defun aria2-maybe-do-stuff-on-emacs-exit ()
    "Hook ran when quitting emacs."
    (when (and aria2c--bin (Running? aria2c--bin))
        (Save aria2c--bin)
        (when aria2c-kill-process-on-emacs-exit (Shutdown! aria2c--bin t))))

(add-hook 'aria2-mode-hook #'(lambda () (add-hook 'kill-emacs-hook #'aria2-maybe-do-stuff-on-emacs-exit)))
(add-hook 'aria2-mode-hook #'(lambda () (hl-line-mode 1)))

(define-derived-mode aria2-mode tabulated-list-mode "Aria2"
    "Mode for controlling aria2 downloader.
\\{aria2-mode-map}"
    :group 'aria2
    (unless (file-executable-p aria2c-executable)
        (signal 'aria2-err-no-executable nil))
    (unless aria2c--bin
        (condition-case nil
            (setq aria2c--bin (eieio-persistent-read aria2c--bin-file aria2c-exe))
            (error (setq aria2c--bin (make-instance aria2c-exe
                                       "aria2c-exe"
                                       :file aria2c--bin-file)))))

    (setq tabulated-list-format aria2--list-format)
    (tabulated-list-init-header)
    (setq tabulated-list-entries #'aria2--list-entries)
    (tabulated-list-print)
    (aria2--start-timers)
    (with-current-buffer aria2-list-buffer-name (setq-local mode-line-format aria2-mode-line-format))
    (run-hooks 'aria2-mode-hook))

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
