;;; aria2-dialog.el --- Control aria2 commandline tool from Emacs -*- lexical-binding: t; -*-

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

(defconst aria2-supported-url-protocols-regexp "\\(?:ftp://\\|http\\(?:s?://\\)\\|magnet:\\)"
    "Regexp matching frp, http, https and magnet urls.")

(defconst aria2-url-list-buffer-name  "*aria2: Add http/https/ftp/magnet url(s)*"
    "Name of a buffer for inputting url's to download.")

(defconst aria2-supported-file-extension-regexp "\\.\\(?:meta\\(?:4\\|link\\)\\|torrent\\)$"
    "Regexp matching .torrent .meta4 and .metalink files.")

(defvar aria2--url-list-widget nil)

(defvar aria2-dialog-map
    (let ((map (make-sparse-keymap)))
        (set-keymap-parent map widget-keymap)
        (define-key map [mouse-1] 'widget-button-click)
        map))

(defun aria2--supported-file-type-or-dir-p (f)
    "Supported file predicate. Also allows directories to enable path navigation."
    (or (file-directory-p f)
        (string-match-p aria2-supported-file-extension-regexp f)))

(define-derived-mode aria2-dialog-mode fundamental-mode "Add urls"
    "Major mode for adding download urls.")

;; Interactive commands start here

;;;###autoload
(defun aria2-add-uris ()
    "Display a form for inputting a list of http/https/ftp/magnet URLs."
    (interactive)
    (switch-to-buffer (get-buffer-create aria2-url-list-buffer-name))
    (kill-all-local-variables)
    (aria2-dialog-mode)
    (let ((inhibit-read-only t))
        (erase-buffer))
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
        :notify (lambda (&rest _)
                    (setq aria2--url-list-widget nil)
                    (kill-buffer aria2-url-list-buffer-name)
                    (switch-to-buffer aria2-list-buffer-name))
        "Cancel")
    (widget-insert "  ")
    (widget-create 'push-button
        :notify (lambda (&rest _)
                    (Uri aria2--bin (widget-value aria2--url-list-widget))
                    (setq aria2--url-list-widget nil)
                    (kill-buffer aria2-url-list-buffer-name)
                    (switch-to-buffer aria2-list-buffer-name)
                    (revert-buffer))
        "Download")
    (widget-insert "\n")
    (use-local-map aria2-dialog-map)
    (widget-setup)
    (goto-char (point-min))
    (widget-forward 3))

;;;###autoload
(defun aria2-add-file ()
    "Prompt for a file and add it. Supports .torrent .meta4 and .metalink files."
    (interactive)
    (let ((chosen-file
              (expand-file-name
                  (read-file-name
                      "Choose .meta4, .metalink or .torrent file: "
                      default-directory nil nil nil 'aria2--supported-file-type-or-dir-p))))
        (if (or (string-blank-p chosen-file)
                (not (file-exists-p chosen-file)))
            (message "No file selected.")
            (if (string-match-p "\\.torrent$" chosen-file)
                (Torrent aria2--bin chosen-file)
                (Metalink aria2--bin chosen-file))))
    (aria2--refresh))

(provide 'aria2-dialog)

;; Local Variables:
;; coding: utf-8-unix
;; indent-tabs-mode: nil
;; End:

;;; aria2-dialog.el ends here
