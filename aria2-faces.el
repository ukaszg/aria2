;;; aria2-faces.el --- Table interface -*- lexical-binding: t; -*-

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

;;; Faces definitions start here.

(defface aria2-file-face `((t :inherit mode-line-buffer-id))
    "Face for download name."
    :group 'aria2
    :group 'face)

(defface aria2-status-face `((t :inherit font-lock-constant-face))
    "Face for status."
    :group 'aria2
    :group 'face)

(defface aria2-type-face `((t :inherit font-lock-builtin-face))
    "Face for download type."
    :group 'aria2
    :group 'face)

(defface aria2-done-face `((t :inherit font-lock-doc-face))
    "Face for % done."
    :group 'aria2
    :group 'face)

(defface aria2-download-face `((t :inherit font-lock-string-face))
    "Face for download speed."
    :group 'aria2
    :group 'face)

(defface aria2-upload-face `((t :inherit font-lock-comment-face))
    "Face for upload speed."
    :group 'aria2
    :group 'face)

(defface aria2-error-face `((t :inherit font-lock-warning-face))
    "Face for error messages."
    :group 'aria2
    :group 'face)

(defface aria2-modeline-key-face `((t :inherit font-lock-warning-face))
    "Face for shortcut hints displayed in mode-line."
    :group 'aria2
    :group 'face)

(defface aria2-modeline-mouse-face `((t :inherit default
                                         :box (:line-width 2 :style pressed-button)
                                         :inverse-video t
                                         :weight bold))
    "Face for shortcuts hoovered by a pointer."
    :group 'aria2
    :group 'face)

(provide 'aria2-faces)

;; Local Variables:
;; coding: utf-8-unix
;; indent-tabs-mode: nil
;; End:

;;; aria2-faces.el ends here
