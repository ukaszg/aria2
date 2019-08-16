;;; aria2-tview.el --- Table interface -*- lexical-binding: t; -*-

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

(defcustom aria2-list-buffer-name "*aria2: downloads list*"
    "Name of buffer to use when showing downloads list."
    :type '(string :tag "Buffer name")
    :group 'aria2)

(defconst aria2--list-format
    [
        ("File"     40 t)   ("Status"    7 t)
        ("Type"     13 t)   ("Done"      4 t)
        ("Download" 12 t)   ("Upload"   12 t)
        ("Size"     10 nil) ("Error"     0 nil)]
    "Format for downloads list columns.")

(defconst aria2--tell-keys
    ["gid" "status" "totalLength" "completedLength" "downloadSpeed"
        "uploadSpeed" "files" "dir" "bittorrent" "errorCode"]
    "Default list of keys for use in aria2.tell* calls.")

(defsubst aria2--list-entries-File (e)
    (let ((bt (alist-get 'bittorrent e)))
        (or (and bt (alist-get 'name (alist-get 'info bt)))
            (let ((uris (alist-get 'uris (elt (alist-get 'files e) 0))))
                (and (< 0 (length uris)) (file-name-nondirectory (alist-get 'uri (elt uris 0)))))
            "unknown")))

(defsubst aria2--list-entries-Status (e)
    (alist-get 'status e))

(defsubst aria2--list-entries-Type (e)
    (or (and (alist-get 'bittorrent e) "bittorrent")
        (let ((uris (alist-get 'uris (elt (alist-get 'files e) 0))))
            (and (< 0 (length uris)) (car-safe (split-string (alist-get 'uri (elt uris 0)) ":"))))
        "unknown"))

(defsubst aria2--list-entries-Done (e)
    (let ((total (float (string-to-number (alist-get 'totalLength e))))
             (completed (float (string-to-number (alist-get 'completedLength e)))))
        (if (>= 0 total)
            "-"
            (format "%d%%" (* 100.0 (/ completed total))))))

(defsubst aria2--list-entries-Download (e)
    (format "%.2f kB" (/ (string-to-number (alist-get 'downloadSpeed e)) 1024)))

(defsubst aria2--list-entries-Upload (e)
    (format "%.2f kB" (/ (string-to-number (alist-get 'uploadSpeed e)) 1024)))

(defsubst aria2--list-entries-Size (e)
    (let ((size (string-to-number (alist-get 'totalLength e))))
        (if (< size 1024)
            (format "%.2f B" size)
            (setq size (/ size 1024))
            (if (< size 1024)
                (format "%.2f kB" size)
                (setq size (/ size 1024))
                (if (< size 1024)
                    (format "%.2f MB" size)
                    (setq size (/ size 1024))
                    (if (< size 1024)
                        (format "%.2f GB" size)
                        (format "%2.f TB" (/ size 1024))))))))

(defsubst aria2--list-entries-Err (e)
    (let ((err (alist-get 'errorCode e)))
        (or (and err (aria2c--decode-error err))
            " - ")))

(defun aria2--list-entries ()
    "Return entries to be displayed in downloads list."
    (let (entries
             (info (append
                       (Active  aria2c--bin aria2--tell-keys)
                       (Waiting aria2c--bin nil nil aria2--tell-keys)
                       (Stopped aria2c--bin nil nil aria2--tell-keys)
                       nil)))
        (dolist (e info entries)
            (push (list
                      (alist-get 'gid e)
                      (vector
                          (list (aria2--list-entries-File e)     'face 'aria2-file-face)
                          (list (aria2--list-entries-Status e)   'face 'aria2-status-face)
                          (list (aria2--list-entries-Type e)     'face 'aria2-type-face)
                          (list (aria2--list-entries-Done e)     'face 'aria2-done-face)
                          (list (aria2--list-entries-Download e) 'face 'aria2-download-face)
                          (list (aria2--list-entries-Upload e)   'face 'aria2-upload-face)
                          (aria2--list-entries-Size e)
                          (list (aria2--list-entries-Err e)      'face 'aria2-error-face)))
                entries))))


(provide 'aria2-tview)

;; Local Variables:
;; coding: utf-8-unix
;; indent-tabs-mode: nil
;; End:

;;; aria2-tview.el ends here
