;;; aria2-exe.el --- aria2c json interface. -*- lexical-binding: t; -*-

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
(require 'json)
(require 'url)
(require 'subr-x)

(defgroup aria2c nil
    "An interface for aria2c command."
    :group 'external
    :group 'aria2
    :prefix "aria2c-")

(defcustom aria2c-kill-process-on-emacs-exit nil
    "Whether aria2c should be stopped when exiting Emacs.
If nil Emacs will reattach itself to the process on entering downloads list."
    :type 'boolean
    :group 'aria2c)

(defcustom aria2c-executable (executable-find "aria2c")
    "Full path to aria2c binary."
    :type 'file
    :group 'aria2c)

(defcustom aria2c-session-file (expand-file-name "aria2c.session" user-emacs-directory)
    "Name of session file.  Will be used with \"--save-session\" and \"--input-file\" options."
    :type 'file
    :group 'aria2c)

(defcustom aria2c-download-directory (or (getenv "XDG_DOWNLOAD_DIR") (expand-file-name "~/"))
    "Default directory to store downloaded files."
    :type 'directory
    :group 'aria2c)

(defcustom aria2c-rcp-listen-port 6800
    "Port on which JSON RCP server will listen."
    :type '(integer :tag "Http port")
    :group 'aria2c)

(defcustom aria2c-rcp-secret (or (let ((uuidgen (executable-find "uuidgen")))
                                     (and uuidgen (string-trim (shell-command-to-string uuidgen))))
                                 (sha1 (format "%s%s%s%s%s%s%s%s%s" (user-uid) (emacs-pid) (system-name)
                                           (user-full-name) (current-time) (emacs-uptime) (buffer-string)
                                           (random) (recent-keys))))
    "Secret value used for authentication with the aria2c process, for use with --rpc-secret= switch."
    :type '(string :tag "Secret")
    :group 'aria2c)

(defcustom aria2c-custom-args nil
    "Additional arguments for aria2c.  This should be a list of strings.  See aria2c manual for supported options."
    :type '(repeat (string :tag "Commandline argument."))
    :group 'aria2c)

(defun aria2c--base64-encode-file (path)
    "Return PATH contents as base64-encoded string."
    (unless (file-exists-p path)
        (signal 'aria2c-err-file-doesnt-exist '(path)))
    (with-current-buffer (find-file-noselect path)
        (unwind-protect
            (base64-encode-string (buffer-string) t)
            (kill-buffer))))

;;; Error definitions start here

(define-error 'aria2c-err-too-many-magnet-urls  "Only one magnet link per download is allowed" 'user-error)
(define-error 'aria2c-err-file-doesnt-exist     "File doesn't exist"                           'user-error)
(define-error 'aria2c-err-not-a-torrent-file    "This is not a .torrent file"                  'user-error)
(define-error 'aria2c-err-not-a-metalink-file   "This is not a .metalink file"                 'user-error)
(define-error 'aria2c-err-failed-to-start       "Failed to start"                              'error)
(define-error 'aria2c-err-no-executable         "Couldn't find `aria2c' executable, aborting"  'error)
(define-error 'aria2c-err-no-such-position-type "Wrong position type"                          'error)

(defconst aria2c--codes-to-errors-alist
    (list
        (cons "0" "All downloads were successful")
        (cons "1" "An unknown error occurred")
        (cons "2" "Time out occurred")
        (cons "3" "A resource was not found")
        (cons "4" "Aria2 saw the specified number of \"resource not found\" error. See --max-file-not-found option")
        (cons "5" "A download aborted because download speed was too slow. See --lowest-speed-limit option")
        (cons "6" "Network problem occurred")
        (cons "7" "There were unfinished downloads")
        (cons "8" "Remote server did not support resume when resume was required to complete download")
        (cons "9" "There was not enough disk space available")
        (cons "10" "Piece length was different from one in .aria2 control file. See --allow-piece-length-change option")
        (cons "11" "Aria2 was downloading same file at that moment")
        (cons "12" "Aria2 was downloading same info hash torrent at that moment")
        (cons "13" "File already existed. See --allow-overwrite option")
        (cons "14" "Renaming file failed. See --auto-file-renaming option")
        (cons "15" "Aria2 could not open existing file")
        (cons "16" "Aria2 could not create new file or truncate existing file")
        (cons "17" "File I/O error occurred")
        (cons "18" "Aria2 could not create directory")
        (cons "19" "Name resolution failed")
        (cons "20" "Aria2 could not parse Metalink document")
        (cons "21" "FTP command failed")
        (cons "22" "HTTP response header was bad or unexpected")
        (cons "23" "Too many redirects occurred")
        (cons "24" "HTTP authorization failed")
        (cons "25" "Aria2 could not parse bencoded file (usually \".torrent\" file)")
        (cons "26" "A \".torrent\" file was corrupted or missing information that aria2 needed")
        (cons "27" "Magnet URI was bad")
        (cons "28" "Bad/unrecognized option was given or unexpected option argument was given")
        (cons "29" "The remote server was unable to handle the request due to a temporary overloading or maintenance")
        (cons "30" "Aria2 could not parse JSON-RPC request"))
    "Mapping of aria2 error codes to error messages.")

(defsubst aria2c--decode-error (err)
    (or (cdr-safe (assoc-string err aria2c--codes-to-errors-alist nil))
        "Unknown/other error"))

(defsubst aria2c-start-cmd ()
    (delq nil
        (append aria2c-custom-args
            `("-D" ;; Start in daemon mode (won't be managed by Emacs).
                 "--enable-rpc=true"
                 ,(format "--rpc-secret=%s" (if aria2c--bin (oref aria2c--bin secret) aria2c-rcp-secret))
                 ,(format "--rpc-listen-port=%s" aria2c-rcp-listen-port)
                 ,(format "--dir=%s" aria2c-download-directory)
                 ,(format "--save-session=%s" aria2c-session-file)
                 ,(when (file-exists-p aria2c-session-file)
                      (format "--input-file=%s" aria2c-session-file))))))

;;;###autoload
(defun aria2c-print-daemon-commandline ()
    "Prints full commandline for aria2c."
    (interactive)
    (let ((options (aria2c-start-cmd)))
        (message "# %s %s" aria2c-executable (string-join options " "))))

;;; Aria2c process controller starts here.

(defclass aria2c-exe (eieio-persistent)
    ((request-id :initarg :request-id
         :initform 0
         :type integer
         :docstring "Value of id field in JSONRPC data, gets incremented for each request.")
        (secret :initarg :secret
            :initform (concat aria2c-rcp-secret)
            :type string
            :docstring "Secret value used for authentication with the aria2c process, for use with --rpc-secret= switch.")
        (pid :initarg :pid
            :initform -1
            :type integer
            :docstring "PID of the aria2c process, or -1 if process isn't running."))
    :docstring "This takes care of starting/stopping aria2c process and provides methods for each remote command.")

(defun aria2c--is-aria-process-p (pid)
    "Returns t if process identified by PID is aria."
    (let ((proc-attr (process-attributes pid)))
        (and
            (string= "aria2c" (alist-get 'comm proc-attr))
            (string= (user-real-login-name) (alist-get 'user proc-attr)))))

(defsubst aria2c-find-pid () (car (cl-remove-if-not #'aria2c--is-aria-process-p (list-system-processes))))

(defvar aria2c--bin nil "aria2c process instance.")

(defconst aria2c--bin-file
    (expand-file-name "aria2c-controller.eieio" user-emacs-directory)
    "File used to persist controller status between Emacs restarts.")

(defmethod Running? ((this aria2c-exe))
    "Returns status of managed (we remember the PID) aria2c process."
    (with-slots (pid) this
        (and (< 0 pid) (aria2c--is-aria-process-p pid))))

(defmethod Save ((this aria2c-exe))
    "Persist controller settings."
    (if (Running? this)
        (eieio-persistent-save this aria2c--bin-file)
        (message "%s is not running." aria2c-executable)))

(defmethod Load ((this aria2c-exe))
    "Restore controller settings.  "
    (if (and (not (Running? this))
            (file-readable-p aria2c--bin-file))
        (setq aria2c--bin
            (condition-case nil
                (eieio-persistent-read aria2c--bin-file aria2c-exe)
                (error (make-instance aria2c-exe "aria2c-exe"
                           :pid (or (aria2c-find-pid) -1)
                           :file aria2c--bin-file))))
        (message "%s is running." aria2c-executable)))

(defmethod Start ((this aria2c-exe))
    "Starts aria2c process."
    (unless (Running? this)
        (let ((options (aria2c-start-cmd)))
            (when aria2--debug
                (message "Starting process: %s %s" aria2c-executable (string-join options " ")))
            (apply #'start-process aria2c-executable nil aria2c-executable options)
            (sleep-for 1)
            ;; aria2 in daemon mode forks to the background, so we search system-processes
            (oset this pid (or (aria2c-find-pid) -1))
            (unless (Running? this)
                (signal 'aria2c-err-failed-to-start (concat aria2c-executable " " (string-join options " ")))))))

(defmethod Post ((this aria2c-exe) method &rest params)
    "Calls a remote METHOD with PARAMS. Returns response alist."
    (Start this)
    (let (
             (url-request-method "POST")
             (url-request-data (json-encode-alist
                                   (list
                                       (cons "jsonrpc" 2.0)
                                       (cons "id" (let ((id (1+ (oref this request-id))))
                                                      (setq id (if (equal id most-positive-fixnum) 0 id))
                                                      (oset this request-id id)
                                                      id))
                                       (cons "method"  method)
                                       (cons "params" (vconcat
                                                          `(,(format "token:%s" (oref this secret)))
                                                          (delq nil params))))))
             (url-request-extra-headers '(("Content-Type" . "application/json")))
             url-history-track
             json-response)
        (when aria2--debug (message "SEND: %s" url-request-data))
        (with-current-buffer
            (url-retrieve-synchronously (format "http://localhost:%d/jsonrpc" aria2c-rcp-listen-port) t)
            ;; expect unicode response
            (set-buffer-multibyte t)
            ;; read last line, where json response is
            (goto-char (point-max))
            (beginning-of-line)
            (setq json-response (json-read))
            (kill-buffer))
        (when aria2--debug (message "RECV: %s" json-response))
        (or (alist-get 'result json-response)
            (error "ERROR: %s" (alist-get 'message (alist-get 'error json-response))))))

(defmethod Uri ((this aria2c-exe) urls)
    "Add a list of http/ftp/bittorrent URLS, pointing at the same file.
When sending magnet link, URLS must have only one element."
    (Post this "aria2.addUri" (vconcat urls)))

(defmethod Torrent ((this aria2c-exe) path)
    "Add PATH pointing at a torrent file to download list."
    (unless (file-exists-p (setq path (expand-file-name (string-trim path))))
        (signal 'aria2c-err-file-doesnt-exist `(,path)))
    (unless (string-match-p "\\.torrent$" path)
        (signal 'aria2c-err-not-a-torrent-file nil))
    (Post this "aria2.addTorrent" (aria2c--base64-encode-file path)))

(defmethod Metalink ((this aria2c-exe) path)
    "Add local .metalink PATH to download list."
    (unless (file-exists-p (setq path (expand-file-name (string-trim path))))
        (signal 'aria2c-err-file-doesnt-exist `(,path)))
    (unless (string-match-p "\\.meta\\(?:4\\|link\\)$" path)
        (signal 'aria2c-err-not-a-metalink-file nil))
    (Post this "aria2.addMetalink" (aria2c--base64-encode-file path)))

(defmethod Shutdown! ((this aria2c-exe) &optional force)
    "Shut down aria2c process.  If FORCE don't wait for unregistering torrents."
    (when (Running? this)
        (Post this (if force "aria2.forceShutdown" "aria2.shutdown"))
        (sleep-for 1)
        (oset this pid -1)))

(defmethod Cancel ((this aria2c-exe) gid &optional force)
    "Remove download identified by GID. If FORCE don't unregister download at bittorrent tracker."
    (Post this (if force "aria2.forceRemove" "aria2.remove") gid))

(defmethod Pause ((this aria2c-exe) gid &optional force)
    "Pause download identified by GID or all if t. If FORCE don't unregister download at bittorrent tracker."
    (Post this (concat (if force "aria2.forcePause" "aria2.pause") (if (eq t gid) "All" "")) (when (eq t gid) gid)))

(defmethod Unpause ((this aria2c-exe) gid)
    "Unpause download identified by GID or all if t."
    (if (eq t gid)
        (Post this "aria2.unpause" gid)
        (Post this "aria2.unpauseAll")))

(defmethod GidUris ((this aria2c-exe) gid)
    "Return a list of uris used in download identified by GID."
    (Post this "aria2.getUris" gid))

(defmethod GidFiles ((this aria2c-exe) gid)
    "Return a file list of a download identified by GID."
    (Post this "aria2.getFiles" gid))

(defmethod GidPeers ((this aria2c-exe) gid)
    "Return a list peers of the download denoted by GID."
    (Post this "aria2.getPeers" gid))

(defmethod GidServers ((this aria2c-exe) gid)
    "Return currently connected HTTP(S)/FTP servers of the download denoted by GID."
    (Post this "aria2.getServers" gid))

(defmethod GidStatus ((this aria2c-exe) gid &optional keys)
    "Return status of a download identified by GID."
    (Post this "aria2.tellStatus" gid keys))

(defmethod Active ((this aria2c-exe) &optional keys)
    "Return statuses of active downloads."
    (Post this "aria2.tellActive" keys))

(defmethod Waiting ((this aria2c-exe) &optional offset num keys)
    "Return statuses of waiting downloads."
    (Post this "aria2.tellWaiting" (or offset 0) (or num most-positive-fixnum) keys))

(defmethod Stopped ((this aria2c-exe) &optional offset num keys)
    "Return statuses of stopped downloads."
    (Post this "aria2.tellStopped" (or offset 0) (or num most-positive-fixnum) keys))

(defmethod GidPosition ((this aria2c-exe) gid pos &optional how)
    "Change position of a download denoted by GID. POS is a number. HOW is one of:
\"POS_SET\" - sets file to POS position from the beginning of a list (first element is 0),
\"POS_CUR\" - moves file by POS places relative to it's current position,
\"POS_END\" - sets file to POS position relative to end of list.
If nil defaults to \"POS_CUR\"."
    (unless (or (null how) (member how '("POS_SET" "POS_CUR" "POS_END")))
        (signal 'aria2c-err-no-such-position-type (list how)))
    (Post this "aria2.changePosition" gid pos (or how "POS_CUR")))

(defmethod GidUri ((this aria2c-exe) gid file-index del-uris add-uris &optional position)
    "This method removes the URIs in DEL-URIS list and appends the URIs in ADD-URIS list to download denoted by GID.
FILE-INDEX is 1-based position, identifying a file in a download.
POSITION is a 0-based index specifying where URIs are inserted in waiting list.
Returns a pair of numbers denoting amount of files deleted and files inserted."
    (Post this "aria2.changeUri" gid file-index del-uris add-uris (or position 0)))

(defmethod Option ((this aria2c-exe) gid &optional options)
    "This method returns or sets options of the download denoted by GID."
    (if options (Post this "aria2.changeOption" gid options)
        (Post this "aria2.getOption" gid)))

(defmethod GlobalOption ((this aria2c-exe) &optional options)
    "Return an alist of global options or set OPTIONS. Global options are used as defaults for newly added files."
    (if options
        (Post this "aria2.changeGlobalOption" options)
        (Post this "aria2.getGlobalOptions")))

(defmethod GlobalStats ((this aria2c-exe))
    "Returns global statistics such as the overall download and upload speeds."
    (Post this "aria2.getGlobalStat"))

(defmethod PurgeResult ((this aria2c-exe))
    "This method purges completed/error/removed downloads to free memory."
    (Post this "aria2.purgeDownloadResult"))

(defmethod RemoveResult ((this aria2c-exe) gid)
    "Removes a completed/error/removed download denoted by GID from memory."
    (Post this "aria2.removeDownloadResult" gid))

(defmethod SaveSession ((this aria2c-exe))
    "Saves the current session to a `aria2c-session-file' file."
    (Post this "aria2.saveSession"))


(provide 'aria2-exe)

;; Local Variables:
;; coding: utf-8-unix
;; indent-tabs-mode: nil
;; End:

;;; aria2-exe.el ends here
