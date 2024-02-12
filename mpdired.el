(defcustom mpd-host (or (getenv "MPD_HOST") "localhost")
  "Host for MPD.")

(defcustom mpd-port (or (getenv "MPD_PORT") 6600)
  "Host for MPD.")

(defun my-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))
	(when (re-search-backward "^OK$" nil t)
	  (set-buffer-modified-p nil)
	  (message "Fini"))))))

(defun msg-me (process event)
  (princ
   (format "Process: %s had the event '%s'" process event)))

(defvar *mpdired-process* nil)

(defun mpdired-local-p (host)
  ;; Hack: if the `expand-file-name' of host leads to an existing
  ;; file, that should be our Unix socket.
  (file-exists-p (expand-file-name host)))

(defun mpc-connect ()
  (with-current-buffer (get-buffer-create "*mpc*")
    (erase-buffer)
    (let* ((localp (mpdired-local-p mpd-host))
	   (host (if localp (expand-file-name mpd-host) mpd-host)))
      ;; Create a new connection if needed
      (unless (and *mpdired-process*
		   (eq (process-status *mpdired-process*) 'open))
	(setq *mpdired-process* (make-network-process :name "mpdired"
						      :buffer (current-buffer)
						      :host host
						      :service (if localp host mpd-port)
						      :family (if localp 'local)
						      :coding 'utf-8
						      :filter 'my-filter
						      :sentinel 'msg-me)))
      ;;(process-send-string proc "list album\n")
      ;;(process-send-string proc "playlistinfo\n")
      (process-send-string *mpdired-process* "listall \"\"\n")
      ;;(process-send-string proc "close\n")
      ;;(buffer-string)
      )))
