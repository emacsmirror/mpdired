(defcustom mpdired-host (or (getenv "MPD_HOST") "localhost")
  "Host for MPD.")

(defcustom mpdired-port (or (getenv "MPD_PORT") 6600)
  "Host for MPD.")

(defun mpdired--subdir-p (dir-a dir-b)
  (let ((pos (string-search dir-a dir-b)))
    (and pos (zerop pos))))

(defvar mpdired--parse-end nil)

(defun mpdired--parse-listall-1 (current accum)
  ;; Recursively rebuild the directory hierarchy from a "listall"
  ;; command into a list.  In the output, a directory is list which
  ;; `car' is its name and its `cdr' is the files or other directory
  ;; it contains.
  (catch 'exit
    (while (not (or mpdired--parse-end
		    (setq mpdired--parse-end
			  (re-search-forward "^OK$" (line-end-position) t 1))))
      ;; Look for file or directory line by line (I'm not interested
      ;; in playlist)
      (re-search-forward "^\\(file\\|directory\\): \\(.*\\)$" (line-end-position) t 1)
      (let ((type (match-string 1))
	    (new (match-string 2)))
	(cond ((string= "file" type) (push new accum))
	      ((string= "directory" type)
	       ;; This new directory is either a subdir of the current
	       ;; one or a new directory of the same level of the
	       ;; current one.  In this last case we need to go one
	       ;; line backward (because we will go forward later) and
	       ;; quit the loop.
	       (cond ((mpdired--subdir-p current new)
		      (forward-line)
		      (push (mpdired--parse-listall-1 new (list new)) accum))
		     (t (forward-line -1)
			(throw 'exit t))))))
      (forward-line)))
  (reverse accum))

(defun mpdired--parse-listall ()
  ;; Called from *mpdired-work*
  (goto-char (point-min))
  (setq mpdired--parse-end nil)
  ;; XXX Empty string is the directory name of the toplevel directory.
  (mpdired--parse-listall-1 "" (list "")))

(defun mpdired-present-listall (contact)
  ;; Called from *mpdired-work*
  (let ((out (get-buffer-create (format "*MPDired (%s:%d)*"
					(car contact) (cadr contact))))
	(content (mpdired--parse-listall)))
    (with-current-buffer out
      (erase-buffer)
      (save-excursion
	(let* ((content (if (cddr content) content (cadr content)))
	       (top (if (string= (car content) "")
			"*toplevel*"
		      (car content))))
	  (insert (propertize top 'face 'bold) ":\n")
	  (dolist (e (cdr content))
	    (cond ((stringp e) (insert e))
		  ((consp e) (insert (propertize (car e) 'face 'dired-directory))))
	    (insert "\n")))))))

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
	  (when (eq mpdired--last-command 'listall)
	    (mpdired-present-listall (process-contact proc)))
	  (set-buffer-modified-p nil))))))

(defun msg-me (process event)
  (unless (string-search "connection broken" event)
    (message "Process: %s had the event '%s'" process event)))

(defvar mpdired-process nil)

(defun mpdired-local-p (host)
  ;; Hack: if the `expand-file-name' of host leads to an existing
  ;; file, that should be our Unix socket.
  (file-exists-p (expand-file-name host)))

(defun mpdired--maybe-init ()
  (with-current-buffer (get-buffer-create "*mpdired-work*")
    (setq-local buffer-read-only nil)
    (erase-buffer)
    ;; Always reparse host should the user have changed it.
    (let* ((localp (mpdired-local-p mpdired-host))
	   (host (if localp (expand-file-name mpdired-host) mpdired-host)))
      ;; Create a new connection if needed
      (unless (and mpdired-process
		   (eq (process-status mpdired-process) 'open))
	(setq mpdired-process (make-network-process :name "mpdired"
						    :buffer (current-buffer)
						    :host host
						    :service (if localp host mpdired-port)
						    :family (if localp 'local)
						    :coding 'utf-8
						    :filter 'my-filter
						    :sentinel 'msg-me))))))

(defun mpdired-listall (path)
  (mpdired--maybe-init)
  (with-current-buffer "*mpdired-work*"
    (setq-local mpdired--last-command 'listall)
    (process-send-string mpdired-process (format "listall \"%s\"\n" path))))

(defun mpdired-test-me ()
  (interactive)
  (mpdired-listall "")
  ;;(mpdired-listall "Aftermath")
  ;;(mpdired-listall "Arcade Fire")
  ;;(mpdired-listall "Arcade Fire/Funeral")
  )
