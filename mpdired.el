(defcustom mpdired-host (or (getenv "MPD_HOST") "localhost")
  "Host for MPD.")

(defcustom mpdired-port (or (getenv "MPD_PORT") 6600)
  "Host for MPD.")

(defvar-keymap mpdired-browse-mode-map
  :doc "Local keymap for MPDired browser."
  "n"   'mpdired-next-line
  "p"   'mpdired-previous-line
  "q"   'bury-buffer
  "C-m" 'mpdired-listall-at-point
  "^"   'mpdired-goto-parent)

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
  ;; It have the good property of being a prefix of any string.
  (mpdired--parse-listall-1 "" (list "")))

(defun mpdired-browse-mode ()
  "Major mode for MPDired browser."
  (kill-all-local-variables)
  (use-local-map mpdired-browse-mode-map)
  (set-buffer-modified-p nil)
  (setq major-mode 'mpdired-browse-mode
	mode-name "MPDired Browse"
	buffer-read-only t))

(defun mpdired--hostname (host service localp)
  (if localp
      (format "%s" host)
    (format "%s:%s" host service)))

(defun mpdired--comm-name (host service localp)
  (format "*mpdired-%s*" (mpdired--hostname host service localp)))

(defun mpdired--browser-name (host service localp)
  (format "*MPDired Browser (%s)*" (mpdired--hostname host service localp)))

(defvar-local mpdired--directory nil)
(defvar-local mpdired--last-command nil)
(defvar-local mpdired--previous-directory nil)

(defun mpdired--insert-file/dir (element)
  (cond ((stringp element)
	 (insert element)
	 (put-text-property (line-beginning-position) (line-end-position) 'type 'file))
	((consp element)
	 (insert (propertize (car element) 'face 'dired-directory))
	 (put-text-property (line-beginning-position) (line-end-position) 'type 'directory))))

(defun mpdired--present-listall (proc)
  ;; Called from *mpdired-work*
  (let* ((peer-info (process-contact proc t))
	 (peer-host (plist-get peer-info :host))
	 (peer-service (plist-get peer-info :service))
	 (peer-localp (eq (plist-get peer-info :family) 'local))
	 (from-directory (with-current-buffer (process-buffer proc)
			   mpdired--previous-directory))
	 (buffer-name (mpdired--browser-name peer-host peer-service peer-localp))
	 (content (mpdired--parse-listall)))
    (with-current-buffer (get-buffer-create buffer-name)
      (let ((inhibit-read-only t))
	(erase-buffer)
	;; `content' is always of the form ("" rest...) so if there
	;; is only one "rest" use it as content.
	(let* ((content (if (cddr content) content (cadr content)))
	       (top (unless (string= "" (car content)) (car content)))
	       (data (cdr content)))
	  ;; Insert the content
	  (save-excursion
	    (if top (insert (propertize top 'face 'bold) ":\n"))
	    (dolist (e (butlast data))
	      (mpdired--insert-file/dir e)
	      (insert "\n"))
	    (mpdired--insert-file/dir (car (last data))))
	  ;; Go to the previous directory line
	  (cond (from-directory
		 (goto-char (point-min))
		 (re-search-forward from-directory nil t)
		 (goto-char (line-beginning-position)))
		(t
		 (goto-char (point-min))
		 (if top (mpdired-next-line))))
	  ;; Set mode and memorize directory
	  (mpdired-browse-mode)
	  (setq-local mpdired--directory (when top top)))))))

(defun mpdired--filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
	(save-excursion
	  ;; Insert the text, advancing the process marker.
	  (goto-char (process-mark proc))
	  (insert string)
	  (set-marker (process-mark proc) (point)))
	(if moving (goto-char (process-mark proc)))
	;; The server has done its work.
	(when (re-search-backward "^OK$" nil t)
	  (when (eq mpdired--last-command 'listall)
	    (mpdired--present-listall proc)))))))

(defun mpdired--sentinel (process event)
  ;; Do not signal a closed connection
  (unless (string-search "connection broken" event)
    (message "Process: %s had the event '%s'" process event)))

(defun mpdired--local-p (host)
  ;; Hack: if the `expand-file-name' of host leads to an existing
  ;; file, that should be our Unix socket.
  (file-exists-p (expand-file-name host)))

(defun mpdired--maybe-init (host service localp)
  (with-current-buffer (get-buffer-create (mpdired--comm-name host service localp))
    (setq-local buffer-read-only nil)
    (erase-buffer)
    (let ((process (get-buffer-process (current-buffer))))
      ;; Create a new connection if needed
      (unless (and process
		   (eq (process-status process) 'open))
	(set-process-buffer (make-network-process :name "mpdired"
						  :buffer (current-buffer)
						  :host host
						  :service service
						  :family (if localp 'local)
						  :coding 'utf-8
						  :filter 'mpdired--filter
						  :sentinel 'mpdired--sentinel)
			    (current-buffer))))))

(defun mpdired-listall (path &optional from)
  ;; Always reparse host should the user have changed it.
  (let* ((localp (mpdired--local-p mpdired-host))
	 (host (if localp (expand-file-name mpdired-host) mpdired-host))
	 (service (if localp host mpdired-port)))
    (mpdired--maybe-init host service localp)
    (with-current-buffer (mpdired--comm-name host service localp)
      (let ((process (get-buffer-process (current-buffer))))
	(when (process-live-p process)
	  (setq-local mpdired--last-command 'listall
		      mpdired--previous-directory (when from from))
	  (process-send-string process (format "listall \"%s\"\n" path)))))))

(defun mpdired-next-line ()
  (interactive)
  (next-line)
  (goto-char (line-beginning-position)))

(defun mpdired-previous-line ()
  (interactive)
  (previous-line)
  (goto-char (line-beginning-position)))

(defun mpdired-listall-at-point ()
  (interactive)
  (goto-char (line-beginning-position))
  (save-excursion
    (re-search-forward "^\\(.*\\)$" (line-end-position) t))
  (if (eq (get-text-property (line-beginning-position) 'type) 'directory)
      (mpdired-listall (match-string 1) mpdired--directory)
    (message "Cannot browse a file.")))

(defun mpdired--unsplit (list separator)
  (let (res)
    (dolist (e (butlast list))
      (push e res)
      (push separator res))
    (push (car (last list)) res)
    (apply 'concat (reverse res))))

(defun mpdired--parent ()
  (when (stringp mpdired--directory)
    (let ((split (split-string mpdired--directory "/")))
      (if (= 1 (length split))
	  ""
	(mpdired--unsplit (butlast split) "/")))))

(defun mpdired-goto-parent ()
  (interactive)
  (let ((parent (mpdired--parent)))
    (if parent
	(mpdired-listall parent mpdired--directory)
      (message "You are at the toplevel."))))

(defun mpdired-test-me ()
  (interactive)
  (mpdired-listall ""))
