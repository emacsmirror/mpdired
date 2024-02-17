(defcustom mpdired-host (or (getenv "MPD_HOST") "localhost")
  "Host for MPD."
  :type 'string)

(defcustom mpdired-port (or (getenv "MPD_PORT") 6600)
  "Host for MPD."
  :type 'integer)

(defvar-keymap mpdired-mode-map
  :doc "Local keymap for MPDired."
  "C-n"   'mpdired-next-line
  "n"     'mpdired-next
  "C-p"   'mpdired-previous-line
  "p"     'mpdired-previous
  "q"     'bury-buffer
  "C-m"   'mpdired-enter
  "^"     'mpdired-goto-parent
  "o"     'mpdired-toggle-view
  "<SPC>" 'mpdired-toggle-play/pause
  "a"     'mpdired-add-at-point
  "D"     'mpdired-delete
  "g"     'mpdired-update)

(defun mpdired--subdir-p (dir-a dir-b)
  (let ((pos (string-search dir-a dir-b)))
    (and pos (zerop pos))))

;; State variables for the communication buffer
(defvar-local mpdired--network-params nil)
(defvar-local mpdired--parse-endp nil)
(defvar-local mpdired--last-command nil)
(defvar-local mpdired--main-buffer nil
  "Link to the main MPDired buffer")
(defvar-local mpdired--previous-directory nil
  "Previous directory used to pass to the MPDired buffer.")
(defvar-local mpdired--ascending-p nil)

(defun mpdired--parse-listall-1 (current accum)
  ;; Recursively rebuild the directory hierarchy from a "listall"
  ;; command into a list.  In the output, a directory is list which
  ;; `car' is its name and its `cdr' is the files or other directory
  ;; it contains.
  (catch 'exit
    (while (not (or mpdired--parse-endp
		    (setq mpdired--parse-endp
			  (re-search-forward "^\\(OK\\|ACK.*\\)$" (line-end-position) t 1))))
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
  ;; Called from the communication buffer.
  (goto-char (point-min))
  (setq mpdired--parse-endp nil)
  ;; XXX Empty string is the directory name of the toplevel directory.
  ;; It have the good property of being a prefix of any string.
  (mpdired--parse-listall-1 "" (list "")))

;; All my functions are called *-queue but the correct are using the
;; correct "playlistid" MPD interface.
(defun mpdired--parse-queue ()
  ;; Called from the communication buffer.
  (goto-char (point-min))
  (setq mpdired--parse-endp nil)
  (let (result file time id)
    (while (not (or mpdired--parse-endp
		    (setq mpdired--parse-endp
			  (re-search-forward "^OK$" (line-end-position) t 1))))
      ;; File
      (when (re-search-forward "^file: \\(.*\\)$" (line-end-position) t 1)
	;; if file is already set store the previous entry in the
	;; list.
	(when file
	  (push (list id file time) result))
	(setq file (match-string 1)))
      ;; Time
      (when (re-search-forward "^Time: \\(.*\\)$" (line-end-position) t 1)
	(setq time (string-to-number (match-string 1))))
      ;; Id
      (when (re-search-forward "^Id: \\(.*\\)$" (line-end-position) t 1)
	(setq id (string-to-number (match-string 1))))
      (forward-line))
    ;; The last one
    (when file (push (list id file time) result))
    (reverse result)))

(defun mpdired-mode ()
  "Major mode for MPDired."
  (use-local-map mpdired-mode-map)
  (set-buffer-modified-p nil)
  (setq major-mode 'mpdired-mode
	mode-name "MPDired"
	buffer-read-only t))

(defun mpdired--hostname (host service localp)
  (if localp
      (format "%s" host)
    (format "%s:%s" host service)))

(defun mpdired--comm-name (host service localp)
  (format "*mpdired-%s*" (mpdired--hostname host service localp)))

(defun mpdired--main-name (host service localp)
  (format "*MPDired (%s)*" (mpdired--hostname host service localp)))

;; State variables for the main buffer
(defvar-local mpdired--directory nil
  "Current directory of the browser view.")
(defvar-local mpdired--view nil)
(defvar-local mpdired--comm-buffer nil
  "Communication buffer associated to this MPDired buffer.")

(defun mpdired--insert-entry (entry)
  (cond ((stringp entry)
	 (insert entry)
	 (put-text-property (line-beginning-position) (line-end-position) 'type 'file))
	((consp entry)
	 (insert (propertize (car entry) 'face 'dired-directory))
	 (put-text-property (line-beginning-position) (line-end-position) 'type 'directory))))

(defun mpdired--insert-song (song)
  (insert (propertize (cadr song) 'face 'dired-ignored))
  (put-text-property (line-beginning-position) (line-end-position) 'id (car song)))

(defun mpdired--present-listall (proc)
  ;; Called by filter of the communication buffer.
  (let* ((peer-info (process-contact proc t))
	 (peer-host (plist-get peer-info :host))
	 (peer-service (plist-get peer-info :service))
	 (peer-localp (eq (plist-get peer-info :family) 'local))
	 (buffer-name (mpdired--main-name peer-host peer-service peer-localp))
	 (content (mpdired--parse-listall))
	 from-directory ascending-p)
    ;; Retrieve infos from this process buffer
    (with-current-buffer (process-buffer proc)
      (setq from-directory mpdired--previous-directory
	    ascending-p mpdired--ascending-p))
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
	    (if top (insert (propertize top 'face 'dired-header) ":\n"))
	    (dolist (e (butlast data))
	      (mpdired--insert-entry e)
	      (insert "\n"))
	    (mpdired--insert-entry (car (last data))))
	  ;; Go to the previous directory line when ascending
	  (cond (ascending-p
		 (goto-char (point-min))
		 (re-search-forward from-directory nil t)
		 (goto-char (line-beginning-position)))
		(t
		 (goto-char (point-min))
		 (if top (mpdired-next-line))))
	  ;; Set mode and memorize stuff
	  (mpdired-mode)
	  (setq mpdired--directory (when top top)
		mpdired--comm-buffer (process-buffer proc)
		mpdired--view 'browser))))))

(defun mpdired--present-queue (proc)
  ;; Called by filter of the communication buffer.
  (let* ((peer-info (process-contact proc t))
	 (peer-host (plist-get peer-info :host))
	 (peer-service (plist-get peer-info :service))
	 (peer-localp (eq (plist-get peer-info :family) 'local))
	 (buffer-name (mpdired--main-name peer-host peer-service peer-localp))
	 (content (mpdired--parse-queue)))
    (with-current-buffer (get-buffer-create buffer-name)
      (let ((inhibit-read-only t))
	(erase-buffer)
	;; Insert the content
	(save-excursion
	  (dolist (song (butlast content))
	    (mpdired--insert-song song)
	    (insert "\n"))
	  (when content
	    (mpdired--insert-song (car (last content)))))
	;; Set mode and memorize stuff
	(mpdired-mode)
	(setq mpdired--comm-buffer (process-buffer proc)
	      mpdired--view 'queue)))))

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
	(when (re-search-backward "^\\(OK\\|ACK.*\\)$" nil t)
	  (cond ((eq mpdired--last-command 'listall)
		 (mpdired--present-listall proc))
		((eq mpdired--last-command 'queue)
		 (mpdired--present-queue proc))))))))

(defun mpdired--sentinel (process event)
  (message "Process: %s had the event '%s'" process event))

(defun mpdired--local-p (host)
  ;; Hack: if the `expand-file-name' of host leads to an existing
  ;; file, that should be our Unix socket.
  (file-exists-p (expand-file-name host)))

(defun mpdired--maybe-reconnect (comm-buffer)
  (let ((process (get-buffer-process comm-buffer)))
    (unless (and process (eq (process-status process) 'open))
      ;; Reconnect from saved parameters.
      (if mpdired--network-params
	  (set-process-buffer (apply 'make-network-process mpdired--network-params)
			      comm-buffer)))))

(defun mpdired--maybe-init (host service localp)
  (with-current-buffer (get-buffer-create (mpdired--comm-name host service localp))
    (erase-buffer)
    (let ((process (get-buffer-process (current-buffer))))
      (unless (and process (eq (process-status process) 'open))
	(let ((params (list :name "mpdired"
			    :buffer (current-buffer)
			    :host host
			    :service service
			    :family (if localp 'local)
			    :coding 'utf-8
			    :filter 'mpdired--filter
			    :sentinel 'mpdired--sentinel)))
	  (setq mpdired--network-params params
		mpdired--main-buffer (mpdired--main-name host service localp))
	  (set-process-buffer (apply 'make-network-process params)
			      (current-buffer)))))))

(defmacro mpdired--with-comm-buffer (process buffer &rest body)
  "Helper macro when sending a command via the communication buffer."
  (declare (indent defun))
  `(with-current-buffer (or ,buffer mpdired--comm-buffer)
     (erase-buffer)
     (mpdired--maybe-reconnect (current-buffer))
     (let ((,process (get-buffer-process (current-buffer))))
       (when (process-live-p ,process)
	 ,@body))))

(defun mpdired-listall-internal (path &optional ascending-p)
  (mpdired--with-comm-buffer process nil
    (setq mpdired--last-command 'listall
	  mpdired--previous-directory (with-current-buffer mpdired--main-buffer mpdired--directory)
	  mpdired--ascending-p ascending-p)
    (process-send-string process (format "listall \"%s\"\n" path))))

(defun mpdired-queue-internal (&optional buffer)
  (mpdired--with-comm-buffer process buffer
    (setq mpdired--last-command 'queue)
    (process-send-string process "playlistid\n")))

(defun mpdired-queue (comm-buffer)
  (mpdired-queue-internal comm-buffer))

(defun mpdired-playid-internal (id)
  (mpdired--with-comm-buffer process nil
    (setq mpdired--last-command 'playid)
    (process-send-string process (format "playid %d\n" id))))

(defun mpdired-add-internal (uri)
  (mpdired--with-comm-buffer process nil
    (setq mpdired--last-command 'add)
    (process-send-string process (format "add \"%s\"\n" uri))))

(defun mpdired-deleteid-internal (id)
  (mpdired--with-comm-buffer process nil
    (setq mpdired--last-command 'deleteid)
    (process-send-string process (format "deleteid %d\n" id))))

(defun mpdired-toggle-play/pause-internal (&optional buffer)
  (mpdired--with-comm-buffer process buffer
    (setq mpdired--last-command 'pause)
    (process-send-string process "pause\n")))

(defun mpdired-next-internal (&optional buffer)
  (mpdired--with-comm-buffer process buffer
    (setq mpdired--last-command 'next)
    (process-send-string process "next\n")))

(defun mpdired-previous-internal (&optional buffer)
  (mpdired--with-comm-buffer process buffer
    (setq mpdired--last-command 'previous)
    (process-send-string process "previous\n")))

(defun mpdired-status-internal ()
  (mpdired--with-comm-buffer process nil
    (setq mpdired--last-command 'status)
    (process-send-string process "status\n")))

(defun mpdired-next-line ()
  (interactive)
  (forward-line)
  (goto-char (line-beginning-position)))

(defun mpdired-previous-line ()
  (interactive)
  (forward-line -1)
  (goto-char (line-beginning-position)))

(defun mpdired-listall-at-point ()
  (goto-char (line-beginning-position))
  (save-excursion
    (re-search-forward "^\\(.*\\)$" (line-end-position) t))
  (if (eq (get-text-property (line-beginning-position) 'type) 'directory)
      (mpdired-listall-internal (match-string 1))
    (message "Cannot browse a file.")))

(defun mpdired-playid-at-point ()
  (let ((id (get-text-property (line-beginning-position) 'id)))
    (when id
      (mpdired-playid-internal id))))

(defun mpdired-enter ()
  (interactive)
  (if (eq mpdired--view 'browser)
      (mpdired-listall-at-point)
    (mpdired-playid-at-point)))

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
	(mpdired-listall-internal parent t)
      (message "You are at the toplevel."))))

(defun mpdired-toggle-view ()
  (interactive)
  (cond ((eq mpdired--view 'browser)
	 (mpdired-queue-internal))
	((eq mpdired--view 'queue)
	 (cond (mpdired--directory
		(mpdired-listall-internal mpdired--directory)
		;; Empty buffer? our current directory was probably
		;; bogus.
		(when (= 0 (buffer-size))
		  (sit-for .2)
		  (mpdired-listall-internal "")))
	       (t (mpdired-listall-internal ""))))))

(defun mpdired-toggle-play/pause ()
  (interactive)
  (mpdired-toggle-play/pause-internal))

(defun mpdired-add-at-point ()
  (interactive)
  (goto-char (line-beginning-position))
  (save-excursion
    (re-search-forward "^\\(.*\\)$" (line-end-position) t))
  (let ((uri (match-string 1)))
    (when uri
      (mpdired-add-internal uri))))

(defun mpdired-deleteid-at-point ()
  (let ((id (get-text-property (line-beginning-position) 'id)))
    (when id
      (mpdired-deleteid-internal id))))

(defun mpdired-delete ()
  (interactive)
  (cond ((eq mpdired--view 'queue)
	 (mpdired-deleteid-at-point)
	 (mpdired-queue-internal))))

(defun mpdired-update ()
  (interactive)
  (cond ((eq mpdired--view 'queue)
	 (mpdired-queue-internal))
	((eq mpdired--view 'browser)
	 (if mpdired--directory
	     (mpdired-listall-internal mpdired--directory)
	   (mpdired-listall-internal "")))))

(defun mpdired-next ()
  (interactive)
  (cond ((eq mpdired--view 'browser)
	 (mpdired-next-line))
	((eq mpdired--view 'queue)
	 (mpdired-next-internal))))

(defun mpdired-previous ()
  (interactive)
  (cond ((eq mpdired--view 'browser)
	 (mpdired-previous-line))
	((eq mpdired--view 'queue)
	 (mpdired-previous-internal))))

;; Main entry point
(defun mpdired ()
  (interactive)
  ;; Get user's host and service current setting.
  (let* ((localp (mpdired--local-p mpdired-host))
	 (host (if localp (expand-file-name mpdired-host) mpdired-host))
	 (service (if localp host mpdired-port))
	 (comm-name (mpdired--comm-name host service localp))
	 (main-name (mpdired--main-name host service localp)))
    (mpdired--maybe-init host service localp)
    ;; Defaults to queue view
    (mpdired-queue comm-name)
    (pop-to-buffer main-name)))
