(defcustom mpdired-host (or (getenv "MPD_HOST") "localhost")
  "Host for MPD."
  :type 'string)

(defcustom mpdired-port (or (getenv "MPD_PORT") 6600)
  "Host for MPD."
  :type 'integer)

(defvar-keymap mpdired-mode-map
  :doc "Local keymap for MPDired."
  "C-n"    'mpdired-next-line
  "n"      'mpdired-next-line
  "<down>" 'mpdired-next-line
  "C-p"    'mpdired-previous-line
  "p"      'mpdired-previous-line
  "<up>"   'mpdired-previous-line
  "C-m"    'mpdired-enter
  "^"      'mpdired-goto-parent
  "o"      'mpdired-toggle-view
  "g"      'mpdired-update
  "q"      'bury-buffer
  "<SPC>"  'mpdired-pause-internal
  "N"      'mpdired-next-internal
  "P"      'mpdired-previous-internal
  "a"      'mpdired-add-at-point
  ;; Only for queue
  "D"      'mpdired-delete)

(defun mpdired--subdir-p (dir-a dir-b)
  (let ((pos (string-search dir-a dir-b)))
    (and pos (zerop pos))))

;; State variables for the communication buffer
(defvar-local mpdired--network-params nil)
(defvar-local mpdired--parse-endp nil)
(defvar-local mpdired--last-command nil)
(defvar-local mpdired--main-buffer nil
  "Link to the main MPDired buffer")
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
  (let ((elapsed 0)
	(duration 1)
	(songid 0)
	(in-status-p t)
	result file time id)
    (while (not (or mpdired--parse-endp
		    (setq mpdired--parse-endp
			  (re-search-forward "^OK$" (line-end-position) t 1))))
      (let ((eol (line-end-position)))
        ;; First, "status" content
	(when (re-search-forward "^songid: \\([0-9]+\\)$" eol t 1)
	  (setq songid (string-to-number (match-string 1))))
	(when (re-search-forward "^time: \\([0-9]+\\):\\([0-9]+\\)$" eol t 1)
	  (setq elapsed (string-to-number (match-string 1))
		duration (string-to-number (match-string 2))))
	;; When we enconter our first "file:" the status parsing is
	;; done so store what we've discovered so far.
	(when (and in-status-p
		   (save-excursion (re-search-forward "^file: .*$" eol t 1)))
	  (setq in-status-p nil)
	  (push songid result)
          (push elapsed result)
	  (push duration result))
	;; Then, "playlistid" content
	;; File
	(when (re-search-forward "^file: \\(.*\\)$" eol t 1)
	  ;; if file is already set store the previous entry in the
	  ;; list.
	  (when file
	    (push (list id file time) result))
	  (setq file (match-string 1)))
	;; Time
	(when (re-search-forward "^Time: \\(.*\\)$" eol t 1)
	  (setq time (string-to-number (match-string 1))))
	;; Id
	(when (re-search-forward "^Id: \\(.*\\)$" eol t 1)
	  (setq id (string-to-number (match-string 1)))))
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
(defvar-local mpdired--previous-directory nil
  "Previous directory of the browser view.")
(defvar-local mpdired--view nil)
(defvar-local mpdired--comm-buffer nil
  "Communication buffer associated to this MPDired buffer.")
(defvar-local mpdired--browser-point nil
  "Saved point position in the browser view.")
(defvar-local mpdired--queue-point nil
  "Saved point position in the queue view.")

(defun mpdired--insert-entry (entry)
  (let ((bol (line-beginning-position)))
    (cond ((stringp entry)
	   (insert entry)
	   (put-text-property bol (line-end-position) 'type 'file)
	   (put-text-property bol (line-end-position) 'uri entry))
	  ((consp entry)
	   (insert (propertize (car entry) 'face 'dired-directory))
	   (put-text-property bol (line-end-position) 'type 'directory)
	   (put-text-property bol (line-end-position) 'uri (car entry))))))

(defun mpdired--insert-song (song)
  (let ((id (car song))
	(uri (cadr song)))
    (insert (propertize uri 'face 'dired-ignored))
    (let ((bol (line-beginning-position))
	  (eol (line-end-position)))
      (put-text-property bol eol 'id id)
      (put-text-property bol eol 'uri uri))))

(defun mpdired--present-listall (proc)
  ;; Called by filter of the communication buffer.
  (let* ((peer-info (process-contact proc t))
	 (peer-host (plist-get peer-info :host))
	 (peer-service (plist-get peer-info :service))
	 (peer-localp (eq (plist-get peer-info :family) 'local))
	 (main-buffer (mpdired--main-name peer-host peer-service peer-localp))
	 (content (mpdired--parse-listall))
	 ascending-p)
    ;; Retrieve infos from this process buffer
    (with-current-buffer (process-buffer proc)
      (setq ascending-p mpdired--ascending-p))
    (with-current-buffer (get-buffer-create main-buffer)
      (let* ((inhibit-read-only t)
	     ;; `content' is always of the form ("" rest...) so if there
	     ;; is only one "rest" use it as content.
	     (content (if (cddr content) content (cadr content)))
	     (top (unless (string= "" (car content)) (car content)))
	     (data (cdr content)))
	(erase-buffer)
	;; Insert the content
	(save-excursion
	  (if top (insert (propertize top 'face 'dired-header) ":\n"))
	  (dolist (e (butlast data))
	    (mpdired--insert-entry e)
	    (insert "\n"))
	  (mpdired--insert-entry (car (last data))))
	;; Set mode and memorize stuff
	(mpdired-mode)
	(if ascending-p (setq mpdired--previous-directory mpdired--directory))
	(setq mpdired--directory (when top top)
	      mpdired--comm-buffer (process-buffer proc)
	      mpdired--view 'browser)
	;; Finally move point to the correct place.
	(cond ((and ascending-p mpdired--previous-directory)
	       (goto-char (point-min))
	       (re-search-forward mpdired--previous-directory nil t)
	       (goto-char (line-beginning-position))
	       (setq mpdired--browser-point (point)))
	      (mpdired--browser-point
	       (goto-char mpdired--browser-point))
	      (t (goto-char (point-min))
		 (when top (mpdired-next-line))))))))

(defun mpdired--present-queue (proc)
  ;; Called by filter of the communication buffer.
  (let* ((peer-info (process-contact proc t))
	 (peer-host (plist-get peer-info :host))
	 (peer-service (plist-get peer-info :service))
	 (peer-localp (eq (plist-get peer-info :family) 'local))
	 (main-buffer (mpdired--main-name peer-host peer-service peer-localp))
	 (data (mpdired--parse-queue))
	 (songid (car data))
	 (elapsed (cadr data))
	 (duration (caddr data))
	 (songs (cdddr data)))
    (with-current-buffer (get-buffer-create main-buffer)
      (let ((inhibit-read-only t))
	(erase-buffer)
	;; Insert songs
	(save-excursion
	  (dolist (song (butlast songs))
	    (mpdired--insert-song song)
	    (insert "\n"))
	  (when songs
	    (mpdired--insert-song (car (last songs)))))
	;; Go to the current song and display elasped time with a face
	;; on the URI.
	(save-excursion
	  (when songid
	    (while (let ((id (get-text-property (point) 'id)))
		     (and id (/= songid id)))
	      (forward-line))
	    (let* ((bol (line-beginning-position))
		   (eol (line-end-position))
		   (x (/ (* elapsed (- eol bol)) duration)))
	      (put-text-property bol (+ bol x) 'face 'dired-special))))
	;; Set mode, restore point and memorize stuff
	(mpdired-mode)
	(when mpdired--queue-point
	  (goto-char mpdired--queue-point))
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
		((or (eq mpdired--last-command 'queue)
		     (eq mpdired--last-command 'deleteid))
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
	  mpdired--ascending-p ascending-p)
    (process-send-string process (format "listall \"%s\"\n" path))))

(defun mpdired-queue-internal (&optional buffer)
  (mpdired--with-comm-buffer process buffer
    (setq mpdired--last-command 'queue)
    ;; Also get the status to identify the current song.
    (process-send-string process "command_list_begin\n")
    (process-send-string process "status\n")
    (process-send-string process "playlistid\n")
    (process-send-string process "command_list_end\n")))

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
    (process-send-string process "command_list_begin\n")
    (process-send-string process (format "deleteid %d\n" id))
    ;; XXX A playlistid should always be preceded by a status
    (process-send-string process "status\n")
    (process-send-string process "playlistid\n")
    (process-send-string process "command_list_end\n")))

(defun mpdired-pause-internal (&optional buffer)
  (interactive)
  (mpdired--with-comm-buffer process buffer
    (setq mpdired--last-command 'pause)
    (process-send-string process "pause\n")))

(defun mpdired-next-internal (&optional buffer)
  (interactive)
  (mpdired--with-comm-buffer process buffer
    (setq mpdired--last-command 'next)
    (process-send-string process "next\n")))

(defun mpdired-previous-internal (&optional buffer)
  (interactive)
  (mpdired--with-comm-buffer process buffer
    (setq mpdired--last-command 'previous)
    (process-send-string process "previous\n")))

;; XXX for debugging
(defun mpdired-status-internal ()
  (mpdired--with-comm-buffer process nil
    (setq mpdired--last-command 'status)
    (process-send-string process "command_list_begin\n")
    (process-send-string process "status\n")
    (process-send-string process "command_list_end\n")))

(defun mpdired--save-point ()
  (cond ((eq mpdired--view 'queue)
	 (setf mpdired--queue-point (point)))
	((eq mpdired--view 'browser)
	 (setf mpdired--browser-point (point)))))

(defun mpdired-next-line ()
  (interactive)
  (forward-line)
  (goto-char (line-beginning-position))
  (mpdired--save-point))

(defun mpdired-previous-line ()
  (interactive)
  (forward-line -1)
  (goto-char (line-beginning-position))
  (mpdired--save-point))

(defun mpdired-listall-at-point ()
  (let* ((bol (line-beginning-position))
	 (type (get-text-property bol 'type))
	 (uri (get-text-property bol 'uri)))
    (if (eq type 'directory)
	(mpdired-listall-internal uri)
      (message "Cannot browse a file."))))

(defun mpdired-playid-at-point ()
  (let ((id (get-text-property (line-beginning-position) 'id)))
    (when id
      (mpdired-playid-internal id))))

(defun mpdired-enter ()
  (interactive)
  (cond ((eq mpdired--view 'browser)
	 (setq mpdired--browser-point nil)
	 (mpdired-listall-at-point))
	(t (mpdired-playid-at-point))))

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
    (cond (parent
	   (setq mpdired--browser-point nil)
	   (mpdired-listall-internal parent t))
	  (t (message "You are at the toplevel.")))))

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

(defun mpdired-add-at-point ()
  (interactive)
  (let* ((bol (line-beginning-position))
	 (uri (get-text-property bol 'uri)))
    (when uri (mpdired-add-internal uri))))

(defun mpdired-deleteid-at-point ()
  (let ((id (get-text-property (line-beginning-position) 'id)))
    (when id
      (mpdired-deleteid-internal id))))

(defun mpdired-delete ()
  (interactive)
  (cond ((eq mpdired--view 'queue)
	 (mpdired-deleteid-at-point))))

(defun mpdired-update ()
  (interactive)
  (cond ((eq mpdired--view 'queue)
	 (mpdired-queue-internal))
	((eq mpdired--view 'browser)
	 (if mpdired--directory
	     (mpdired-listall-internal mpdired--directory)
	   (mpdired-listall-internal "")))))

(defun mpdired--prepare ()
  ;; Get user's host and service current setting.
  (let* ((localp (mpdired--local-p mpdired-host))
	 (host (if localp (expand-file-name mpdired-host) mpdired-host))
	 (service (if localp host mpdired-port))
	 (comm-name (mpdired--comm-name host service localp))
	 (main-name (mpdired--main-name host service localp)))
    (mpdired--maybe-init host service localp)
    (cons comm-name main-name)))


;; General commands (i.e. usable outside of the MPDired buffer).
(defun mpdired-pause ()
  (interactive)
  (let ((buffers (mpdired--prepare)))
    (mpdired-pause-internal (car buffers))))

(defun mpdired-next ()
  (interactive)
  (let ((buffers (mpdired--prepare)))
    (mpdired-next-internal (car buffers))))

(defun mpdired-previous ()
  (interactive)
  (let ((buffers (mpdired--prepare)))
    (mpdired-previous-internal (car buffers))))

;; Main entry point.
(defun mpdired ()
  (interactive)
  (let* ((buffers (mpdired--prepare))
	 (comm (car buffers))
	 (main (cdr buffers)))
    ;; Defaults to queue view
    (mpdired-queue-internal comm)
    (pop-to-buffer main)))

(provide 'mpdired)
