;;; mpdired.el --- A dired-like client for Music Player Daemon -*- lexical-binding: t -*-

;; Copyright (C) 2024  Manuel Giraud

;; Author: Manuel Giraud <manuel@ledu-giraud.fr>
;; Keywords: multimedia

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This a client for the Music Player Daemon (mpd) with interactions
;; inspired from Dired.  It features two views packed into the same
;; interactive buffer: the browser view and the queue view.
;;
;; In those view, most of the interactions are mimic after Dired mode
;; with marks and action on them.  For example, in the queue view, you
;; could flag songs for removal with `d' and then execute with `x'.
;;
;; MPDired connects to a MPD server using two customs: `mpdired-host'
;; and `mpdired-port'.  Once connected, the handle to the server is
;; saved in a buffer local variable into the MPDired buffer.  From now
;; on, the customs are just used by global MPDired commands to connect
;; to the user defined server.  All commands used inside a MPDired
;; buffer will connect to the buffer local server.  This way, you can
;; manage more than one MPD server with multiple MPDired buffers.

;;; Philosophy:
;;
;; MPDired is designed to be the least intrusive.  Nothing will be
;; shown into the mode line, which I consider to be user's territory.
;; There is no timers set by MPDired, so updating anything always
;; comes from a user action.
;;
;; The browser view is built only from the MPD's "listall" command.
;; The MPD's documentation does *not* recommend to do so but AFAIU
;; there is no other way to access your music collection in terms of
;; directories and files.  As my music collection is already ordered
;; into directories and with meaningful filenames, I prefer to use
;; this interface rather then to rely on files' tags.
;;
;; If your music collection consists of just a set of not very well
;; named files into one big directory and that you rely on tags such
;; as "Genre", "Album", "Artist" to find your way through it then,
;; maybe, MPDired is not the right client for you.

;;; Bugs & Funs:
;;
;; - MPDired does not handle MPD server with password.
;;
;; - Marks are really temporary.  As I rebuild the views often and the
;;   mark is only stored in text property they will be wipe out
;;   regularly.
;;
;; - some URI based commands work in both view. So for example, in the
;;   queue, you can append the song at point to this same queue.

;;; Code:

(defcustom mpdired-host (or (getenv "MPD_HOST") "localhost")
  "Host for MPD."
  :type 'string)

(defcustom mpdired-port (or (getenv "MPD_PORT") 6600)
  "Host for MPD."
  :type 'integer)

(defvar-keymap mpdired-mode-map
  :doc "Local keymap for MPDired."
  :full t
  :parent special-mode-map
  "C-n"    #'mpdired-next-line
  "n"      #'mpdired-next-line
  "<down>" #'mpdired-next-line
  "C-p"    #'mpdired-previous-line
  "p"      #'mpdired-previous-line
  "<up>"   #'mpdired-previous-line
  "C-m"    #'mpdired-enter
  "^"      #'mpdired-goto-parent
  "o"      #'mpdired-toggle-view
  "g"      #'mpdired-update
  "G"      #'mpdired-db-update
  "<SPC>"  #'mpdired-pause-internal
  "N"      #'mpdired-next-internal
  "P"      #'mpdired-previous-internal
  "a"      #'mpdired-add
  "v"      #'mpdired-set-volume-internal
  ;; Status toggles
  "s s"    #'mpdired-stop
  "s R"    #'mpdired-toggle-repeat
  "s r"    #'mpdired-toggle-random
  "s S"    #'mpdired-toggle-single
  "s c"    #'mpdired-toggle-consume
  ;; Marks
  "m"      #'mpdired-mark-at-point
  "* m"    #'mpdired-mark-at-point
  "d"      #'mpdired-flag-at-point
  "u"      #'mpdired-unmark-at-point
  "<DEL>"  #'mpdired-previous-unmark
  "* !"    #'mpdired-unmark-all-marks
  "U"      #'mpdired-unmark-all-marks
  "t"      #'mpdired-toggle-marks
  "* t"    #'mpdired-toggle-marks
  "* c"    #'mpdired-change-marks
  "% d"    #'mpdired-flag-files-regexp
  "% m"    #'mpdired-mark-files-regexp
  "x"      #'mpdired-flagged-delete
  "D"      #'mpdired-delete)

(defface mpdired-currdir
  '((t :inherit dired-header))
  "Face used to show current directory.")

(defface mpdired-directory
  '((t :inherit dired-directory))
  "Face used to show a directory.")

(defface mpdired-playlist
  '((t :inherit dired-symlink))
  "Face used to show a playlist.")

(defface mpdired-song
  '((t :inherit dired-ignored))
  "Face used to show a song.")

(defface mpdired-progress
  '((t :inherit dired-special))
  "Face used to show the progress of a song.")

(defface mpdired-marked
  '((t :inherit dired-marked))
  "Face used to show a marked entry.")

(defface mpdired-flagged
  '((t :inherit dired-flagged))
  "Face used to show an entry flagged for deletion.")

;; State variables for the communication buffer
(defvar-local mpdired--network-params nil)
(defvar-local mpdired--parse-endp nil)
(defvar-local mpdired--last-command nil)
(defvar-local mpdired--main-buffer nil
  "Link to the main MPDired buffer.")
(defvar-local mpdired--ascending-p nil)
(defvar-local mpdired--playlist nil)
(defvar-local mpdired--message nil)

(defun mpdired--subdir-p (dir-a dir-b)
  (let ((pos (string-search dir-a dir-b)))
    (and pos (zerop pos))))

(defun mpdired--parse-listall-1 (current accum)
  ;; Recursively rebuild the directory hierarchy from a "listall"
  ;; command into a list.  In the output, a directory is list which
  ;; `car' is its name and its `cdr' is the files or other directory
  ;; it contains.
  (catch 'exit
    (while (not (or mpdired--parse-endp
		    (setq mpdired--parse-endp
			  (re-search-forward "^\\(OK\\|ACK.*\\)$"
					     (line-end-position) t 1))))
      ;; Look for file, playlist or directory line by line.
      (when
	  (re-search-forward "^\\(file\\|playlist\\|directory\\): \\(.*\\)$"
			     (line-end-position) t 1)
	(let ((type (match-string 1))
	      (name (match-string 2)))
	  (cond ((or (string= "file" type)
		     (string= "playlist" type))
		 (push (cons (intern type) name) accum))
		((string= "directory" type)
		 ;; This new directory is either a subdir of the current
		 ;; one or a new directory of the same level of the
		 ;; current one.  In this last case we need to go one
		 ;; line backward (because we will go forward later) and
		 ;; quit the loop.
		 (cond ((mpdired--subdir-p current name)
			(forward-line)
			(push (mpdired--parse-listall-1 name (list name)) accum))
		       (t (forward-line -1)
			  (throw 'exit t)))))))
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
	state volume repeat random single consume
	result file time id)
    (while (not (or mpdired--parse-endp
		    (setq mpdired--parse-endp
			  (re-search-forward "^\\(OK\\|ACK.*\\)$"
					     (line-end-position) t 1))))
      (let ((eol (line-end-position)))
        ;; First, "status" content
	(when in-status-p
	  (when (re-search-forward "^state: \\(.*\\)$" eol t 1)
	    (setq state (match-string 1)))
	  (when (re-search-forward "^volume: \\([0-9]+\\)$" eol t 1)
	    (setq volume (string-to-number (match-string 1))))
	  (when (re-search-forward "^repeat: \\([0-9]+\\)$" eol t 1)
	    (setq repeat (string= "1" (match-string 1))))
	  (when (re-search-forward "^random: \\([0-9]+\\)$" eol t 1)
	    (setq random (string= "1" (match-string 1))))
	  (when (re-search-forward "^single: \\([0-9]+\\)$" eol t 1)
	    (setq single (string= "1" (match-string 1))))
	  (when (re-search-forward "^consume: \\([0-9]+\\)$" eol t 1)
	    (setq consume (string= "1" (match-string 1))))
	  ;; current song status
	  (when (re-search-forward "^songid: \\([0-9]+\\)$" eol t 1)
	    (setq songid (string-to-number (match-string 1))))
	  (when (re-search-forward "^time: \\([0-9]+\\):\\([0-9]+\\)$" eol t 1)
	    (setq elapsed (string-to-number (match-string 1))
		  duration (string-to-number (match-string 2)))))
	;; When we enconter our first "file:" the status parsing is
	;; done so store what we've discovered so far.
	(when (and in-status-p
		   (save-excursion (re-search-forward "^file: .*$" eol t 1)))
	  (setq in-status-p nil)
	  ;; Save status in main buffer
	  (with-current-buffer mpdired--main-buffer
	    (setq mpdired--status
		  (list state volume repeat random single consume)))
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
    ;; There was only status but no songs
    (when in-status-p
      ;; Save status in main buffer
      (with-current-buffer mpdired--main-buffer
	(setq mpdired--status
	      (list state volume repeat random single consume)))
      (push songid result)
      (push elapsed result)
      (push duration result))
    ;; The last song if any
    (when file (push (list id file time) result))
    (reverse result)))

(defun mpdired-mode ()
  "Major mode for MPDired."
  (use-local-map mpdired-mode-map)
  (set-buffer-modified-p nil)
  (setq major-mode 'mpdired-mode
	mode-name "MPDired"
	truncate-lines t
	buffer-read-only t))

(defun mpdired--hostname (host service localp)
  (if localp
      (let ((inode (file-attribute-inode-number
		    (file-attributes service))))
	(format "/%d" inode))
    (if (= 6600 service)
	host
      (format "%s:%s" host service))))

(defun mpdired--comm-name (host service localp)
  (format " *mpdired-comm-%s*" (mpdired--hostname host service localp)))

(defun mpdired--main-name (host service localp)
  (format "*MPDired (%s)*" (mpdired--hostname host service localp)))

;; State variables for the main buffer
(defvar-local mpdired--view nil
  "Current view of the MPDired buffer.")
(defvar-local mpdired--directory nil
  "Current directory (or MPD playlist) of the browser view.")
(defvar-local mpdired--comm-buffer nil
  "Communication buffer associated to this MPDired buffer.")
(defvar-local mpdired--status nil
  "Local copy of the MPD status.  It will updated regularly.")

;; I tried to use markers but since I often erase the buffer's
;; content, these markers are reset to 1.
(defvar-local mpdired--browser-point nil
  "Saved point position in the browser view.")
(defvar-local mpdired--songid-point nil
  "Songid for point position in the queue view.")

(defun mpdired--bol ()
  "Correct beginning of line in a MPDired buffer."
  (+ 2 (line-beginning-position)))

(defun mpdired--short-name (string)
  (car (last (split-string string "/"))))

(defun mpdired--reset-face ()
  (let* ((bol (mpdired--bol))
	 (eol (line-end-position))
	 (type (get-text-property bol 'type))
	 (mark (get-text-property bol 'mark)))
    (remove-text-properties bol eol '(face))
    (cond ((and mark (char-equal mark ?d))
	   (put-text-property bol eol 'face 'mpdired-flagged))
	  ((and mark (char-equal mark ?*))
	   (put-text-property bol eol 'face 'mpdired-marked))
	  ((eq type 'directory)
	   (put-text-property bol eol 'face 'mpdired-directory))
	  ((eq type 'playlist)
	   (put-text-property bol eol 'face 'mpdired-playlist))
	  ((eq type 'song)
	   (put-text-property bol eol 'face 'mpdired-song)))))

(defun mpdired--insert-entry (entry)
  "Insert ENTRY in MPDired browser view."
  (insert "  ")
  (let ((bol (mpdired--bol)))
    (when (consp entry)
      (let ((type (car entry)))
	(cond ((stringp type)  ;; this is a directory
	       (insert (propertize (mpdired--short-name type) 'face 'mpdired-directory))
	       (put-text-property bol (line-end-position) 'type 'directory)
	       (put-text-property bol (line-end-position) 'uri type))
	      ((eq type 'file)
	       (let ((file (cdr entry)))
		 (insert (mpdired--short-name file))
		 (put-text-property bol (line-end-position) 'type 'file)
		 (put-text-property bol (line-end-position) 'uri file)))
	      ((eq type 'playlist)
	       (let ((playlist (cdr entry)))
		 (insert (propertize (mpdired--short-name playlist)
				     'face 'mpdired-playlist))
		 (put-text-property bol (line-end-position) 'type 'playlist)
		 (put-text-property bol (line-end-position) 'uri playlist))))))
    (insert "\n")))

(defun mpdired--insert-status ()
  "Insert current status in MPDired queue view."
  (when mpdired--status
    (let* ((state (car mpdired--status))
	   (volume (nth 1 mpdired--status))
	   (repeat (nth 2 mpdired--status))
	   (random (nth 3 mpdired--status))
	   (single (nth 4 mpdired--status))
	   (consume (nth 5 mpdired--status))
	   (string (cond ((string= "stop" state) "Stopped")
			 ((string= "play" state) "Playing")
			 ((string= "pause" state) "Paused"))))
      (insert (propertize string 'face 'bold))
      (when (numberp volume)
	(insert (format " Volume: %d" volume)))
      (when repeat (insert " Repeat"))
      (when random (insert " Random"))
      (when single (insert " Single"))
      (when consume (insert " Consume"))
      (insert "\n"))))

(defun mpdired--insert-song (song)
  "Insert SONG in MPDired queue view."
  (let ((id (car song))
	(uri (cadr song)))
    (insert "  " (propertize uri 'face 'mpdired-song))
    (let ((bol (mpdired--bol))
	  (eol (line-end-position)))
      (put-text-property bol eol 'id id)
      (put-text-property bol eol 'type 'song)
      (put-text-property bol eol 'uri uri))
    (insert "\n")))

(defun mpdired--goto-id (songid)
  (let ((max (point-max)))
    (while (and (< (point) max)
		(let ((id (get-text-property (mpdired--bol) 'id)))
		  (or (null id)
		      (and id (/= songid id)))))
      (mpdired--next-line))))

(defun mpdired--present-list (proc)
  ;; Called by filter of the communication buffer.
  (let* ((peer-info (process-contact proc t))
	 (peer-host (plist-get peer-info :host))
	 (peer-service (plist-get peer-info :service))
	 (peer-localp (eq (plist-get peer-info :family) 'local))
	 (main-buffer (mpdired--main-name peer-host peer-service peer-localp))
	 (content (mpdired--parse-listall))
	 ascending-p from)
    ;; Retrieve infos from this process buffer
    (with-current-buffer (process-buffer proc)
      (setq ascending-p mpdired--ascending-p
	    playlist mpdired--playlist))
    (with-current-buffer (get-buffer-create main-buffer)
      (let* ((inhibit-read-only t)
	     ;; `content' is always of the form ("" rest...) so if there
	     ;; is only one "rest" use it as content.
	     (content (if (cddr content) content (cadr content)))
	     (top (if playlist
		      playlist
		    (unless (string= "" (car content)) (car content))))
	     (data (cdr content)))
	(erase-buffer)
	;; Insert the content
	(save-excursion
	  (when (stringp top)
	    (insert (propertize top 'face 'mpdired-currdir) ":\n"))
	  (dolist (e data) (mpdired--insert-entry e)))
	;; Set mode and memorize stuff
	(mpdired-mode)
	(if ascending-p (setq from mpdired--directory))
	(setq mpdired--directory (when top top)
	      mpdired--comm-buffer (process-buffer proc)
	      mpdired--view 'browser)
	;; Finally move point to the correct place.
	(cond ((and ascending-p from)
	       (goto-char (point-min))
	       (let ((max (point-max)))
		 (while (and (< (point) max)
			     (let ((uri (get-text-property (mpdired--bol) 'uri)))
			       (or (null uri)
				   (and uri (not (string= from uri))))))
		   (forward-line)))
	       (goto-char (mpdired--bol))
	       (setq mpdired--browser-point (point)))
	      (mpdired--browser-point
	       (goto-char mpdired--browser-point)
	       (goto-char (mpdired--bol)))
	      (t (goto-char (point-min))
		 (if top
		     (mpdired-next-line)
		   (goto-char (mpdired--bol)))))))))

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
	;; Insert content
	(save-excursion
	  ;; Status header
	  (goto-char (point-min))
	  (mpdired--insert-status)
	  ;; Songs
	  (dolist (song songs) (mpdired--insert-song song)))
	;; Go to the current song and display elasped time with a
	;; different face on its URI.
	(save-excursion
	  (when songid
	    (let ((max (point-max)))
	      (while (and (< (point) max)
			  (let ((id (get-text-property (mpdired--bol) 'id)))
			    (or (null id)
				(and id (/= songid id)))))
		(forward-line)))
	    (let* ((bol (mpdired--bol))
		   (eol (line-end-position))
		   (x (/ (* elapsed (- eol bol)) duration)))
	      (put-text-property bol (+ bol x) 'face 'mpdired-progress))))
	;; Go to bol no matter what
	(goto-char (mpdired--bol))
	;; Set mode, restore point and memorize stuff
	(mpdired-mode)
	(when mpdired--songid-point
	  (mpdired--goto-id mpdired--songid-point))
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
	  (cond ((or (eq mpdired--last-command 'listall)
		     (eq mpdired--last-command 'listplaylist))
		 (mpdired--present-list proc))
		((or (eq mpdired--last-command 'queue)
		     (eq mpdired--last-command 'deleteid))
		 (mpdired--present-queue proc)))
	  ;; Display and reset message.
	  (when mpdired--message
	    (message (format "%s done." mpdired--message))
	    (setq mpdired--message nil)))))))

(defun mpdired--sentinel (process event)
  (unless (string-search "connection broken" event)
    (message "Process: %s had the event '%s'" process event)))

(defun mpdired--local-p (host)
  "Is HOST a local socket?"
  ;; Hack: if the `expand-file-name' of host leads to an existing
  ;; file, that should be our Unix socket.
  (file-exists-p (expand-file-name host)))

(defun mpdired--maybe-reconnect (comm-buffer)
  "If COMM-BUFFER is disconnected, reconnect it to its server."
  (let ((process (get-buffer-process comm-buffer)))
    (unless (and process (eq (process-status process) 'open))
      ;; Reconnect from saved parameters.
      (if mpdired--network-params
	  (set-process-buffer
	   (apply 'make-network-process mpdired--network-params)
	   comm-buffer)))))

(defun mpdired--maybe-init (host service localp)
  (with-current-buffer (get-buffer-create
			(mpdired--comm-name host service localp))
    (erase-buffer)
    (let ((process (get-buffer-process (current-buffer))))
      (unless (and process (eq (process-status process) 'open))
	(let ((params (list :name "mpdired-comm"
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
  "Helper macro when sending a command via the communication buffer.
PROCESS will be bound to the communication buffer's process.  BUFFER is
an optional communication buffer."
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
    (process-send-string process "command_list_begin\n")
    ;; At toplevel also lists MPD's playlists.
    (when (string= "" path)
      (process-send-string process "listplaylists\n"))
    (process-send-string process (format "listall \"%s\"\n" path))
    (process-send-string process "command_list_end\n")))

(defun mpdired-listplaylist-internal (path &optional ascending-p)
  (mpdired--with-comm-buffer process nil
    (setq mpdired--last-command 'listplaylist
	  mpdired--ascending-p ascending-p
	  mpdired--playlist path)
    (process-send-string process (format "listplaylist \"%s\"\n" path))))

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

(defun mpdired-add-internal (typed-uris)
  (mpdired--with-comm-buffer process nil
    (setq mpdired--last-command 'add)
    (process-send-string process "command_list_begin\n")
    (if (listp typed-uris)
	(dolist (typed-uri typed-uris)
	  (let ((type (car typed-uri))
		(uri (cdr typed-uri)))
	    ;; "add" is called "load" for playlists
	    (if (eq type 'playlist)
		(process-send-string process (format "load \"%s\"\n" uri))
	      (process-send-string process (format "add \"%s\"\n" uri)))))
      (process-send-string process (format "add \"%s\"\n" typed-uris)))
    (process-send-string process "command_list_end\n")))

(defun mpdired-deleteid-internal (id)
  (mpdired--with-comm-buffer process nil
    (setq mpdired--last-command 'deleteid)
    (process-send-string process "command_list_begin\n")
    (if (listp id)
	(dolist (i id)
	  (process-send-string process (format "deleteid %d\n" i)))
      (process-send-string process (format "deleteid %d\n" id)))
    ;; XXX A playlistid should always be preceded by a status
    (process-send-string process "status\n")
    (process-send-string process "playlistid\n")
    (process-send-string process "command_list_end\n")))

(defun mpdired-pause-internal (&optional buffer)
  "Toggle pause."
  (interactive)
  (mpdired--with-comm-buffer process buffer
    (setq mpdired--last-command 'pause
	  mpdired--message "Toggle pause...")
    (process-send-string process "pause\n")))

(defun mpdired-db-update ()
  "Issue a database update."
  (interactive)
  (mpdired--with-comm-buffer process nil
    (setq mpdired--last-command 'stop
	  mpdired--message "DB Update...")
    (process-send-string process "update\n")))

(defun mpdired-stop ()
  "Stops playing."
  (interactive)
  (mpdired--with-comm-buffer process nil
    (setq mpdired--last-command 'stop
	  mpdired--message "Stopping...")
    (process-send-string process "stop\n")))

(defun mpdired-toggle-repeat ()
  "Toggle repeat mode."
  (interactive)
  (mpdired--with-comm-buffer process nil
    (setq mpdired--last-command 'repeat)
    (let ((repeat
	   (with-current-buffer mpdired--main-buffer
	     (nth 2 mpdired--status))))
      (process-send-string process
			   (format "repeat %d\n" (if repeat 0 1))))))

(defun mpdired-toggle-random ()
  "Toggle random mode."
  (interactive)
  (mpdired--with-comm-buffer process nil
    (setq mpdired--last-command 'random)
    (let ((random
	   (with-current-buffer mpdired--main-buffer
	     (nth 3 mpdired--status))))
      (process-send-string process
			   (format "random %d\n" (if random 0 1))))))

;; XXX no oneshot support
(defun mpdired-toggle-single ()
  "Toggle single mode."
  (interactive)
  (mpdired--with-comm-buffer process nil
    (setq mpdired--last-command 'single)
    (let ((single
	   (with-current-buffer mpdired--main-buffer
	     (nth 4 mpdired--status))))
      (process-send-string process
			   (format "single %d\n" (if single 0 1))))))

;; XXX no oneshot support
(defun mpdired-toggle-consume ()
  "Toggle consume mode."
  (interactive)
  (mpdired--with-comm-buffer process nil
    (setq mpdired--last-command 'consume)
    (let ((consume
	   (with-current-buffer mpdired--main-buffer
	     (nth 5 mpdired--status))))
      (process-send-string process
			   (format "consume %d\n" (if consume 0 1))))))

(defun mpdired-next-internal (&optional buffer)
  "Start playing the next song from the queue."
  (interactive)
  (mpdired--with-comm-buffer process buffer
    (setq mpdired--last-command 'next)
    (process-send-string process "next\n")))

(defun mpdired-previous-internal (&optional buffer)
  "Start playing the previous song from the queue."
  (interactive)
  (mpdired--with-comm-buffer process buffer
    (setq mpdired--last-command 'previous)
    (process-send-string process "previous\n")))

(defun mpdired-set-volume-internal (volume &optional buffer)
  "Change MPD volume.  VOLUME is a number between 0 and 100."
  (interactive "nVolume: ")
  (mpdired--with-comm-buffer process buffer
    (setq mpdired--last-command 'setvol)
    (process-send-string process
			 (format "setvol %d\n" (min 100 (max 0 volume))))))

(defun mpdired--save-point ()
  (cond ((eq mpdired--view 'queue)
	 (let ((bol (mpdired--bol)))
	   (unless (> bol (point-max))
	     (setf mpdired--songid-point (get-text-property bol 'id)))))
	((eq mpdired--view 'browser)
	 (setf mpdired--browser-point (point)))))

(defun mpdired--next-line ()
  (forward-line)
  (goto-char (mpdired--bol)))

(defun mpdired-next-line ()
  "Go to next line and save position."
  (interactive)
  (mpdired--next-line)
  (mpdired--save-point))

(defun mpdired--previous-line ()
  (forward-line -1)
  (goto-char (mpdired--bol)))

(defun mpdired-previous-line ()
  "Go to previous line and save position."
  (interactive)
  (mpdired--previous-line)
  (mpdired--save-point))

(defun mpdired-listall-at-point ()
  (let* ((bol (mpdired--bol))
	 (type (get-text-property bol 'type))
	 (uri (get-text-property bol 'uri)))
    (cond ((eq type 'directory)
	   (mpdired-listall-internal uri))
	  ((eq type 'playlist)
	   (mpdired-listplaylist-internal uri))
	  ((eq type 'file)
	   (message "Cannot browse a file.")))))

(defun mpdired-playid-at-point ()
  (let ((id (get-text-property (mpdired--bol) 'id)))
    (when id (mpdired-playid-internal id))))

(defun mpdired-enter ()
  "In the browser view, browse the entry at point.
In the queue view, start playing the song at point."
  (interactive)
  (cond ((eq mpdired--view 'browser)
	 (setq mpdired--browser-point nil)
	 (mpdired-listall-at-point))
	((eq mpdired--view 'queue)
	 (mpdired-playid-at-point))))

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
  "Browse the parent directory of the current one."
  (interactive)
  (let ((parent (mpdired--parent)))
    (cond (parent
	   (setq mpdired--browser-point nil)
	   (with-current-buffer mpdired--comm-buffer
	     (setq mpdired--playlist nil))
	   (mpdired-listall-internal parent t))
	  (t (message "You are at the toplevel.")))))

(defun mpdired-toggle-view ()
  "Toggle between the browser and the queue view."
  (interactive)
  (cond ((eq mpdired--view 'browser)
	 (mpdired-queue-internal))
	((eq mpdired--view 'queue)
	 (cond (mpdired--directory
		(mpdired-listall-internal mpdired--directory)
		;; XXX this is a bit hacky
		(sit-for .1)
		;; If the buffer has only one header line then we may
		;; have been visiting a playlist.
		(when (= 1 (count-lines (point-min) (point-max)))
		  (mpdired-listplaylist-internal mpdired--directory))
		(sit-for .1)
		;; If the buffer is *still* a one liner then we may
		;; have a bogus `mpdired--directory': go back to
		;; toplevel.
		(when (= 1 (count-lines (point-min) (point-max)))
		  (mpdired-listall-internal "")))
	       (t (mpdired-listall-internal ""))))))

(defun mpdired--mark (mark)
  (let ((inhibit-read-only t))
    (when (get-text-property (mpdired--bol) 'uri)
      (save-excursion
	(goto-char (line-beginning-position))
	(delete-char 1)
	(insert-char mark))
      (put-text-property (mpdired--bol) (line-end-position) 'mark mark)
      (mpdired--reset-face))))

(defun mpdired--clear-mark ()
  (let ((inhibit-read-only t)
	(bol (mpdired--bol)))
    (when (get-text-property bol 'mark)
      (remove-text-properties (mpdired--bol) (line-end-position) '(mark face))
      (mpdired--reset-face)
      (save-excursion
	(goto-char (line-beginning-position))
	(delete-char 1)
	(insert-char ? )))))

(defun mpdired-mark-at-point ()
  "Mark entry at point."
  (interactive)
  (mpdired--mark ?*)
  (mpdired-next-line))

(defun mpdired-flag-at-point ()
  "Flag entry at point."
  (interactive)
  (mpdired--mark ?d)
  (mpdired-next-line))

(defun mpdired-toggle-marks ()
  "Toggle marks."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((max (point-max)))
      (while (< (point) max)
	(let ((mark (get-text-property (mpdired--bol) 'mark)))
	  (if (and mark (char-equal mark ?*))
	      (mpdired--clear-mark)
	    (mpdired--mark ?*)))
	(forward-line)))))

(defun mpdired-change-marks (&optional old new)
  "Change mark from OLD to NEW.  It asks the user for OLD and NEW."
  (interactive
   (let* ((cursor-in-echo-area t)
	  (old (progn (message "Change (old mark): ") (read-char)))
	  (new (progn (message  "Change %c marks to (new mark): " old)
		      (read-char))))
     (list old new)))
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (let ((max (point-max)))
	(while (< (point) max)
	  (let ((mark (get-text-property (mpdired--bol) 'mark)))
	    (when (and mark (char-equal mark old))
	      (mpdired--mark new)))
	  (forward-line))))))

(defun mpdired-unmark-at-point ()
  "Remove any mark at point."
  (interactive)
  (mpdired--clear-mark)
  (mpdired-next-line))

(defun mpdired-previous-unmark ()
  "Remove any mark on the previous line and move to it."
  (interactive)
  (mpdired-previous-line)
  (mpdired--clear-mark))

(defun mpdired-unmark-all-marks ()
  "Remove all marks in the current buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (let ((max (point-max)))
	(while (< (point) max)
	  (mpdired--clear-mark)
	  (forward-line))))))

(defun mpdired--collect-marked (want)
  "Collect entries marked with WANT."
  (save-excursion
    (goto-char (point-min))
    (let ((max (point-max))
	  result)
      (while (< (point) max)
	(let* ((bol (mpdired--bol))
	       (mark (get-text-property bol 'mark))
	       (id (get-text-property bol 'id))
	       (type (get-text-property bol 'type))
	       (uri (get-text-property bol 'uri)))
	  (when (and mark (char-equal mark want))
	    (push (cons id (cons type uri)) result)))
	(forward-line))
      (reverse result))))

(defun mpdired-mark-files-regexp (regexp &optional mark)
  "Mark entries which matches a user provided REGEXP."
  (interactive (list (read-regexp "Mark (regexp): ")))
  (save-excursion
    (goto-char (point-min))
    (let ((mark (or mark ?*))
	  (max (point-max)))
      (while (< (point) max)
	(when (re-search-forward regexp (line-end-position) t)
	  (mpdired--mark mark))
	(forward-line)))))

(defun mpdired-flag-files-regexp (regexp)
  "Flag entries which matches a user provided REGEXP."
  (interactive (list (read-regexp "Flag for deletion (regexp): ")))
  (mpdired-mark-files-regexp regexp ?d))

(defun mpdired--append-message (message)
  "Put a MESSAGE for the communication buffer."
  (with-current-buffer mpdired--comm-buffer
    (if mpdired--message
	(setq (format "%s %s" mpdired--message message))
      (setq mpdired--message message))))

(defun mpdired-add-at-point ()
  (let* ((bol (mpdired--bol))
	 (uri (get-text-property bol 'uri)))
    (when uri
      (mpdired--append-message (format "Adding %s..." uri))
      (mpdired-add-internal uri)
      (mpdired-next-line))))

(defun mpdired--build-add-message (typed-uris)
  (let* ((uris (mapcar 'cdr typed-uris))
	 (n (length uris)))
    (cond ((= n 1) (format "Adding \"%s\"..." (car uris)))
	  ((= n 2)
	   (format "Adding \"%s\" and \"%s\"..." (car uris) (cadr uris)))
	  ((> n 2)
	   (format "Adding \"%s\", \"%s\" and %d others..."
		   (car uris) (cadr uris) (- n 2))))))

(defun mpdired-add ()
  "Recursively add the entry at point at the end of the queue."
  (interactive)
  (let* ((marked (mpdired--collect-marked ?*))
	 (typed-uris (mapcar 'cdr marked)))
    (cond (typed-uris
	   (mpdired--append-message (mpdired--build-add-message typed-uris))
	   (mpdired-add-internal typed-uris))
	  (t (mpdired-add-at-point)))))

(defun mpdired-deleteid-at-point ()
  (let ((id (get-text-property (mpdired--bol) 'id)))
    (when id
      (save-excursion
	(forward-line)
	(let ((bol (mpdired--bol)))
	  (unless (>= bol (point-max))
	    (setq mpdired--songid-point
		  (get-text-property bol 'id)))))
      (mpdired-deleteid-internal id))))

(defun mpdired-delete ()
  "Remove song at point from the queue."
  (interactive)
  (cond ((eq mpdired--view 'queue)
	 (mpdired-deleteid-at-point))))

(defun mpdired--find-next-unmarked-id ()
  (save-excursion
    (let ((max (point-max)))
      (while (and (< (point) max)
		  (get-text-property (mpdired--bol) 'mark))
	(forward-line))
      (unless (>= (mpdired--bol) max)
	(get-text-property (mpdired--bol) 'id)))))

(defun mpdired-flagged-delete ()
  "Remove flagged songs from the queue."
  (interactive)
  (when (eq mpdired--view 'queue)
    (let* ((flagged (mpdired--collect-marked ?d))
	   (ids (mapcar 'car flagged)))
      (when flagged
	(setf mpdired--songid-point (mpdired--find-next-unmarked-id))
	(mpdired-deleteid-internal ids)))))

(defun mpdired-update ()
  "Update the buffer content.  It works both for browser and queue view."
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


;; Global commands (i.e. usable outside of the MPDired buffer).
(defun mpdired-pause ()
  "Toggle MPDired pause."
  (interactive)
  (let ((buffers (mpdired--prepare)))
    (mpdired-pause-internal (car buffers))))

(defun mpdired-next ()
  "Go to next song in the queue."
  (interactive)
  (let ((buffers (mpdired--prepare)))
    (mpdired-next-internal (car buffers))))

(defun mpdired-previous ()
  "Go to previous song in the queue."
  (interactive)
  (let ((buffers (mpdired--prepare)))
    (mpdired-previous-internal (car buffers))))

(defun mpdired-set-volume (volume)
  "Set MPDired volume."
  (interactive "nVolume: ")
  (let ((buffers (mpdired--prepare)))
    (mpdired-set-volume-internal (min 100 (max 0 volume)) (car buffers))))

;; Main entry point.
(defun mpdired ()
  "Open MPDired."
  (interactive)
  (let* ((buffers (mpdired--prepare))
	 (comm (car buffers))
	 (main (cdr buffers)))
    ;; Defaults to queue view
    (mpdired-queue-internal comm)
    (pop-to-buffer main)))

(provide 'mpdired)
;;; mpdired.el ends here
