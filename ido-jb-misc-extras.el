;;; ido-jb-misc-extras.el --- Miscellaneous extra ido related commands

;; Filename: ido-jb-misc-extras.el
;; Description: miscellaneous functions for `ido'
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2015, Joe Bloggs, all rites reversed.
;; Created: 2015-08-28 15:30:22
;; Version: 0.1
;; Last-Updated: 2015-08-28 15:30:22
;;           By: Joe Bloggs
;; URL: https://github.com/vapniks/ido-jb-misc-extras
;; Keywords: unix
;; Compatibility: GNU Emacs 24.5.1
;; Package-Requires: 
;;
;; Features that might be required by this library:
;;
;; run-assoc ido-choose-function
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: 
;;
;; Bitcoin donations gratefully accepted: 12k9zUo9Dgqk8Rary2cuzyvAQWD5EAuZ4q
;;
;; This library provides various miscellaneous `ido' related commands & functions
;; that I use occasionally. 
;;

;;; Commands:
;;
;; Below is a complete command list:
;;
;; `ido-execute-extended-command'
;;  Use `ido' to select a command to execute.
;; `ido-bookmark-jump'
;;  Switch to bookmark interactively using `ido'.
;; `ido-run-associated-program'
;;  Open the current candidate file with `run-associated-program'.
;;  `ido-goto-favourite'
;;  Choose commonly used file/dired buffer with ido, and jump to it.
;;  `ido-goto-recent-file'
;;  Choose recently used file with ido, and jump to it.
;;  `ido-goto-recent-dir'
;;  Choose recently used dired buffer with ido, and jump to it.
;;  `ido-cdargs'
;;  Choose cdargs bookmark and jump to corresponding directory.
;;  `ido-switch-to-cdargs-directory'
;;; Change to a cdargs bookmarked directory from ido minibuffer prompt.
;;
;; The following functions are defined:
;;
;; `ido-sort-mtime'
;;  Sort ido filelist by modification time instead of alphabetically.
;; `ido-completing-read-multiple'
;;  Read multiple items with `ido-completing-read'.
;;
;;; Customize:
;;
;; `ido-favourites-list'
;; List of choice-action pairs for use with the `ido-goto-favourite' command.
;; `ido-cdargs-config'
;; Location of cdargs config file.

;;; Installation:
;;
;; Put ido-jb-misc-extras.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'ido-jb-misc-extras)

;; To ensure files are sorted by modification time when using `ido-find-file',
;; put the following line somewhere in your startup file (~/.emacs):

;; (add-hook 'ido-make-file-list-hook 'ido-sort-mtime)



;;; Require
(eval-when-compile (require 'cl))
(require 'ido-choose-function)

;;;###autoload
(defun ido-run-associated-program nil
  "Open the current candidate file with `run-associated-program'."
  (interactive)
  (run-associated-program
   (concat ido-current-directory (car ido-matches))))

;;;###autoload
(defun ido-bookmark-jump (bname)
  "Switch to bookmark BNAME interactively using `ido'.

If bookmarks+ is installed and a prefix arg is provided then a list
of tags will be prompted for to filter the bookmarks at the next prompt.
With a single prefix bookmarks must match all tags, and with a double prefix
they only need match one of the tags."
  (interactive (let* ((tags (if current-prefix-arg
				(bmkp-read-tags-completing)))
		      (dsjnc (and current-prefix-arg
				  (= (prefix-numeric-value current-prefix-arg)
				     16)))
		      (bmks (if (and tags (featurep 'bookmark+))
				(bmkp-remove-if-not
				 (lambda (bmk)
				   (let ((bmktags (bmkp-get-tags bmk)))
				     (catch 'bmkp-b-mu-b-t-an
				       (dolist (tag tags)
					 (if dsjnc
					     (if (assoc-default tag (bmkp-get-tags bmk) nil t)
						 (throw 'bmkp-b-mu-b-t-an t))
					   (unless (assoc-default tag (bmkp-get-tags bmk) nil t)
					     (throw 'bmkp-b-mu-b-t-an nil))))
				       (if dsjnc nil t))))
				 bookmark-alist))))
		 (list (ido-completing-read
			"Bookmark: "
			(unless (and current-prefix-arg (not bmks))
			  (bookmark-all-names bmks))
			nil t))))
  (bookmark-jump bname))

(defvar ido-execute-command-cache nil)
;;;###autoload
(defun ido-execute-extended-command nil
  "Use `ido' to select a command to execute."
  (interactive)
  (call-interactively
   (intern
    (ido-completing-read
     "M-x "
     (progn
       (unless ido-execute-command-cache
	 (mapatoms (lambda (s)
		     (when (commandp s)
		       (setq ido-execute-command-cache
			     (cons (format "%S" s) ido-execute-command-cache))))))
       ido-execute-command-cache)))))

;;;###autoload
(defun ido-sort-mtime nil
  "Sort ido filelist by modification time instead of alphabetically."
  (if (not (or (equal "/" ido-current-directory)
	       (equal "/sudo:" ido-current-directory)
	       (equal "/su:" ido-current-directory)))
      (progn (setq ido-temp-list
		   (sort ido-temp-list
			 (lambda (a b)
			   (let* ((ta (nth 5 (file-attributes
					      (concat ido-current-directory a))))
				  (tb (nth 5 (file-attributes
					      (concat ido-current-directory b))))
				  (ta0 (nth 0 ta))
				  (tb0 (nth 0 tb))
				  (ta1 (nth 1 ta))
				  (tb1 (nth 1 tb)))
			     (if (not ta) nil
			       (if (not tb) t
				 (if (= ta0 tb0)
				     (> ta1 tb1)
				   (> ta0 tb0))))))))
	     (ido-to-end ;; move . files to end (again)
	      (delq nil (mapcar
			 (lambda (x) (if (string-equal (substring x 0 1) ".") x))
			 ido-temp-list))))))

;; need this variable for the next function
(defcustom ido-favourites-list nil
  "List of choice-action pairs for use with the `ido-goto-favourite' command.
     Each element should be a cons cell (NAME . COMMAND) where NAME is the name
     displayed in the ido prompt, and COMMAND is the command to be executed when
     NAME is selected."
  :type 'alist
  :group 'ido)

;;;###autoload
(defun ido-goto-favourite nil
  "Choose commonly used file/dired buffer with ido, and jump to it."
  (interactive)
  (funcall (ido-choose-function ido-favourites-list "Favourite: ")))

;;;###autoload
(defun ido-goto-recent-file (file)
  "Choose recently used FILE with ido, and jump to it."
  (interactive
   (list (let* ((filepaths (let ((items))
			     (dolist (item file-name-history)
			       (if (and (stringp item)
					(not (string-match ":" item))
					(file-regular-p item)
					(not (member item items)))
				   (add-to-list 'items item t)))
			     items))
		(filenames (mapcar 'file-name-nondirectory filepaths))
		(numfilenames (length filenames))
		;; get filename from user with ido
		(chosenfilename (ido-completing-read "Recent file: " filenames))
		(afterfilenameslist (member chosenfilename filenames))
		(posinlist (- numfilenames (length afterfilenameslist))))
	   (nth posinlist filepaths))))
  (find-file file))

;;;###autoload
(defun ido-goto-recent-dir (place)
  "Choose recently used directory (PLACE) with ido, and jump to it with dired."
  (interactive
   (list (ido-completing-read "Recent dir: "
			      (let ((items))
				(dolist (item file-name-history)
				  (if (and (stringp item)
					   (not (string-match ":" item))
					   (> (length item) 0))
				      (let ((itemd (file-name-directory item)))
					(if (and (stringp itemd)
						 (file-directory-p itemd)
						 (not (member itemd items)))
					    (add-to-list 'items itemd t)))))
				items))))
  (dired place))

(unless (not (require 'extract-text nil t))
  (defcustom ido-cdargs-config "~/.cdargs"
    "Location of cdargs config file.
Each line of the file must be a bookmark name followed by a space,
and then a filepath, e.g:  emacs ~/.emacs.d"
    :type 'file)
  (defun ido-cdargs-directory (bkmk)
    "Return the cdargs directory corresponding to bookmark BKMK.
If there is none then return nil."
    (with-temp-buffer
      (insert-file-contents ido-cdargs-config)
      (if (re-search-forward (concat "^" (regexp-opt (list bkmk)) " *\\(\\S-.*\\S-\\)\\s-*")
			     nil t)
	  (match-string 1))))
  
  (defun ido-cdargs (bkmk &optional findfile)
    "Choose subdir of cdargs bookmark directory.
BKMK is the name of the cdargs bookmark to use.
If called with prefix arg, or if FINDFILE is non-nil, then prompt 
for a file within the bookmarked directory, and open it.
Location of cdargs config file is stored in `ido-cdargs-config'."
    (interactive
     (list (ido-completing-read
	    "Directory bookmark: "
	    (let ((items))
	      (if (file-readable-p ido-cdargs-config)
		  (with-temp-buffer
		    (insert-file-contents ido-cdargs-config)
		    (setq extract-text-debugging nil)
		    (extract-text (regex "^\\w+") :REPS 1000 :ERROR 'skip))
		(error "Can't read cdargs config file: %s" ido-cdargs-config))))
	   current-prefix-arg))
    (if findfile
	(find-file (ido-read-file-name "File: " (ido-cdargs-directory bkmk) nil t))
      (ido-file-internal 'dired 'dired (ido-cdargs-directory bkmk) "Subdirectory: " 'dir nil nil)))

  ;; The following command can be bound to a key in `ido-file-dir-completion-map'
  (defun ido-switch-to-cdargs-directory ()
    "Change to a cdargs bookmarked directory from ido minibuffer prompt.
Use currently entered text as bookmark name, and switch to corresponding directory if there is one."
    (interactive)
    (let* ((dir (ido-cdargs-directory ido-text)))
      (when dir
	(ido-set-current-directory dir)
	(setq ido-exit 'refresh
	      ido-text-init nil
	      ido-rotate-temp t
	      ido-text nil)
	(exit-minibuffer)))))


;;;###autoload
(defun ido-completing-read-multiple (prompt choices
					    &optional predicate require-match
					    initial-input hist def sentinel)
  "Read multiple items with ido-completing-read.
Reading stops when the user enters SENTINEL. By default, SENTINEL is
\"*done*\". SENTINEL is disambiguated with clashing completions
by appending _ to SENTINEL until it becomes unique. So if there
are multiple values that look like SENTINEL, the one with the
most _ at the end is the actual sentinel value. See
documentation for `ido-completing-read' for details on the
other parameters."
  (let ((sentinel (if sentinel sentinel "*done*"))
	(done-reading nil)
	(res ()))
    ;; uniquify the SENTINEL value
    (while (find sentinel choices)
      (setq sentinel (concat sentinel "_")))
    (setq choices (cons sentinel choices))
    ;; read some choices
    (while (not done-reading)
      (setq this-choice (ido-completing-read prompt choices predicate require-match initial-input hist def))
      (if (equal this-choice sentinel)
	  (setq done-reading t)
	(setq res (cons this-choice res))))
    ;; return the result
    res))

;;;###autoload
(defun ido-display-buffer-right (&optional left)
  "Split the current window horizontally and display a buffer in the other half.
Buffer name is selected using ido.
If LEFT is non-nil, or command is called with a prefix arg, then put new buffer
in the left hand side window instead of the right hand side window."
  (interactive "P")
  (let* ((require-match (confirm-nonexistent-file-or-buffer))
	 (buf (ido-read-internal 'buffer "Buffer: " 'ido-buffer-history nil
				 require-match nil)))
    (split-window-right)
    (unless left (other-window -1))
    (switch-to-buffer buf nil t)
    (other-window 1)))

;; Redefine `ido-restrict-to-matches' so that application with a prefix arg
;; will remove matches from the current list.
(eval-after-load "ido.elc"
  '(defun ido-restrict-to-matches (&optional arg)
     "Set current item list to the currently matched items.
If a prefix ARG is used then remove matched items from list."
     (interactive "P")
     (when ido-matches
       (setq ido-cur-list
	     (if arg (cl-set-difference
		      ido-cur-list ido-matches :test 'equal)
	       ido-matches)
	     ido-text-init ""
	     ido-rescan (if arg t)
	     ido-exit 'keep)
       (if arg (setq ido-matches ido-cur-list))
       (exit-minibuffer))))

(provide 'ido-jb-misc-extras)

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "ido-jb-misc-extras.el" (buffer-name) (buffer-string) "update")

;;; ido-jb-misc-extras.el ends here
