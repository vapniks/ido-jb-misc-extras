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
;; Package-Requires: ((ido-choose-function "20151021") (run-assoc "20180725"))
;;
;; Features that might be required by this library: cl-lib, wid-edit, cus-edit
;;
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
(require 'jb-misc-macros)
(require 'cl-lib)
(require 'wid-edit)
(require 'cus-edit) ;; some types (hook, etc.) live here

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


;; Extra prompting functions for customization types
(defsubst ido--cus-get-tag (w)
  (and (widgetp w) (consp w) (memq :tag w)
       (widget-get w :tag)))

;;;###autoload
(defun ido--list-prompt-value (widget prompt value unbound)
  (let ((args (widget-get widget :args))
	(listprompt (or (ido--cus-get-tag widget) prompt)))
    (cl-flet ((promptusr (c j)
		(widget-prompt-value c (format "%s [%d] %s: "
					       listprompt j
					       (or (ido--cus-get-tag c) ""))
				     (nth (1- j) value) unbound)))
      (cl-loop for child in args
               for i from 1
	       for pos = (and (listp child)
			      (cl-position :inline child))
	       if (and pos (nth (1+ pos) child))
	       append (promptusr child i)
	       else collect (promptusr child i)))))

;;;###autoload
(defun ido--vector-prompt-value (widget prompt value unbound)
  (vconcat (ido--list-prompt-value widget prompt (append value nil) unbound)))

;;;###autoload
(defun ido--repeat-prompt-value (widget prompt value unbound)
  (let* ((child (or (car (widget-get widget :args)) 'sexp))
	 (parentprompt (or (ido--cus-get-tag widget) prompt))
	 (childprompt (or (ido--cus-get-tag child) parentprompt))
         (n (read-number (format "No. of elements for %s: " parentprompt)
                         (and (sequencep value) (length value)))))
    (cl-loop for i from 1 upto n
             collect (widget-prompt-value child (format "%s [%d]: " parentprompt i)
					  (nth (1- i) value) unbound))))

;;;###autoload
(defun ido--cons-prompt-value (widget prompt value unbound)
  (let ((args (widget-get widget :args))
	(parentprompt (or (ido--cus-get-tag widget) prompt)))
    (cons (let ((w (or (car args) 'sexp)))
	    (widget-prompt-value w (format "%s: %s " parentprompt
					   (or (ido--cus-get-tag w) "car"))
				 (car value) unbound))
          (let ((w (or (cadr args) 'sexp)))
	    (widget-prompt-value w (format "%s: %s " parentprompt
					   (or (ido--cus-get-tag w) "cdr"))
				 (cdr value) unbound)))))

;;;###autoload
(defun ido--set-prompt-value (widget prompt _value _unbound)
  (cl-loop for child in (widget-get widget :args)
	   for pos = (and (listp child)
			  (cl-position :inline child))
	   if (and pos (nth (1+ pos) child))
	   append (widget-prompt-value child prompt nil t)
	   else when (y-or-n-p (format "Include %s? "
				       (or (widget-get child :tag)
					   (prin1-to-string child))))
           collect (widget-prompt-value child prompt nil t)))

;;;###autoload
(defun ido--alist-prompt-value (widget prompt value unbound)
  (let* ((kt (or (widget-get widget :key-type) 'sexp))
	 (ktag (ido--cus-get-tag kt))
         (vt (or (widget-get widget :value-type) 'sexp))
	 (vtag (ido--cus-get-tag vt))
         (n (read-number (format "No. of entries for %s: "
				 (or (ido--cus-get-tag widget) prompt))
                         (and (listp value) (length value)))))
    (cl-loop for i from 1 upto n
             collect (cons (widget-prompt-value kt (format "%s [%d]: " (or ktag "Key") i)
						(car (nth (1- i) value)) unbound)
                           (widget-prompt-value vt (format "%s [%d]: " (or vtag "Value") i)
						(cdr (nth (1- i) value)) unbound)))))

;; Note: don't be tempted to try and account for :inline items in choice widgets
;;  the final `widget-prompt-value' call will give an error about mismatching types.
;;;###autoload
(defun ido--choice-prompt-value (widget prompt value unbound)
  (let* ((args (widget-get widget :args))
	 (tag (ido--cus-get-tag widget))
         (items
          (let ((seen (make-hash-table :test 'equal)))
            (mapcar (lambda (child)
		      (let* ((tag  (ido--cus-get-tag child))
			     (type (widget-type child))
			     (base (cond (tag)
					 ((memq type '(const function-item variable-item))
					  (format "%s" (widget-get child :value)))
					 (t (format "%s" type))))
			     (display base)
			     (n 2))
			;; Ensure unique display strings (two consts with no tag)
			(while (gethash display seen)
			  (setq display (format "%s [%d]" base n))
			  (cl-incf n))
			(puthash display t seen)
			(cons display child)))
		    args)))
         (display-strings (mapcar #'car items))
         (default (when (and value (not unbound))
                    (car (cl-find-if (lambda (item) (widget-apply (cdr item) :match value))
				     items))))
         (chosen (ido-completing-read (format "%s: " (or tag prompt))
				      display-strings nil t nil
				      (widget-get widget :history)
				      default))
	 (chosedef (string= chosen default)))
    ;; Recursively prompt for the chosen alternative's value
    (widget-prompt-value (cdr (assoc chosen items)) prompt
			 (when chosedef value)
			 (or unbound (not chosedef)))))

;;;###autoload
(defun ido--plist-prompt-value (widget prompt value unbound)
  (let* ((key-type   (or (widget-get widget :key-type)  'symbol))
	 (ktag       (ido--cus-get-tag key-type))
         (value-type (or (widget-get widget :value-type) 'sexp))
	 (vtag       (ido--cus-get-tag value-type))
         (cur-len    (and (plistp value) (/ (length value) 2)))
	 (tag        (ido--cus-get-tag widget))
         (count      (read-number (format "No. of key-value pairs for %s: "
					  (or tag prompt))
				  (or cur-len 0)))
         result)
    (dotimes (i count)
      (let ((k (widget-prompt-value key-type (format "%s [%d]: " (or ktag "Key") (1+ i))
				    (nth (* i 2) value) unbound))
            (v (widget-prompt-value value-type (format "%s [%d]: " (or vtag "Value") (1+ i))
				    (nth (1+ (* i 2)) value) unbound)))
        (setq result (plist-put result k v))))
    result))

;; Install previously defined functions into associated customization type symbols
(dolist (type '((list     . ido--list-prompt-value)
                (group    . ido--list-prompt-value)
		(vector   . ido--vector-prompt-value)
                (repeat   . ido--repeat-prompt-value)
                (cons     . ido--cons-prompt-value)
                (set      . ido--set-prompt-value)
                (alist    . ido--alist-prompt-value)
		(choice   . ido--choice-prompt-value)
		(radio    . ido--choice-prompt-value)
		(plist    . ido--plist-prompt-value)))
  (let ((def (get (car type) 'widget-type)))
    (when def
      (plist-put (cdr def) :prompt-value (cdr type)))))
;;;###autoload
(cl-defun ido-custom-prompt-variable (prompt-var prompt-val &optional comment)
  "Use `ido' to prompt for a user option variable and value and return them as a list.
PROMPT-VAR is the prompt for the variable, and PROMPT-VAL is the prompt for the value.
The %s escape in PROMPT-VAL is replaced with the name of the variable.

If the variable has a `variable-interactive' property, that is used as if
it were the arg to `interactive' (which see) to interactively read the value.

If the variable has a `custom-type' property, it must be a widget and the
`:prompt-value' property of that widget will be used for reading the value.
If the variable also has a `custom-get' property, that is used for finding
the current value of the variable, otherwise `symbol-value' is used.

If optional COMMENT argument is non-nil, also prompt for a comment and return
it as the third element in the list."
  (let* ((vars (cl-loop for sym being the symbols
			if (get sym 'custom-type)
			collect (symbol-name sym)))
	 (sap (symbol-name (symbol-at-point)))
	 (var (intern-soft (ido-completing-read
			    "User option: " vars
			    nil t nil nil (when (member sap vars) sap))))
	 (minibuffer-help-form `(describe-variable ',var))
	 ;;(other-window-scroll-buffer)
	 (val
	  (let ((prop (get var 'variable-interactive))
		(type (get var 'custom-type))
		(prompt (format prompt-val var)))
            (setq type (ensure-list type))
	    (cond (prop (call-interactively
			 `(lambda (arg) (interactive ,prop) arg)))
		  (type (widget-prompt-value
			 type prompt
			 (if (boundp var)
			     (funcall (or (get var 'custom-get) 'symbol-value) var))
			 (not (boundp var))))
		  (t (eval-minibuffer prompt))))))
    (if comment
 	(list var val (read-string "Comment: " (get var 'variable-comment)))
      (list var val))))
;;;###autoload
(defun ido-customize-save-variable (variable value &optional comment) 	;ido version of `customize-save-variable'
  "Like `customize-save-variable', but using `ido-custom-prompt-variable' to prompt the user."
  (interactive (ido-custom-prompt-variable "Set and save variable: "
					   "Set and save value for %s as: "
					   current-prefix-arg))
  (customize-save-variable variable value comment))
;;;###autoload
(defun ido-customize-set-variable (variable value &optional comment) 	;ido version of `customize-save-variable'
  "Like `customize-set-variable', but using `ido-custom-prompt-variable' to prompt the user."  
  (interactive (ido-custom-prompt-variable "Set and save variable: "
					   "Set and save value for %s as: "
					   current-prefix-arg))
  (customize-set-variable variable value comment))

;;;###autoload
(defun ido-choose-from-alist--internal (options alter new delete)
  (let* ((descriptions (mapcar #'car options))
	 (choice (when options (ido-completing-read
				(let ((parts (delq nil
						   (list (and new (format "%s to create new item" new))
							 (and alter (format "%s to alter item" alter))
							 (and delete (format "%s to delete item" delete))))))
				  (format "Choose option%s: "
					  (if parts (format " (%s)" (string-join parts ", ")) "")))
				(append descriptions (when alter (list alter)) (when new (list new))
					(when delete (list delete)))
				nil t)))
	 (alterp (equal choice alter)))
    (let* ((description (if (member choice (list alter delete))
			    (ido-completing-read (format "Choose option to %s: "
							 (if alter "alter" "delete"))
						 descriptions nil t)))
	   (newdescription (if (member choice (list alter new))
			       (cl-loop with olddesc = (if (equal choice alter) description)
					with disallowed = (remove olddesc descriptions)
					for d = (read-string "New description (different to others): " olddesc)
					while (member d disallowed)
					finally return d))))
      (list choice description newdescription))))

;;;###autoload
(defmacro ido-choose-from-alist (options &optional allowalter allownew allowdelete inputspec)
  "Prompt to choose an option from an alist, delete, alter, or add a new one, set and optionally save it.
OPTIONS is an alist or a symbol pointing to an alist, whose cars are descriptions, and whose cdrs are values
of any type. The user chooses one of the descriptions, and the corresponding value is returned.

If ALLOWALTER is non-nil then the user may also select \"ALTER\" (or a string provided in the ALLOWALTER arg)
and then select an item to alter. If ALLOWNEW is non-nil then they can select \"NEW\" (or a string provided
in the ALLOWNEW arg), and add a new item. If ALLOWDELETE is non-nil and OPTIONS is a symbol, then they can
select \"DELETE\" (or a string provided in the ALLOWDELETE arg), and choose an item to delete.
When altering or adding a new item, if OPTIONS is a customizable user option then `widget-prompt-value' will be
used to prompt for a value, unless the INPUTSPEC arg is supplied. INPUTSPEC should always be supplied when OPTIONS
is not a customizable user option symbol and ALLOWALTER or ALLOWNEW is non-nil.
INPUTSPEC can either be a customization type/widget specifying the data type of the value to be prompted
for, e.g. '(list integer string) or a function that prompts the user for a value and returns it.
In the latter case the function will be passed a single argument; the current value of the option being altered,
or if a new item is being added then the default value (if there is one) or nil (if not).

When ALTER, NEW or DELETE are chosen, if OPTIONS is the symbol of a customizable variable it will be set to the
new value, and the user will be asked if they want to save it."
  (cl-with-gensyms (optsym optval new alter delete choice description newdescription currentval newval)
    `(with-symbol-and-value-bindings
      ((,options ,optsym ,optval))
      (let* ((,new (and ,allownew (if (stringp ,allownew) ,allownew "NEW")))
	     (,alter (and ,allowalter (if (stringp ,allowalter) ,allowalter "ALTER")))
	     (,delete (and ,allowdelete (if (stringp ,allowdelete) ,allowdelete "DELETE")))
	     ,currentval ,newval)
	(cl-destructuring-bind (,choice ,description ,newdescription)
	    (ido-choose-from-alist--internal ,optval ,alter ,new ,delete)
	  (if (not (member ,choice (list ,alter ,new ,delete)))
	      (cdr (assoc ,choice ,optval))
	    (if (equal ,choice ,delete)
		(setq ,optval (assoc-delete-all ,description ,optval))
	      (setq ,currentval (cdr (assoc ,description ,optval))
		    ,newval (if (functionp ,inputspec)
				(funcall ,inputspec ,currentval)
			      ;; TODO use `type-of' &/or `cl-typecase' to reconstruct custom-type of options when its not a symbol 
			      ;; so that we can use `widget-prompt-value' in those cases too.
			      ;; This should be a separate function/macro.
			      (if (or ,inputspec (custom-variable-p ,optsym))
				  (widget-prompt-value (or ,inputspec
							   (cadr (memq :value-type (get ,optsym 'custom-type))))
						       "" ,currentval (string= ,choice ,new))
				(read-from-minibuffer "New value: " (if (equal ,choice ,alter) (format "%s" ,currentval))
						      nil t))))
	      (when ,optsym
		(if ,description ;; alter item
		    (let ((item (assoc ,description ,optval)))
		      (setcar item ,newdescription)
		      (setcdr item ,newval))
		  ;; add new item
		  (setq ,optval (add-to-list ,optsym (cons ,newdescription ,newval))))))
	    (when (custom-variable-p ,optsym)
	      (customize-set-variable ,optsym ,optval)
	      (if (y-or-n-p (format "Save new value of `%s'? " (symbol-name ,optsym)))
		  (customize-save-variable ,optsym ,optval)))
	    ,newval))))))

;;;###autoload
(defun ido-jb-show-help nil
  "Show ido keybindings in the *Help* buffer.
Bind this to a key in `ido-common-completion-map',
e.g. (define-key ido-common-completion-map (kbd \"C-h\") 'ido-jb-show-help)"
  (interactive)
  (let ((win (selected-window)))
    (describe-keymap 'ido-completion-map)
    (select-window win)))

(provide 'ido-jb-misc-extras)

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "ido-jb-misc-extras.el" (buffer-name) (buffer-string) "update")

;;; ido-jb-misc-extras.el ends here
