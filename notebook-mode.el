;; $Id$
;; Set up a notebook mode.  

;;;
;; A notebook is a mostly text file with a few input/output cells.
;; It has a sub-process which the input cells are sent to and which
;; returns output cells.  
;; 
;; The default notebook uses the shell bash.
;; See also matlab.el and mupad.el which use matlab and mupad as their
;; subprocesses.
;;;

;; Put this file, and it's companions, in one of the dirctories in the Emacs
;; search path, then add the following to your .emacs:
;; (add-to-list 'auto-mode-alist ("\\.mu$" . mupad-notebook-mode))
;; (add-to-list 'auto-mode-alist ("\\.matlab$" . matlab-notebook-mode))
;; (add-to-list 'auto-mode-alist ("\\.M$" . matlab-notebook-mode))
;; (add-to-list 'auto-mode-alist ("\\.shell$" . notebook-mode))

;; Then add the following:
;; (autoload 'mupad-notebook-mode "mupad-notebook-mode" "Notebook for mupad" t)
;; (autoload 'matlab-notebook-mode "matlab-notebook-mode" "Notebook for matlab" t)
;; (autoload 'octave-notebook-mode "octave-notebook-mode" "Notebook for octave" t)
;; (autoload 'notebook-mode "notebook-mode" "Notebook for matlab" t)


(provide 'notebook-mode)
(require 'notebook-mode)		;needed for byte compiler.

(require 'sh-script)


;;; Variables that are user configurable, or user readable.

;; Note to myself: change these here and in Makefile -- FDGC
(defconst notebook-mode-version 2
  "The version number of this copy of notebook mode.")
(defconst notebook-mode-version-minor 1
  "The minor version number of this copy of notebook mode.")

;;
(defvar notebook-mode-hook nil
  "If this is non-nil, it is the hook that's run for any notebook.")
(defvar notebook-default-text-mode
  (or (cdr (assoc "\\.tex$" auto-mode-alist)) 'tex-mode)
  "The default base-mode to use for text regions in a new notebook.")
(defvar notebook-text-font-lock-hook nil
  "Run this before initializing font lock for the text region.")
(defvar notebook-input-font-lock-hook nil
  "Run this before initializing font lock for the input region.")
(defvar notebook-output-font-lock-hook nil
  "Run this before initializing font lock for the output region.")


;;; Variables that are used internally, or can be set by various notebook modes.

(defconst nb-adjust-input-string
  (lambda (string buffer name)
    ;; (scratch (format "Adjusting %s from cell %s.\n" string name))
    (replace-regexp-in-string
     "^\\s-*;" "" 			; remove blank lines with semicolons.
     (replace-regexp-in-string
      "\\s-*;\\(\\s-*;\\)*" ";"	     ;replace several semicolons with only one.
      (concat "echo -e \"\\fBegin \\f" buffer "\\f " name "\";" string
	      "; echo -e \"\\fEnd " name "\";\n")))
    )
  "A function which adjusts an input string so that it can be sent to
the process.  It is called with STRING, BUFFER, and NAME.
STRING is the string which should be sent.  BUFFER is the name of the
notebook buffer. NAME is the name of the input cell.  Output from
the process will have to be recognized by NAME and BUFFER.")
(make-variable-buffer-local 'nb-adjust-input-string)

;;
(defconst nb-adjust-output-string
  (lambda (i-beg i-end o-beg o-end cell)
    (save-excursion
      (goto-char o-beg)
      (if (looking-at " *\b") ()
	(insert "\n"))
      ))
  "A function which adjusts an output string that came from 
the process.  It is called with INPUT-START, INPUT-END
 OUTPUT-START, OUTPUT-END, and CELL.  It should edit
the current buffer between START and END.  CELL is the name of the cell. ")
(make-variable-buffer-local 'nb-adjust-output-string)

(defvar nb-extra-cell-initialize nil
  "A hook which is called after a cell is initialized.  It's only argument is the
cell itself."
  )
(make-variable-buffer-local 'nb-extra-cell-initialize)

(defvar nb-extra-cell-delete nil
  "A hook which is called before a cell's data is removed from the cell list.
It's only argument is the cell itself.  This is done after the cells text has
been removed from the buffer."
  )
(make-variable-buffer-local 'nb-extra-cell-delete)

;;
(defvar nb-syntax-table nil
  "Used as local syntax table for font lock in notebooks.")
(defconst nb-display-table (make-display-table)
  "The display table for buffer.")
(aset nb-display-table 12 (vector 124))	; A bar for the special character
(aset nb-display-table 8 (vector 124))	; A bar for the special character
;;
(defvar nb-cell-list nil
  "A list of cells for this buffer.")
(make-variable-buffer-local 'nb-cell-list)

(defconst nb-process nil "The notebook process.")
(make-variable-buffer-local 'nb-process)

;;
(setq process-environment (cons "PAGER=cat" process-environment))

;;
(defconst nb-start-process
  (lambda (&optional old)
    (if (not nb-process)
	(setq nb-process
	      (start-process
	       (buffer-name)		; name of process
	       (format "*%s-process*" (buffer-name)) ; process buffer.
	       "bash")))		; Program name.
    (process-kill-without-query nb-process)
    (set-process-filter nb-process 'nb-filter)
    ;; (process-send-string nb-process
    ;;        "alias ls='ls --no-color'\n")
    (process-send-string nb-process "PS1=\"\"\n")
    (process-send-string nb-process "export TERM=unknown\n")
    (process-send-string nb-process "PROMPT_COMMAND=\"\"\n")
    (setq mode-line-process ": run")
    (save-excursion
      (let ((oldbuf (current-buffer))
	    (regexp nb-output-regexp)
	    )
	(set-buffer (get-buffer-create 
		     (format "*%s-process*" (buffer-name)))) ; process buffer.
	(setq mode-line-process ": %s")
	(setq nb-output-regexp regexp))
      ))
  "This variable is called in order to start a new process for the
current buffer. If it is passed an argument of true, it should try to find a
currently running process. (This doesn't work for the shell notebook.)"
  )
(make-variable-buffer-local 'nb-start-process)

;;
(defun nb-find-process (name)
  "Find a process named NAME that is already running. If there is none,
return nil."
  (let ((list (process-list))		;Get a list of processes.
	(proc) (process nil) (command))
    (while (and list (not process))		;Look at each one.
      (setq proc (car list))
      (setq command (process-name proc))
      (if (string-match name command)
	  (setq process proc))
      (setq list (cdr list))
      )
    process				; Return the result of the search.
    )
  )




;; Here are some variables that make up regular expressions.
(let ( (name "\\([^ \t\n\b\f>]*\\)")	; Possible name regular expression.
       (ws "\\s *")			; Whitespace.
       (body "\\([^\b\f]*\\)")		; Body of input or output region.
       )

  (defconst nb-cell-regexp 
    (concat "\b\\(" name ">>?\\)"	; Prompt and name.
	    body  "\b"   body "\b"	; input and output.
	    )
    "This is the regular expression which matches a i/o cell.
The first character should be unusual, since this is sometimes
used in searches.
The first set of parenthesis should match the prompt.
The second set should match either the cell name, or an empty string.
The third should match the input part of the cell.
The fourth should match the output part of the cell.

It is hard coded in part of the code that a byte of 0x07 is signals a
boundary for this input.")

  (make-variable-buffer-local 'nb-cell-regexp)


  ;; Note: The default has changed in version 2.0: the cell name is no longer
  ;; embedded into the prompt.
  (defconst nb-empty-cell-format "\b>>  \b  --no output yet-- \b\n"
    "This is inserted as an initial empty cell.  It can use the name of the
cell if it wishes (e.g. \"\bin(%s) =  \n< >\b\").  "
    )

  (make-variable-buffer-local 'nb-empty-cell-format)

  (defconst nb-output-regexp 
    (concat
     "\fBegin \f"			; A keywork
     body "\f" ws name ws "\n"		; The buffer name, and cell name.
     body				; The useful part of the output.
     ws "\fEnd" ws  
     "\\2" )				; The name should be at the end.
    "The regular expression which matches the output from the
process.
The name of the buffer should match the first set of parentheses.
The name of the cell should match the second pair of parentheses.
The third set should match the useful part of the output.

This should not use 0x07 to signal a boundary, since that is
used for the input. A form feed works nicely as a specail marker.
")

  (make-variable-buffer-local 'nb-output-regexp)

  )



;;; Cell Manipulation.

;; A cell is a list of
;; (  a name (or number)
;;    a status, one of: 'entered, 'unentered, 'processed, 'error
;;    a begining marker.
;;    an overlay for the prompt. 
;;    an overlay for the input. 
;;    an overlay for the output.
;; )
(defun nb-cell-name (cell) (if cell (car cell) nil))
(defun nb-cell-status (cell) (nth 1 cell))
(defun nb-cell-begin (cell) (nth 2 cell))
(defun nb-cell-prompt-overlay (cell) (nth 3 cell))
(defun nb-cell-input-overlay (cell) (nth 4 cell))
(defun nb-cell-output-overlay (cell) (nth 5 cell))
;;
(defun nb-set-cell-status (cell status)
  "Set the cells status, and change the prompt's color, too."
  (setcar (nthcdr 1 cell) status)
  (overlay-put (nb-cell-prompt-overlay cell)
	       'face
	       (cond ((equal status 'entered)  'notebook-entered-face)
		     ((equal status 'unentered)  'notebook-unentered-face)
		     ((equal status 'processed)  'notebook-processed-face)
		     ((equal status 'error)  'notebook-error-face)
		     ))
  )
;;
(defun nb-find-cell-by-name (name)
  "Search the cell list for the cell named NAME cell."
  ;;(scratch (format "Trying to find cell %s\n" name))
  (let ((result ()) (list nb-cell-list))
    (while list
      (if (not (equal name (nb-cell-name (car list))))
	  (setq list (cdr list))	;if not equal.
	(setq result (car list))	;if equal.
	(setq list ())			;terminate loop. 
	)
      )
    result				;return result from the loop.
    ) )
;;
(defun nb-find-cell-by-position (pos &optional strict)
  "Find the cell which contains the position POS.
If strict is nil, it finds the cell before or containing POS, otherwise
it finds the cell strictly containing POS. It returns nil if there is no
such cell.
It uses (looking-at) to set match data just before returning."
  (interactive "d")
  (let ((cell) (magic-character (substring nb-cell-regexp 0 1)) (overlays))
    ;;(scratch (format "Looking for cell at %d.  " pos))
    (save-excursion
      (goto-char pos)
      (while (and (not (looking-at nb-cell-regexp))
		  (search-backward magic-character nil t)
		  ))
      (setq overlays (overlays-at (point)))
      (while (and overlays (not cell))
	(setq cell (overlay-get (car overlays) 'cell))
	(setq overlays (cdr overlays)))
      
      (if (not cell)			;if no cell was found.
	  (if (looking-at nb-cell-regexp)
	      ;; This is probably an error: looking at a cell that is not on
	      ;; the list.  So we will just add it quietly to the list:
	      (setq cell (nb-initialize-one-cell (point)))
	    ()			      ; Not looking at a cell.  return quietly.
	    )				; return null if no cell found.
	(looking-at nb-cell-regexp)
	;;(scratch (format ") %s is from %d to %d. "
	;;		 (nb-cell-name cell) (match-beginning 0) (match-end 0)))
	(if (and strict (> pos (match-end 0))) ; not before the end of cell...
	    (setq cell nil)
	  )
	;;(scratch (format "cell-by-pos is %s.\n" (nb-cell-name cell)))
	cell				;return the result.
	))))
;;
(defun nb-cell-part-by-position (pos)
  "Returns the part of a cell that POS is in.  It can either be
the tokens 'prompt, 'input, 'output, or nil, if the position is either
in the prompt, the input region, the output region of a cell, or not in
a cell at all.
"
  (interactive "d")
  (let ((overlays (overlays-at pos)) (type nil))
    (while (and overlays (not type))
      (setq type (overlay-get (car overlays) 'cell-part))
      (setq overlays (cdr overlays)))
    type
    ))
;;


;;; Notebook initialization code. 

(defun notebook-mode-initialize (&optional input-mode output-mode)
  "This initializes notebook-mode and its variants."
  ;; (message "Initializing notebook mode.") 
  (put 'funny-mode 'mode-class 'special) ; This mode uses special text.
  (setq buffer-display-table nb-display-table) ; Use brackets for characters.
  (setq nb-cell-list nil)
  (nb-initialize-cells (point-min) (point-max))
  (if nb-process
      (setq mode-line-process (format ": %s" (process-status nb-process)))
    (setq mode-line-process ": no proc.")
    )
  (setq paragraph-start			;A paragraph starts with:
	(concat paragraph-start		; what ever it did before.
		"\\|"  (char-to-string
			(aref nb-cell-regexp 0)) ; A cell can start a paragraph.
		))
  (setq paragraph-separate		; But it can't separate one.
	(concat paragraph-separate
		))
  ;; Now, set up the font-lock mode.
  (if nb-use-font-lock (nb-font-lock-setup input-mode output-mode))
  ;; PENDING: This shouldn't be turned on unless font-lock-global something is
  ;; PENDING: set.  
  )

;;
(defun nb-setup-keymap (keymap)
  (define-key keymap "\n"     'newline)
  (define-key keymap "\e\t"     'nb-next-cell)
  (define-key keymap [C-return] 'nb-send-input)
  (define-key keymap [M-return] 'nb-send-input-forward)
  (define-key keymap [H-return] 'nb-send-input-and-create)
  (define-key keymap "\C-c\C-r" 'nb-send-input-region)
  (define-key keymap "\C-c\C-b" 'nb-send-input-buffer)
  (define-key keymap "\C-c\C-s" 'nb-start-process)
  (define-key keymap "\C-c\C-c" 'nb-kill-process)
  (define-key keymap "\C-c\C-n"     'nb-create-cell)
  (define-key keymap "\C-c\C-d"     'nb-delete-cell-and-text)
  (define-key keymap "\C-c-"     'nb-clear-output-buffer)
  (define-key keymap "\C-y"     'nb-yank)
  (define-key keymap "\ey"     'nb-yank-pop)
  (define-key keymap "\C-c>"     'nb-toggle-prompt)
  (define-key keymap "\C-c\C-f" nb-text-mode-converter)
  (if (and (not (lookup-key keymap "\C-c\C-v"))
	   (functionp 'TeX-command-master))
    (define-key keymap "\C-c\C-v" 'TeX-command-master)) ; 
    
  (define-key keymap
    [menu-bar notebook] (cons "Notebook" (make-sparse-keymap "Notebook")))
  (define-key keymap [menu-bar notebook notebook-to-plain-text]
    '("Create Plain Text File From Notebook" . notebook-to-plain-text))
  (define-key keymap [menu-bar notebook notebook-to-html]
    '("Create HTML File From Notebook" . notebook-to-html))
  (define-key keymap [menu-bar notebook notebook-to-tex]
    '("Create TeX File From Notebook" . notebook-to-tex))
  (define-key keymap [menu-bar notebook notebook-to-script]
    '("Create Shell Script" . notebook-to-script))
  (define-key keymap [menu-bar notebook nb-delete-cell-and-text]
    '("Delete Cell" . nb-delete-cell-and-text))
  (define-key keymap [menu-bar notebook nb-create-cell]
    '("New Cell" . nb-create-cell))
  (define-key keymap [menu-bar notebook nb-kill-process]
    '("Kill Process" . nb-kill-process))
  (define-key keymap [menu-bar notebook nb-start-process ]
    '("Start a New Process" . nb-start-process))
  (define-key keymap [menu-bar notebook nb-send-input-buffer]
    '("Send Buffer to Process" . nb-send-input-buffer))
  (define-key keymap [menu-bar notebook nb-send-input-region]
    '("Send Region to Process" . nb-send-input-region))
  (define-key keymap [menu-bar notebook nb-send-input]
    '("Send Cell to Process" . nb-send-input))
  (define-key keymap [menu-bar notebook nb-clear-output-buffer]
    '("Clear all output in buffer" . nb-clear-output-buffer))
  keymap
  )


;;; Cell creation and initialziation.

(defun nb-initialize-cells (beg end)
  "Create cell data for all of the i/o cells that are between BEG and END."
  (if (< end beg)
      (let ((temp beg))			;Swap beg and end.
	(setq beg end)
	(setq end beg))
    )
  ;; (message "Initializing cells between %d and %d." beg end)
  (save-excursion
    (goto-char beg)
    (while (re-search-forward nb-cell-regexp end t)
      (nb-initialize-one-cell (match-beginning 0)))
    ))
      
;;
(defun nb-initialize-one-cell (pos)
  "Create cell data for the cell at position POS.  POS must be exactly at
the start of the cell text.  The new cell is returned."
  ;;(scratch (format "Creating a new cell at %d.\n" pos))
  (save-excursion
    (goto-char pos)
    (if (not (looking-at nb-cell-regexp))
	(error (format  "Text at %d doesn't look like an I/O cell." pos))
      ;; Now we are looking at a cell. Create data for it.
      (let ((name (buffer-substring
		   (match-beginning 2) (match-end 2)))
	    (beg (make-marker))
	    (prompt (make-overlay	; The prompt overlay goes
		     (match-beginning 0) ; from the start of the cell, 
		     (match-beginning 3))) ; to just before the input.
	    (input  (make-overlay	; The input overlay goes
		     (match-beginning 3) ;from the beginning of the input,
		     (match-end 3)))	; to the end of the input.
	    (output (make-overlay	; The output overlay goes
		     (match-end 3)	; from just after the input, 
		     (match-end 0)))	; to the end of the cell.
	    (cell)
	    )
	(if (equal name "")		; If the name isn't imbedded in the
					; prompt, create a new one.
	    (setq name (funcall nb-name-new-cell ))
	  ;; If it has a name, check that there isn't alread a cell with
	  ;; this name.  If there is, give this one a new name.
	  (if (nb-find-cell-by-name name)
	      (progn
		(goto-char (match-beginning 2)) ; Delete the old name.
		(delete-region (match-beginning 2) (match-end 2))
		(setq name (funcall nb-name-new-cell))
		(insert name)))
	  ) ;; Done checking name.
	(set-marker beg pos)		; Set the beginning point.
	
					; Make the cell, and put it on the
					; list.
	(setq cell (list name 'unentered beg prompt input output))
	(setq nb-cell-list (cons cell nb-cell-list))
	(nb-set-cell-status cell 'unentered) ; adjust the colors of the cell.
	
	(unless nb-use-font-lock
	  (overlay-put input 'face 'notebook-input-face)
	  (overlay-put output 'face 'notebook-output-face))
	;; Use the following so that the overlays can keep track of changes
	;; in their text.
	;;; PENDING -- check that prompt can change
	;;; PENDING -- before adding this: (overlay-put prompt 'intangible t)
	(overlay-put prompt 'modification-hooks 
		     (nb-make-hook 'nb-modify-prompt cell))
	(overlay-put prompt 'insert-in-front-hooks
		     (nb-make-hook 'nb-insert-in-front-prompt cell))
	(overlay-put input 'modification-hooks 
		     (nb-make-hook 'nb-modify-input cell))
	(overlay-put input 'insert-in-front-hooks
		     (nb-make-hook 'nb-modify-input cell))
	(overlay-put input 'insert-behind-hooks 
		     (nb-make-hook 'nb-insert-behind-input cell))
	(overlay-put output 'modification-hooks 
		     (nb-make-hook 'nb-modify-output cell))
	(overlay-put output 'insert-in-front-hooks 
		     (nb-make-hook 'nb-insert-in-front-output cell))
	(overlay-put prompt 'cell-part 'prompt) 
	(overlay-put  input 'cell-part  'input) 
	(overlay-put output 'cell-part 'output) 
	(overlay-put prompt 'cell cell) 
	(overlay-put  input 'cell cell) 
	(overlay-put output 'cell cell) 
	(run-hook-with-args 'nb-extra-cell-initialize cell)
	cell				;return the cell.
	)
      ))
  )

;; The following function is a convoluted way to generate a function with
;; the cell built in.  It is passed as an argument to one of the three
;; main functions.
(defun nb-make-hook (which-function cell)
  "Make a hook for the overlay."
  (list (list				; Make a list of lists.
	 'lambda			;The head is lambda.
	 '(o after beg end &optional len) ; These are the variables.
	 (list which-function
	       'o 'after 'beg 'end
	       (list 'quote cell)
	       'len))
	))
;;
(defun nb-create-cell (pos)		
  "Create a new cell at position POS.  Text is inserted, and point moved
to the beginning of the input portion.  If point is already in a cell, a new cell
is created on the following line."
  (interactive "d")
  ;; First, position point at POS.  However, we must make sure that we
  ;; don't try to embed a new cell into an old one.
  ;;(scratch "Creating new cell:\n")
  (if (or (not (nb-find-cell-by-position pos t)) ; If not in a cell,
	  (equal pos (match-beginning 0))) ; or at the very start of a
					; cell .
      (goto-char pos)			; Goto the given position.
    ;;(scratch (format "Trying to insert into cell %s.\n"
    ;;	     (nb-cell-name (nb-find-cell-by-position pos t))))
    (goto-char (match-end 0))		;otherwise Goto to the end of the cell,
    (forward-line 1)			; And then go to the next line.
    (if (nb-find-cell-by-position (point) t) ; If we've moved to a new cell,
	(goto-char (match-beginning 0))) ; then go to it's start.
    
    )
  (setq pos (point))
  ;;(scratch "Position found. Inserting text.\n")
  (insert-before-markers		; Insert text for a new cell.
   (format nb-empty-cell-format		
	   (funcall nb-name-new-cell)))
  (let ((cell (nb-initialize-one-cell pos))) ; Create cell data for this cell.
    (goto-char pos)			; Go back to the start.
    (looking-at nb-cell-regexp)		; Find the match data.
    (goto-char (match-beginning 3)) ; Leave point at the start of the input data.
    (save-excursion
      (indent-according-to-mode))      ; Finally, leave this cell looking nice.
    cell))				; return the newly created cell. 

;;
(defconst nb-name-new-cell (lambda () (nb-default-name-new-cell))
  "This is called to pick a name for a newly generated cell.")
(make-variable-buffer-local 'nb-name-new-cell)
;;
(defconst nb-next-cell-number 1
  "The current number of the input cell.")
(make-variable-buffer-local 'nb-next-cell-number)
;;
(defun nb-default-name-new-cell ()
  (while (nb-find-cell-by-name (format "%d" nb-next-cell-number))
    (setq nb-next-cell-number (+ 1 nb-next-cell-number)))
  (format "%d" nb-next-cell-number))
;;



;;; Font Lock code.

(defconst nb-use-font-lock
  (boundp 'font-lock-multiline)		; this is set in new versions of font-lock.
  "This is true if notebook modes should use font lock on the input/output regions."
  )

(defun nb-font-lock-setup (input-mode output-mode)
  "Turn on the alternate fontlock mode for a notebook."
;;    (message "font-lock-setup mod=%s in=%s out=%s" (buffer-modified-p)
;;  	   input-mode output-mode)
  (add-hook (make-local-variable 'change-major-mode-hook) 'nb-turnoff-font-lock)
  (if (not (equal font-lock-fontify-region-function 'nb-fontify-region))
      (set (make-local-variable 'original-fontify)
	   font-lock-fontify-region-function)) ;save the original fontify function.
  (set (make-local-variable 'font-lock-fontify-region-function)
       'nb-fontify-region)
  (set (make-local-variable 'font-lock-beginning-of-syntax-function)
       'nb-font-lock-beginning-of-syntax)

  ;; Set up font-lock for the text, input, and output regions.
  ;; ----------------------text regions: 
  (set (make-local-variable 'nb-text-font-lock-data)
       (nb-font-lock-set-defaults nil notebook-text-font-lock-hook nil nil))
  ;; (message "nb-text-data found.")

  ;; ----------------------input regions: 
  (set (make-local-variable 'nb-input-font-lock-data)
       (nb-font-lock-set-defaults input-mode notebook-input-font-lock-hook
				  'notebook-input-face t))
  ;; (message "found nb-input-data")

  ;; ----------------------- output regions:
  (set (make-local-variable 'nb-output-font-lock-data)
       (nb-font-lock-set-defaults output-mode notebook-output-font-lock-hook
				  'notebook-output-face t))
  ;; (message "found nb-output-data")

  ;; put back the original data.
  (nb-restore nb-font-lock-variables nb-text-font-lock-data)
  ;; Remove the old fontification.
  (font-lock-unfontify-region (point-min) (point-max))
  ;; Use the new fontification.
  ;;(font-lock-fontify-region (point-min) (point-max))
  (font-lock-mode 1)			;PENDING: put somewhere else???
  ;; (message "end font-lock-setup mod=%s" (buffer-modified-p))
  )
;;
(defun nb-font-lock-set-defaults ( hook1 hook2 face own-buffer)
  "Have font-lock set the defaults for the alternate set of variables."
  (save-excursion
    (let ((temp-buffer (if own-buffer (generate-new-buffer "notebook-temp-buf")))
	  (data))
      (if own-buffer (set-buffer temp-buffer))
      ;; (message "temp-buffer = %s, hook = %s %s" temp-buffer hook1 hook2)
      (setq font-lock-set-defaults nil)	; tell font-lock it hasn't set defaults yet.
      (setq nb-background-face face)
      ;; (message "Running hooks %s %s" hook1 hook2)
      (run-hooks 'hook1 'hook2)
      ;; (message "Done with hooks")
      (unless nb-syntax-table (setq nb-syntax-table (syntax-table)))
      ;; (message "Setting the defaults....")
      (font-lock-mode t)
      ;; (message "after set-defaults") 
      ;; (message "font-lock-keywords = %S" font-lock-keywords)
      (setq data (nb-store nb-font-lock-variables))
      (if own-buffer (kill-buffer temp-buffer))
      ;; (message "Done with kill buffer")
      data)))
;;
(defun nb-turnoff-font-lock ()
  "Turn off the special font-lock handling for notebook."
  (interactive)
  (save-excursion
    (font-lock-unfontify-region (point-min) (point-max))
    (setq font-lock-fontify-region-function original-fontify)
    (font-lock-fontify-region (point-min) (point-max))
    )
  )

;; This looks for the two keywords
(defun nb-fontify-region (beg end &optional loudly)
  "This is called by font-lock mode to fontify a region.  It uses the
default, and it uses `nb-fontify-cell-part' for fontifying input/output." 
  ;; (message "My fontify-region  %d %d mod=%s" beg end (buffer-modified-p))
  (let ((old-data (nb-store nb-font-lock-variables))
	(old-syntax (syntax-table))
	;;(inhibited (nb-store nb-save-vars)) 
	(modified (buffer-modified-p)) )
    (unwind-protect 
	(save-restriction
	  (widen)
	  (goto-char beg)
	  ;; (nb-restore nb-save-vars nb-new-vals)
	  (font-lock-unfontify-region beg end)
	  (let ((subend) min max (cell (nb-find-cell-by-position beg nil))
		;; 'type' is what part of a cell we are currently in. 
		(type (nb-cell-part-by-position beg)) )
	    (if cell
		(setq min (+ 1 (overlay-end (nb-cell-output-overlay cell))))
	      (setq min (point-min)))
	    (while (< beg end)
	      ;; (message "looping with beg = %d" beg)
  	      ;; If we are in the input part, also do the prompt part (just in case)
	      ;; *** First do the prompt part of the cell.
	      (when (equal type 'prompt)
		;; (put-text-property (overlay-start (nb-cell-prompt-overlay cell))
		;; (overlay-end (nb-cell-prompt-overlay cell))
		;;    'fontified t)
		(setq beg (overlay-start (nb-cell-input-overlay cell)))
		(setq type 'input))
	      ;; *** Second do the input part of the cell.
	      (when (and (< beg end) (equal type 'input))
		(setq min (overlay-start (nb-cell-input-overlay cell)))
		(setq max (overlay-end (nb-cell-input-overlay cell)))
		(setq subend (min max end))
		;; (message "input at  %d %d -- %d %d" min beg subend max)
		(setq nb-input-font-lock-data
		      (nb-fontify-cell-part beg subend min max
					    nb-input-font-lock-data loudly))
		(setq beg subend)
		(setq type 'output)
		)
	      ;; *** Third do the output part of the cell.
	      (when (and (< beg end) (equal type 'output))
		(setq min (overlay-start (nb-cell-output-overlay cell)))
		(setq beg (max beg min))
		(setq max (overlay-end (nb-cell-output-overlay cell)))
		(setq subend (min max end))
		;; (message "output at  %d %d -- %d %d" min beg subend max)
		(setq nb-output-font-lock-data
		      (nb-fontify-cell-part beg subend min max
					    nb-output-font-lock-data loudly))
		(setq min beg) ;; use this as minimum for text in step 4.
		)
	      ;; *** Forth do the text between the cells.
	      (setq cell (nb-next-cell beg 1 t)) ;get the next cell.
	      (if cell ;; find the maximum end of this region.
		  (setq max (overlay-end (nb-cell-prompt-overlay cell)))
		(setq max (point-max)))
	      (setq subend (min end max)) ; subend is where to stop.
	      ;; Now use the original font-lock-region function on what's left.
	      ;; (message "text at  %d %d -- %d %d" min beg subend max)
	      (if (< beg subend)
		  (setq nb-text-font-lock-data
			(nb-fontify-cell-part beg subend min max
					      nb-text-font-lock-data loudly)))
	      (setq beg subend)
	      (setq type 'prompt) ;; Anything left starts with a prompt.
	      ))
	  )
      (if (and (not modified) (buffer-modified-p))
	  (set-buffer-modified-p nil))
      (nb-restore nb-font-lock-variables old-data)
      ;; (nb-restore nb-save-vars inhibited)  
      (set-syntax-table old-syntax)
      ))
  ;; (debug-syntax-and-face "end of fontify-region" (syntax-table))
  )

;;
(defun nb-fontify-cell-part (beg end min max data &optional loudly)
  "This uses the other keywords to fontify. DATA stores some of the variables
that are used by font-lock.  MIN and MAX give the range that should be used.
Most of this is taken from `font-lock-default-fontify-region'.
"
  ;; First, use the correct font lock variables.
  (narrow-to-region min max)
  (nb-restore nb-font-lock-variables data)
  ;; (debug-syntax-and-face "nb table: " nb-syntax-table)
  ;; (debug-syntax-and-face "font-lock table: " font-lock-syntax-table)

  (with-syntax-table (or font-lock-syntax-table
			 nb-syntax-table)
    ;; check to see if we should expand the beg/end area for
    ;; proper multiline matches
    (when (and font-lock-multiline
	       (> beg min)
	       (get-text-property (1- beg) 'font-lock-multiline))
      ;; We are just after or in a multiline match.
      (setq beg (or (previous-single-property-change beg 'font-lock-multiline)
		    min))
      (goto-char beg)
      (setq beg (line-beginning-position)))
    (setq beg (max min beg))
    (when font-lock-multiline
      (setq end (or (text-property-any end (point-max)
				       'font-lock-multiline nil)
		    max)))
    (goto-char end)
    (setq end (min max (line-beginning-position 2)))
    ;;(debug-syntax-and-face (format "fontify-cell-part beg = %d, end=%d table: "
    ;;beg end) (syntax-table))

    ;; Now do the fontification.
    (when font-lock-syntactic-keywords
      (font-lock-fontify-syntactic-keywords-region beg end)
      ;; (debug-syntax-and-face "after keywords table: " (syntax-table))
      )
    (unless font-lock-keywords-only
      (font-lock-fontify-syntactically-region beg end loudly))
    (font-lock-fontify-keywords-region beg end loudly)
    ;; add the background:
    ;; (debug-syntax-and-face  "almost end of cell part: " (syntax-table) )
    (when nb-background-face
      (font-lock-append-text-property beg end 'face nb-background-face))
    )
  ;; return the alterante variables, in case they were changed.
  (widen)
  ;; (debug-syntax-and-face  "very end of cell part: " (syntax-table) )
  ;; This has to be last, since it is the return value of this function.
  (nb-store nb-font-lock-variables)   ; return the new values of the variables.
  )

;; This is the list of all the variables that need to be saved.
;; I hope I got them all...
(defconst nb-font-lock-variables
  '( major-mode
     font-lock-set-defaults
     font-lock-cache-state font-lock-cache-position
     font-lock-fontified
     font-lock-multiline font-lock-keywords font-lock-keywords-only
     font-lock-keywords-case-fold-search font-lock-syntax-table
     font-lock-syntactic-keywords
     nb-background-face
     nb-syntax-table
     )
  "The list of variables that are saved when we use the alternate font-lock"
  )

(defun nb-font-lock-beginning-of-syntax ()
  "Go to the beginning of the cell part.  Only works during fontification."
  (goto-char (point-min)))

;; It's functions like these that make me love lisp. Isn't this fun?
;; PENDING; maybe these are built in somewhere? I should probably check...
(defun nb-store (names)
  "Make a data list of the variables in the list NAMES."
  (if names
      (cons (symbol-value (car names)) (nb-store (cdr names)))
    ()))
;;
(defun nb-restore (names data)
  "Restore the data in DATA to the variables in NAMES."
  (when names
    (set (car names) (car data))
    (nb-restore (cdr names) (cdr data))
    ))


;;; Fontlock faces and pretty colors
(defgroup notebook-faces nil
  "Faces for notebook mode."
  :group 'faces)
(defface notebook-input-face
  '((((type tty) (class color)) (:background "blue"))
    (((class color) (background light)) (:background "light blue"))
    (((class color) (background dark))  (:background "dark blue"))
    )
  "Face for the input region of a cell in notebook mode."
  :group 'notebook-faces)
(defface notebook-output-face
  '((((type tty) (class color)) (:foreground "cyan"))
    (((class color) (background light)) (:foreground "dark cyan"))
    (((class color) (background dark)) (:foreground "cyan"))
    )
  "Face for the output region of a cell in notebook mode."
  :group 'notebook-faces)
(defface notebook-unentered-face
  '((((type tty) (class color)) (:foreground "orange"))
    (((class color) (background light))
     (:foreground "dark green" :background "light orange"))
    (((class color) (background dark))
     (:foreground "light green" :background "dark orange"))
    )
  "Face for the prompt region of a cell in notebook mode.
Used for cells that have not yet been entered."
  :group 'notebook-faces)
(defface notebook-entered-face
  '((((type tty) (class color)) (:foreground "yellow"))
    (((class color) (background light))
     (:foreground "white"  :background "dark green"))
    (((class color) (background dark))
     (:foreground "black"  :background "light green"))
    )
  "Face for the prompt region of a cell in notebook mode.
Used for cells that have been entered, but had no output yet."
  :group 'notebook-faces)
(defface notebook-processed-face
  '((((type tty) (class color)) (:foreground "green"))
    (((class color) (background light))
     (:foreground "dark green"  :background "yellow"))
    (((class color) (background dark))
     (:foreground "light green"  :background "dark green"))
    )
  "Face for the prompt region of a cell in notebook mode.
Used for cells that have been processed."
  :group 'notebook-faces)

(defface notebook-error-face
  '((((type tty) (class color)) (:foreground "red"))
    (((class color) (background light))
     (:foreground "white"  :background "red"))
    (((class color) (background dark))
     (:foreground "white"  :background "red"))
    )
  "Face for the prompt region of a cell in notebook mode.
Used for cells that have had an error state returned."
  :group 'notebook-faces)


;;; Cleaning output.

(defun nb-clear-output-buffer ()
  "Erase all the output of all cells in the buffer."
  (interactive)
  (nb-clear-output-region (point-min) (point-max))
  )
;;
(defun nb-clear-output-region (beg end)
  "Send all of the cells in the region to the process."
  (interactive "r")
  (if (< end beg)
      (let ((temp beg))			;Swap beg and end.
	(setq beg end)
	(setq end temp))
    )
  (save-excursion
    (goto-char beg)
    (while (re-search-forward nb-cell-regexp end t)
      (delete-region (match-beginning 4) (match-end 4))
      )
    ))

;;; Interaction with sub-process.

(defun nb-send-input-forward (position &optional arg)
  "Send the current input cell and move point to the next."
  (interactive "d\np")
  (if (not arg) (setq arg 1))
  (goto-char position)
  (while (< 0 arg)
    (nb-send-input (point))
    (nb-next-cell (point))
    (setq arg (1- arg))
    ))
;;
(defun nb-send-input-and-create (position)
  "Send the current input cell and create a new cell on the next line."
  (interactive "d")
  (goto-char position)
  (nb-send-input (point))
  (nb-create-cell (point))
  )
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun nb-send-input-buffer ()
  "Send all of the cells in the buffer to the process."
  (interactive)
  (nb-send-input-region (point-min) (point-max))
  )
;;
(defun nb-send-input-region (beg end)
  "Send all of the cells in the region to the process."
  (interactive "r")
  (if (< end beg)
      (let ((temp beg))			;Swap beg and end.
	(setq beg end)
	(setq end temp))
    )
  (save-excursion
    (goto-char beg)
    (while (re-search-forward nb-cell-regexp end t)
      (nb-send-input (point))))
  )
;;
;;
(defun nb-send-input (position)
  "Send the input cell at or before POSITION.  point isn't moved.
If there is no cell at or before this position, nothing is sent."
  (interactive "d")
  (save-excursion
    (if (or (not nb-process)
	    (not (equal 'run (process-status nb-process))) )
	(funcall nb-start-process t))
    (let ( (string) (cell (nb-find-cell-by-position position nil)))
      (if cell
	  (progn
	    (setq string
		  (buffer-substring (match-beginning 3) (match-end 3)))
	    ;;(scratch (format "Sending %s from cell '%s'.\n"
	    ;;	     string (nb-cell-name cell)))
	    (setq string  (if nb-adjust-input-string
			      (funcall nb-adjust-input-string string
				       (buffer-name) (nb-cell-name cell))
			    (string)))
	    (if (boundp 'notebook-debug-input-buffer)
		(save-excursion
		  (set-buffer notebook-debug-input-buffer)
		  (goto-char (point-max))
		  (insert (concat "<<" string ">>"))
		  ))			; end debugging statements.
	    (process-send-string nb-process string)
	    (nb-set-cell-status cell 'entered)
	    ))))
  )


(defconst nb-preprocess-output nil
  "A function hook which adjusts an output string before it is inserted into
the buffer. If this variable is nil, no preprocessing is done.
It is called with the parameters (NAME STRING) where NAME is the name of the cell
and STRING is the output.  If it returns nil, the output is inserted as usual,
otherwise the output is ignored.
")
(make-variable-buffer-local 'nb-preprocess-output)

(defun nb-insert-output (buffer name string)
  "Put STRING in the cell named NAME, in the buffer BUFFER."
  (save-excursion			; Return to proces buffer afterwards.
    (set-buffer buffer)
    (unless (and nb-preprocess-output
	     (funcall nb-preprocess-output name string))
	
      (save-excursion			; Save current point and mark.
	(let ((cell (nb-find-cell-by-name name))
	      )
	  (if (equal cell nil)
	      (if (equal name "error") (message string)) ;Put error as a message. 
	    ;; Otherwise, insert output.
	    ;; First, change the status, if it was entered before.
	    ;; This doesn't change the status if it was an error or unentered.
	    ;; This way, if a cell is entered, then it is changed, then its output
	    ;; is returned, the status will still be unentered.
	    (if (equal (nb-cell-status cell) 'entered)
		(nb-set-cell-status cell 'processed))
	    (goto-char (nb-cell-begin cell))
	    (if (not (looking-at nb-cell-regexp))
		(error "Cell %s doesn't look like an I/O cell." name))
	    (goto-char (match-beginning 4))
	    (delete-region (match-beginning 4) (match-end 4))
	    (insert string)
	    (if nb-adjust-output-string
		(funcall nb-adjust-output-string
			 (match-beginning 3)
			 (match-end 3)
			 (match-beginning 4)
			 (+ (match-beginning 4) (length string))
			 cell ))
	    ))))))
;;
(defun nb-start-process (&optional old)
  "This function is called in order to start a new process for the
current buffer. If it is passed an argument of true, it should try to find a
currently running process. (This doesn't work for the shell notebook.)"
  (interactive)
  (funcall nb-start-process old)
  )
;;
(defun nb-kill-process ()
  "Kill the current process."
  (interactive)
  (if  nb-process
      (let ((buff (process-buffer nb-process)))
	(delete-process nb-process)
	(kill-buffer buff)
	(setq nb-process nil))
    )
  (setq mode-line-process ": no proc.")
  )
;;
(defun nb-filter (proc string)
  (let ((old-buffer (current-buffer)))
    (unwind-protect
	(let ((beg) (end))
	  (if (boundp 'notebook-debug-output-buffer)
	      (save-excursion
		(set-buffer notebook-debug-output-buffer)
		(goto-char (point-max))
		(insert string)
		))
	  (set-buffer (process-buffer proc)) ;Go to the processes buffer.
	  (save-excursion
	    (goto-char (point-max))
	    (insert string)
	    (goto-char (point-min))
	    ;;(scratch "Deleting control characters from output.\n")
	    (while (search-forward-regexp "[^\b]\b" nil t)
	      (delete-region (match-beginning 0) (match-end 0))
	      (if (> (point) (point-min)) (forward-char -1)))
	    (goto-char (point-min))
	    ;;(scratch (format "Got output. Looking for '%s'.\n"
	    ;;	     nb-output-regexp))
	    (while (search-forward-regexp nb-output-regexp nil t)
	      ;;(scratch "found some.")
	      (setq beg (match-beginning 0))
	      (setq end (match-end 0))
	      (nb-insert-output
	       (buffer-substring (match-beginning 1) (match-end 1))
	       (buffer-substring (match-beginning 2) (match-end 2))
	       (buffer-substring (match-beginning 3) (match-end 3)) )
	      (delete-region (point-min) end))
	    ))
      (set-buffer old-buffer)
      ))) 
       
;;
(defun nb-delete-lines (beg string &optional bound noerror)
  "Delete the lines in string from the buffer."
  ;;(scratch "Deleting input lines from output.\n")
  (let ((start 0) (line) )
    (setq string (concat string "\n"))
    ;;(scratch (format "string = '%s'" string))
    ;; Find the next line in string.  First strip of all whitespace at
    ;; the beginning, then strip off all white space at the end.
    (while (and (< start (length string)) ; 
		(string-match
		 "\\s *\\(.*\\)\n"
		 string start))
      (let ((beg (match-beginning 1)) (end (match-end 1)))
	(if (string-match "\\s *\n" string start) ; strip of whitespace at end.
	    (setq end (match-beginning 0)))
	(if (< beg end)
	    (setq line (substring string beg end))
	  (setq line "\n")
	  )
	)
      (setq start (match-end 0))
      ;;(scratch (format "working on line '%s' (start=%d)\n" line start))
      (goto-char beg)
      (if (search-forward line bound noerror)
	  (progn
	    ;;(scratch (format "deleting at %d\n" (point)))
	    (delete-region (- (point) (length line)) (point)))
	)
      ))
  )


;;; Movement.

(defun nb-next-cell (position &optional count no-create)
  "Goto the the beginning of the cell after POSITION.
COUNT is the number of times to repeat this. If there is no next cell,
one is created.
However, if no-create is t, then it moves to point-max.
It returns the cell, or nil if no cell was found." 
  (interactive "d\np")
  (goto-char position)
  (if (null count)
      (setq count 1))  
  (if (and (nb-find-cell-by-position position nil) ;If we are in a cell.
	   (> (match-beginning 3) position)) ; And before the input part.
      (goto-char (match-beginning 0)))	; Include this cell in the search.
  ;; Then, search forward.
  (if (re-search-forward nb-cell-regexp nil t count)
      (nb-find-cell-by-position (goto-char (match-beginning 3)) nil) ;return cell.
    ;; Otherwise, Go as far forward as we can, and create a new one.
    (while (re-search-forward nb-cell-regexp nil t)
      (goto-char (+ 1 (match-end 0))))
    ;;
    (if no-create
	(progn
	  (goto-char (point-max))
	  ()				;return nil.
	  )
      (forward-line 1)
      (nb-create-cell (point))		;return the new cell.
      )
    )
  )


;;; Hooks called by emacs when part of cell is modified by user.



(defun nb-modify-prompt (overlay after beg end cell &optional len)
  "This is called when a prompt overlay is changed."
  ;; (message "running nb-modify-prompt %s (%d-%d)" after beg end)
  (if after
      (nb-delete-cell-data cell)	; Only modification that can be done.
    (if (or  (> beg (nb-cell-begin cell))
	     (< end (overlay-end (nb-cell-output-overlay cell)))
	     )
	(error "Notebook prompt is read only."))))
;;
(defun nb-insert-in-front-prompt (overlay after beg end cell &optional len)
  "Insert in front of prompt hook."
  ;; (message "running nb-insert-in-front-prompt %s (%d-%d)" after beg end)
  (if after
      (progn
	(move-overlay overlay end	;Move the start of the prompt.
		      (overlay-end overlay)) ; Keep the end the same.
	(set-marker (nb-cell-begin cell) end) ; Move the start of the cell.
	)
    ))
;;
(defun nb-modify-input (overlay after beg end cell &optional len)
  "This is called when an input overlay is changed."
  ;;(message "running nb-modify-input %s (%d-%d) font=%s"
  ;;	   after beg end font-lock-fontify-region-function)
  (if (and after
	   (not (equal 'unentered (nb-cell-status cell))))
      (nb-set-cell-status cell 'unentered))
  )
;;
(defun nb-insert-behind-input (overlay after beg end cell &optional len)
  "Insert behind an input overlay hook."
  ;; (message "running nb-insert-behind-input %s (%d-%d)" after beg end)
  (if (and after
	   (not (equal 'unentered (nb-cell-status cell))))
      (nb-set-cell-status cell 'unentered))
  (if after
      (move-overlay overlay	       
		    (overlay-start overlay) ; Don't move the beginning.
		    end)		; Move the end.
    )
  )

;; PENDING: play with this.  Maybe delete across boundaries,
;; but put the marker back in place???
(defun nb-modify-output (overlay after beg end cell &optional len)
  "This is called when an output overlay is changed."
  ;; (message "running nb-modify-output %s (%d-%d)" after beg end)
  (if (not after)
      ;; The modification must be over the whole cell (a deletion, handled
      ;; by modify-prompt) or it should only cover the actual output and
      ;; not any stray control characters.
      (save-excursion
	(goto-char (nb-cell-begin cell))
	(looking-at nb-cell-regexp)
	(if (not (or			; not either
		  (and (<= beg (point)) ; all of cell...
		       (>= end (match-end 0)))
		  (and (>= beg (match-beginning 4)) ; or only output part.
		       (<= end (match-end 4)))
		  ))
	    (error "Can not modify boundary between input and output.")
	  ))
    )
  )
;;
(defun nb-insert-in-front-output (overlay after beg end cell &optional len)
  "Insert in front of output hook."
  ;; (message "running nb-insert-in-front-output %s (%d-%d)" after beg end)
  (if after
      (move-overlay overlay end		;Move the start of the output.
		    (overlay-end overlay)) ; Keep the end the same.
    ))
;;


;;; Deleting and yanking a cell.

(defun nb-delete-cell-and-text (pos)
  "Delete the cell text and data at position POS"
  (interactive "d")
  (let ((cell (nb-find-cell-by-position pos t)))
    (if cell
	(progn 
	  (delete-region (nb-cell-begin cell) ; Delete the text.
			 (overlay-end (nb-cell-output-overlay cell)))
	  (nb-delete-cell-data cell)
	  )
      (error (format "Position %d is not in an I/O cell." pos)))
    ))
;;
(defun nb-delete-cell-data (cell)
  "Delete the data for this cell."
  (if (not cell)
      ()
    (run-hook-with-args 'nb-extra-cell-delete cell)
    (delete-overlay (nb-cell-prompt-overlay cell))
    (delete-overlay (nb-cell-input-overlay cell))
    (delete-overlay (nb-cell-output-overlay cell))
    (setq nb-cell-list (delete cell nb-cell-list))
    ))
;;
(defun nb-yank (&optional arg)
  "Like yank, but it also sets up data structures for any cells."
  (interactive "p")
  (yank arg)
  (nb-initialize-cells (mark) (point)))
;;
(defun nb-yank-pop (&optional arg)
  "Like yank-pop, but it also sets up data structures for any cells."
  (interactive "p")
  (if (not arg) (setq arg 1))
  (delete-region (point) (mark))
  (nb-yank (+ 1 arg))
  )
;;

;;; Text mode utilities.

(defvar nb-text-mode-converter 'notebook-to-plain-text 
  "The choosen text mode for this notebook.  Set by \\[nb-choose-text-mode].")
(make-variable-buffer-local 'nb-text-mode-converter)


(defun nb-choose-text-mode ()
  "Choose the text mode for the current buffer."
  ;; This is copied modified from the routine \\[tex-mode].
	      
  (if (equal (buffer-size) 0)
      (progn 
	(funcall notebook-default-text-mode)
	(setq nb-text-mode-converter
	      (cond ((eq notebook-default-text-mode
			 (or (cdr (assoc "\\.tex$" auto-mode-alist)) 'tex-mode))
		     'notebook-to-tex)
		    ((eq notebook-default-text-mode
			 (or (cdr (assoc "\\.html$" auto-mode-alist)) 'html-mode))
		     'notebook-to-html)
		    (t 'notebook-to-plain-text)
		    ))
	)
    (save-excursion
      (goto-char (point-min))
      (cond ((re-search-forward		; see if it is a TeX file.
	      ;; This should probably be cleaner. 
	      (concat "\\\\documentclass"  "\\|" "\\\\begin"  "\\\\end")
	      nil t)
	     ;; (message "I am using tex mode  %s."
	     ;;      (or (cdr (assoc "\\.tex$" auto-mode-alist)) 'tex-mode))
	     (funcall (or (cdr (assoc "\\.tex$" auto-mode-alist)) 'tex-mode))
	     (setq nb-text-mode-converter 'notebook-to-tex))
	    ((re-search-forward "<html>\\|<body>" nil t) ; now see if it is html
	     ;;(message "I am using html mode %s."
	     ;; (or (cdr (assoc "\\.html$" auto-mode-alist)) 'html-helper-mode))
	     (funcall (or (cdr (assoc "\\.html$" auto-mode-alist))
			 'html-helper-mode))
	     (setq nb-text-mode-converter 'notebook-to-html))
	    (t
	     ;; (message "I am using plain text mode.")
	     (text-mode)		;default to plain text mode.
	     (setq nb-text-mode-converter 'notebook-to-plain-text))
	    ))))


(defun nb-prompt-short ()
  "Set the default to be short prompts."
  (interactive)
  (setq nb-empty-cell-format "\b>   \b --no output yet-- \b\n")
  )

(defun nb-prompt-long ()
  "Set the default to be long prompts."
  (interactive)
  (setq nb-empty-cell-format "\b>>  \b --no output yet-- \b\n")
  )

(defun nb-toggle-prompt (pos)
  "Toggle the prompt of the cell at POS to be either long or short."
  (interactive "d")
  (save-excursion
    (let ((cell (nb-find-cell-by-position pos nil)) (prompt) )
      (if (equal cell nil) ()		; return if no cell.
	(goto-char (nb-cell-begin cell))
	(if (not (looking-at nb-cell-regexp))
	    (error "Cell %s doesn't look like an I/O cell." name))
	(setq prompt (nb-cell-prompt-overlay cell))
	(goto-char (match-beginning 1))
	(overlay-put prompt 'modification-hooks  nil)
	(if (looking-at ">>") 		; do the actual change.
	    (delete-char 1)
	  (insert ">>")
	  (delete-char (-  (match-end 1) (match-beginning 1)))
	  )
	(overlay-put prompt 'modification-hooks 
		     (nb-make-hook 'nb-modify-prompt cell))
	)))
  )

(defun notebook-to-tex ()
  "Convert this file to an tex file."
  (interactive)
  (let ((notebook-name (buffer-file-name))
	(notebook-buffer (current-buffer))
	(file-dir (file-name-directory (buffer-file-name)))
	(tex-name)
	(cell-regexp nb-cell-regexp)	; Keep track of the local variable.
	(tex-buffer) )
    (setq tex-name (concat (file-name-sans-extension notebook-name) ".tex"))
    (setq tex-buffer (find-file-noselect tex-name))
    (save-excursion
      (set-buffer tex-buffer)
      (delete-region (point-min) (point-max))
      (insert-buffer-substring notebook-buffer)
      (goto-char (point-min))		; Get rid of special characters
      (while (re-search-forward cell-regexp nil t)
	(let ((prompt  (buffer-substring
			(match-beginning 1) (match-end 1)))
	      (name   (buffer-substring
		       (match-beginning 2) (match-end 2)))
	      (input  (buffer-substring
		       (match-beginning 3) (match-end 3)))
	      (output  (buffer-substring
			(match-beginning 4) (match-end 4)))
	      )
	  (delete-region (match-beginning 0) (match-end 0))
	  (insert (funcall nb-adjust-tex-input
			   notebook-buffer name input prompt))
	  (insert (funcall nb-adjust-tex-output
			   notebook-buffer name output prompt))
	  )
	)
      (goto-char (point-min))		; make some special stuff.
      (if (re-search-forward "\\document.*\n" nil t)
	  (goto-char (match-end 0)))
      (end-of-line)
      (insert "\n\\newcommand{\\prompt}{\\mbox{$>$}}\n") 


      (goto-char (point-min))		; join nearby cells
      (while (re-search-forward 
	      "\\\\end{verbatim}[ \t]*\n\\\\begin{verbatim}"
	      nil t)
	(goto-char (match-beginning 0))
	(delete-region (match-beginning 0) (match-end 0) )
	(insert "\n"))
      ;; Now run TeX on it: (this is taken from tex-file...)
      (save-some-buffers)

      (if (functionp 'TeX-command-master) ;if AucTeX is installed
	  (TeX-command-master)
	  ;; Otherwise, don't use auctex.
	  ;; This is the regular tex-mode version: 
	(if (tex-shell-running)
	    (tex-kill-job)
	  (tex-start-shell))
	(tex-send-command tex-shell-cd-command file-dir)
	(tex-send-command tex-command tex-name)
	(setq tex-last-buffer-texed (current-buffer))
	(setq tex-print-file (buffer-file-name))
	)
      )	;;  End of stuff in tex-buffer.  Now we go back to notebook buffer.
    (if (functionp 'tex-display-shell) (tex-display-shell) )
    )
  )

(defun nb-clean-for-tex (string)
  ;; turn one dollar sign into two.
  (setq string (replace-regexp-in-string "\\$" "$-$-" string)) 
  ;; turn backslashes into special characters.
  (setq string (replace-regexp-in-string "\\\\" "$\\\\backslash$" string))
  ;; turn two dollar signs into backslash dollar sign.
  (setq string (replace-regexp-in-string "\\$-\\$-" "\\\\$" string)) 
  ;; Turn special characters into backslash special character:
  (replace-regexp-in-string "\\([%{}]\\)" "\\\\\\1" string)
  )

(defconst nb-adjust-tex-input
  (lambda (buffer name input prompt)
    (if (equal ">" prompt)
	(concat "{ \\tt > " (nb-clean-for-tex input))
      (concat "\n\\begin{verbatim}\n>> " input))
    )
  "A function which adjusts an input string so that it is valid tex.")
(make-variable-buffer-local 'nb-adjust-tex-input)

(defconst nb-adjust-tex-output 
  (lambda (buffer name output prompt)
    (if (equal ">" prompt)
	(if (equal output "")
	    "}"
	  (concat "\\, (" (nb-clean-for-tex input) ")}"))
      (concat output " \\end{verbatim}")
      )
    )
  "A function which adjusts an output string so that it is valid tex.")
(make-variable-buffer-local 'nb-adjust-tex-output)


(defun notebook-to-html ()
  "Convert this file to an html file."
  (interactive)
  (let ((notebook-name (buffer-file-name))
	(notebook-buffer (current-buffer))
	(file-dir (file-name-directory (buffer-file-name)))
	(html-name)
	(cell-regexp nb-cell-regexp)	; Keep track of the local variable.
	(html-buffer) )
    (setq html-name (concat (file-name-sans-extension notebook-name) ".html"))
    (setq html-buffer (find-file-noselect html-name))
    (save-excursion
      (set-buffer html-buffer)
      (delete-region (point-min) (point-max))
      (insert-buffer-substring notebook-buffer)
      (goto-char (point-min))		; Get rid of special characters
      (while (re-search-forward cell-regexp nil t)
	(let ((prompt  (buffer-substring
		      (match-beginning 1) (match-end 1)))
	      (name   (buffer-substring
		      (match-beginning 2) (match-end 2)))
	      (input  (buffer-substring
		      (match-beginning 3) (match-end 3)))
	      (output  (buffer-substring
		      (match-beginning 4) (match-end 4)))
	      )
	  (delete-region (match-beginning 0) (match-end 0))
	  (setq input (funcall nb-adjust-html-input input notebook-buffer name))
	  (setq output (funcall nb-adjust-html-output output notebook-buffer name))
	  (if (equal ">" prompt)
	      (if  (equal "" output)
	      (insert (concat  "<KBD>" input "</KBD>" ))
	      (insert (concat  "<KBD>" input "</KBD> (<SAMP>" output "</SAMP>)" )))
	    (insert (concat  "\n<pre>\n&gt;&gt; " input output "\n</pre>" ))
	      )
	  ))
      )
    (save-some-buffers)
    )
  )

(defconst nb-adjust-html-input
  (lambda (string buffer name)
    ;; (scratch (format "Adjusting %s from cell %s.\n" string name))
    (replace-regexp-in-string 
     ">" "&gt;"
     (replace-regexp-in-string
      "<" "&lt;"
     (replace-regexp-in-string
      "\"" "&quot;"
      (replace-regexp-in-string "&" "&amp;" string)))) )
  "A function which adjusts an input string so that it is valid html.")
(make-variable-buffer-local 'nb-adjust-html-input)

(defconst nb-adjust-html-output nb-adjust-html-input
  "A function which adjusts an output string so that it is valid html.")
(make-variable-buffer-local 'nb-adjust-html-output)

(defun notebook-to-plain-text ()
  "Convert this file to a plain text file."
  (interactive)
  (let ((notebook-name (buffer-file-name))
	(notebook-buffer (current-buffer))
	(file-dir (file-name-directory (buffer-file-name)))
	(text-name)
	(cell-regexp nb-cell-regexp)	; Keep track of the local variable.
	(text-buffer) )
    (setq text-name (concat (file-name-sans-extension notebook-name) ".txt"))
    (setq text-buffer (find-file-noselect text-name))
    (save-excursion
      (set-buffer text-buffer)
      (delete-region (point-min) (point-max))
      (insert-buffer-substring notebook-buffer)
      (goto-char (point-min))		; Get rid of special characters
      (while (re-search-forward "\b" nil t)
	(replace-match "" nil nil))
      )))

;;; Notebook Shell Mode.

;;
(defvar nb-shell-script-extension ".sh"
  "The default extension to use when creating a new shell script.
This is used by \\[notebook-to-script]. " )
(make-variable-buffer-local 'nb-shell-script-extension)

(defvar nb-script-comment "#"
  "This is put at the beginning of a line to comment out text regions
in the routine \\[notebook-to-script]. " )
(make-variable-buffer-local 'nb-script-comment)


;;
(defvar nb-shell-script-line "#! /bin/bash"
  "The first line to put in a new shell script.
This is used by \\[notebook-to-script]. " )
(make-variable-buffer-local 'nb-shell-script-line)

;;
(defun notebook-mode ()
  "Major mode for editing a bash shell notebook.  

\\<notebook-mode-map>
This mode is used to edit input to, and output from, a sub process. The
sub-process is a bash shell, but try `\\[apropos-command notebook-mode]' for
some notebook modes for other programs, such as matlab or octave. 

The notebook has a collection of cells, consisting of a prompt, an input region
and an output region.  You tell Emacs to send a cell to the process (using
\\[nb-send-input] or \\[nb-send-input-forward]) and when the process responds,
it will paste the output in to the output region.  You can then go back and
edit other cells and re-enter them.  Between the cells is regular text, which
you can do with as you wish.

------------------------------------

Here is a list of some useful commands:

\\[nb-send-input]
Sends the cell at (or just before) point to the sub-process.  If the process is
not running, it is started.

\\[nb-next-cell]
Move point the the beginning of the next cell's input.  If there is no
cell after point, a new cell is created.

\\[nb-create-cell]
Create a new cell at point.  If point is currently in a cell, a new
cell is created on the next line.

\\[nb-send-input-forward]
First enter the current cell, and then move to the next cell. 
The same as typing \\[nb-send-input] followed by \\[nb-next-cell]

\\[nb-send-input-and-create]
First enter the current cell, and then create a new cell immediately afterwards.
The same as typing \\[nb-send-input] followed by \\[nb-create-cell]

\\[nb-send-input-region]
Send all cells in the current region to the process.  If the process
is not running, it is started first.

\\[nb-send-input-buffer]
Send all cells in the buffer to the process.  If the process
is not running, it is started first.

\\[nb-delete-cell-and-text]
Delete the cell at point: both the input and output region.

\\[nb-kill-process]
Kill the process.

\\[nb-start-process]
Re-start the process. 

\\[notebook-to-script]
Creates a new shell script out of the notebook.

Entry to this mode calls the value of `notebook-mode-hook',
`shell-notebook-mode-hook', and any hooks called by the text mode."
  (interactive)
  ;; Note: I'll add comments starting with M: if this is a standard thing
  ;; a mode should do.
  (kill-all-local-variables)		; M: all modes start with this.
  (nb-choose-text-mode)	;; pick a text mode (to inherit font-locking, etc).
  (notebook-mode-initialize 'sh-mode nil) ; this also sets up font-lock mode.
  (setq major-mode 'notebook-mode)	; M: all modes do this.
  (setq mode-name "Shell Notebook")	; M: all modes do this.
  ;; M: other modes might set the indent-line-function, we inherit from text mode.
  (use-local-map			; M: notebook modes need the special keys.
   (nb-setup-keymap (copy-keymap (current-local-map))))
  (run-hooks 'notebook-mode-hook 'shell-notebook-mode-hook)
  )
  
;;
(defvar shell-notebook-mode-hook nil
  "If this is non-nil, it is the hook that's run for any shell notebook.")

;; 
(defun notebook-to-script (filename)
  "Convert this notebook file to a shellscript file.
It prompts for a file FILENAME to which the new shell script is written.
If the filename is the same as the notebook's file, then a new file is
created with the suffix '.sh'.

The shell script is made up of all the input cells in the notebook.
The output cells are ignored, and the text in between has comment
characters, '#', added to the beginning of each line.
"
  (interactive "FOutput shell script to: " )
  (let ((notebook-name (buffer-file-name))
	(notebook-buffer (current-buffer))
	(cell-regexp nb-cell-regexp)	; Keep track of the local variable.
	(script-buffer) (cell))
    ;; Set the filename, if it wasn't passed as an argument.
    (setq script-buffer (find-file filename))
    (setq filename (buffer-file-name))
    (if (equal filename notebook-name)
	(setq filename (concat (file-name-sans-extension filename)
			       nb-shell-script-extension))
      )
    (setq script-buffer (find-file filename))
    (setq filename (buffer-file-name))
    (delete-region (point-min) (point-max))
    (insert-buffer-substring notebook-buffer)
    (shell-script-mode)
    (goto-char (point-min))
    (insert nb-shell-script-line "\n")
    (previous-line 1)
					;make everything a comment.
    (replace-string "\n" (concat "\n" nb-script-comment " " ))
    (goto-char (point-min))
    (replace-string "-*- notebook -*-" "" )
    (goto-char (point-min))
    (while (re-search-forward cell-regexp nil t)
      (setq cell (buffer-substring-no-properties (match-beginning 3)
						 (match-end 3) ))
      (goto-char (match-beginning 0))
      (beginning-of-line)
      (delete-region  (point) (match-end 0))
      (insert       (replace-regexp-in-string "\n# " "\n" cell))
      )
    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; End of notebook-mode.el

;; PENDING: do something like this:  
;; (setq TeX-file-extensions
;;  '("tex" "sty" "cls" "ltx" "texi" "texinfo" "shell" "matlab" "octave"))
