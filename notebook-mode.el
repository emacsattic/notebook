;; $Id$
;; Set up a notebook mode.  

(provide 'notebook-mode)
(require 'notebook-mode)		;needed for byte compiler.

;;;
;; A notebook is a mostly text file with a few input/output cells.
;; It has a sub-process which the input cells are sent to and which
;; returns output cells.  
;; 
;; The default notebook uses the shell bash.
;; See also matlab.el and mupad.el which use matlab and mupad as their
;; subprocesses.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some standard variables.

(defvar notebook-mode-hook nil
  "If this is non-nil, it is the hook that's run for any notebook")
(make-variable-buffer-local 'notebook-mode-hook)

(defconst nb-adjust-input-string
  (lambda (string buffer name)
    ;; (scratch (format "Adjusting %s from cell %s.\n" string name))
    (replace-regexp-in-string
     "^\\s-*;" "" 			; remove blank lines with semicolons.
     (replace-regexp-in-string
      "\\s-*;\\(\\s-*;\\)*" ";" 	;replace several semicolons with only one.
      (concat "echo -e \"\\fBegin " buffer " " name "\";" string
	      "; echo -e \"\\fEnd " name "\";\n")))
    )
  "A function which adjusts an input string so that it can be sent to
the process.  It is called with STRING, BUFFER, and NAME.
STRING is the string which should be sent.  BUFFER is the name of the
notebook buffer. NAME is the name of the input cell.  Output from
the process will have to be recognized by NAME and BUFFER.")
(make-variable-buffer-local 'nb-adjust-input-string)

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

(defconst nb-display-table (make-display-table)
  "The display table for buffer.")
(aset nb-display-table 12 (vector 124))	; A bar for the special character
(aset nb-display-table 8 (vector 124))	; A bar for the special character

(defvar nb-cell-list nil
  "A list of cells for this buffer.")
(make-variable-buffer-local 'nb-cell-list)

;; Here are some variables that make up regular expressions.
(let ( (name "\\([^ \t\n\b\f]*\\)")	; Possible name regular expression.
       (ws "\\s *")			; Whitespace.
       (body "\\([^\b\f]*\\)")		; Body of input or output region.
       )
  (defconst nb-cell-regexp 
    (concat "\b\\(" name ">\\)"		; Prompt and name.
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

  (defconst nb-empty-cell-format 
    (concat "\b%s>  \b  (no output yet)\b\n")
    "This is inserted as an initial empty cell.  It can use the name of the
cell if it wishes (e.g. \"\bin(%s) =  \n< >\b\").  "
    )
  (make-variable-buffer-local 'nb-empty-cell-format)
  
  (defconst nb-output-regexp 
    (concat
     "\fBegin" ws			; A keywork
     name ws name ws "\n"		; The buffer name, and cell name.
     body				; The useful part of the output.
     ws "\fEnd" ws  
     "\\2" )			; The name should be at the end.
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

(defvar nb-shell-script-extension ".sh"
  "The default extension to use when creating a new shell script.
This is used by {notebook-to-script}. " )
  
(defvar nb-shell-script-line "#! /bin/bash"
  "The first line to put in a new shell script.
This is used by {notebook-to-script}. " )
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (define-key keymap "\C-y"     'nb-yank)
  (define-key keymap "\ey"     'nb-yank-pop)
  (define-key keymap "\C-c\C-f"     'notebook-to-script)
  (define-key keymap
    [menu-bar notebook] (cons "Notebook" (make-sparse-keymap "Notebook")))
  (define-key keymap [menu-bar notebook nb-send-input]
    '("Send Cell to Process" . nb-send-input))
  (define-key keymap [menu-bar notebook nb-send-input-region]
    '("Send Region to Process" . nb-send-input-region))
  (define-key keymap [menu-bar notebook nb-send-input-buffer]
    '("Send Buffer to Process" . nb-send-input-buffer))
  (define-key keymap [menu-bar notebook nb-start-process ]
    '("Start a New Process" . nb-start-process))
  (define-key keymap [menu-bar notebook nb-kill-process]
    '("Kill Process" . nb-kill-process))
  (define-key keymap [menu-bar notebook nb-create-cell]
    '("New Cell" . nb-create-cell))
  (define-key keymap [menu-bar notebook nb-delete-cell-and-text]
    '("Delete Cell" . nb-delete-cell-and-text))
  (define-key keymap [menu-bar notebook notebook-to-script]
    '("Create Shell Script" . notebook-to-script))
  keymap
  )

(defvar notebook-mode-map (nb-setup-keymap (make-sparse-keymap))
  "Keymap for notebook mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode notebook-mode fundamental-mode "Notebook"
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



Entry to this mode calls the value of `notebook-mode-hook'
if that value is non-nil."

  ;; (scratch "Running notebook mode.\n") 
  (notebook-mode-initialize))


(defun notebook-mode-initialize ()
  "This initializes notebook-mode and its variants."
  ;; (scratch "Initializing notebook mode.\n") 
  (put 'funney-mode 'mode-class 'special) ; This mode uses special text.
  (setq buffer-display-table nb-display-table) ; Use brackets for characters.
  (setq nb-cell-list nil)
  (nb-initialize-cells (point-min) (point-max))
  (if nb-process
      (setq mode-line-process (format ": %s" (process-status nb-process)))
    (setq mode-line-process ": no proc.")
    )
  ;;(scratch (format "nb-cell-regexp = %s, aref = %s\n" nb-cell-regexp
  ;;		   (aref nb-cell-regexp 0)))
  
  (setq paragraph-start			;A paragraph starts with:
	(concat paragraph-start		; what ever it did before.
		"\\|"  (char-to-string
			(aref nb-cell-regexp 0)) ; A cell can start a paragraph.
		))
  (setq paragraph-separate		; But it can't separate one.
	(concat paragraph-separate
		))
  (run-hooks 'common-notebook-mode-hook)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic cell manipulation:

;;; A cell is a list of
;; a name (or number)
;; a status, one of: 'entered, 'unentered, 'processed, 'error
;; a begining marker.
;; an overlay for the prompt. 
;; an overlay for the input. 
;; an overlay for the output.
(defun nb-cell-name (cell) (if cell (car cell) nil))
(defun nb-cell-status (cell) (nth 1 cell))
(defun nb-cell-begin (cell) (nth 2 cell))
(defun nb-cell-prompt-overlay (cell) (nth 3 cell))
(defun nb-cell-input-overlay (cell) (nth 4 cell))
(defun nb-cell-output-overlay (cell) (nth 5 cell))

(defun nb-set-cell-status (cell status)
  (setcar (nthcdr 1 cell) status))

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
	       ()			; Not looking at a cell.  return quietly.
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


(defun nb-cell-part-by-position (pos)
  "Returns the part of a cell that POS is in.  It can either be
the tokens 'prompt, 'input, 'output, or nil, if the position is either
in the prompt, the input region, the output region of a cell, or not in
a cell at all.
"
  (interactive "d")
  (let ((overlays (overlays-at pos)) (type))
    (while (and overlays (not type))
      (setq type (overlay-get (car overlays) 'cell-part))
      (setq overlays (cdr overlays)))
    type
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Cell creation and intialization.

(defun nb-initialize-cells (beg end)
  "Create cell data for all of the i/o cells that are between BEG and END."
  (if (< end beg)
      (let ((temp beg))			;Swap beg and end.
	(setq beg end)
	(setq end beg))
    )
  ;;(scratch (format "Initializing between %d and %d.\n" beg end))
  ;;(scratch (format "Looking for '%s'.\n" nb-cell-regexp))
  (save-excursion
    (goto-char beg)
    (while (re-search-forward nb-cell-regexp end t)
      (nb-initialize-one-cell (match-beginning 0)))
    ))
      

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
		(goto-char (match-beginning 2))	; Delete the old name.
		(delete-region (match-beginning 2) (match-end 2))
		(setq name (funcall nb-name-new-cell))
		(insert name)))
	  ) ;; Done checking name.
	(set-marker beg pos)		; Set the beginning point.
	
					; Make the cell, and put it on the
					; list.
	(setq cell (list name 'unentered beg prompt input output))
	(setq nb-cell-list (cons cell nb-cell-list))
	
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
	(nb-set-colors cell)		; adjust the colors of the cell.
	(overlay-put prompt 'cell-part 'prompt) 
	(overlay-put  input 'cell-part  'input) 
	(overlay-put output 'cell-part 'output) 
	(overlay-put prompt 'cell cell) 
	(overlay-put  input 'cell cell) 
	(overlay-put output 'cell cell) 
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
    (goto-char (match-beginning 3))	; Leave point at the start of the input data.
    (save-excursion
      (indent-according-to-mode))       ; Finally, leave this cell looking nice.
    cell))				; return the newly created cell. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Colors:
(defun nb-set-colors (cell)
  "Set the colors of the cell appropriately."
  (let ( (status (nb-cell-status cell)))
    ;;(scratch (format "Set color for %s (status = %s)\n"
    ;;	     (nb-cell-name cell) status))
    (overlay-put (nb-cell-input-overlay cell)
		 'face 'notebook-input-face)
    (overlay-put (nb-cell-output-overlay cell)
		 'face 'notebook-output-face)
    (overlay-put (nb-cell-prompt-overlay cell)
		 'face
		 (cond ((equal status 'entered)  'notebook-entered-face)
		       ((equal status 'unentered)  'notebook-unentered-face)
		       ((equal status 'processed)  'notebook-processed-face)
		       ((equal status 'error)  'notebook-error-face)
		       ))
    ))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Movement:

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
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(defun nb-send-input-and-create (position)
  "Send the current input cell and create a new cell on the next line."
  (interactive "d")
  (goto-char position)
  (nb-send-input (point))
  (nb-create-cell (point))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun nb-send-input-buffer ()
  "Send all of the cells in the buffer to the process."
  (interactive)
  (nb-send-input-region (point-min) (point-max))
  )

(defun nb-send-input-region (beg end)
  "Send all of the cells in the region to the process."
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
	    (nb-set-cell-status cell 'entered)
	    (nb-set-colors cell)
	    (setq string
		  (buffer-substring (match-beginning 3) (match-end 3)))
	    ;;(scratch (format "Sending %s from cell '%s'.\n"
	    ;;	     string (nb-cell-name cell)))
	    (if (boundp 'notebook-debug-input-buffer)
		(save-excursion
		    (set-buffer notebook-debug-input-buffer)
		  (goto-char (point-max))
		  (insert (concat "<<" string ">>  -> <<"
				  (if nb-adjust-input-string
				      (funcall nb-adjust-input-string string
					       (buffer-name) (nb-cell-name cell))
				    (string))
				  ">>"))
		  ))			; end debugging statements.

	    (process-send-string
	     nb-process
	     (if nb-adjust-input-string
		 (funcall nb-adjust-input-string string
			  (buffer-name) (nb-cell-name cell))
	       (string))
	     )
	    ))))
  )



(defun nb-insert-output (buffer name string)
  "Put STRING in the cell named NAME, in the buffer BUFFER."
  (save-excursion			; Return to proces buffer afterwards.
    (set-buffer buffer)
    (save-excursion			; Save current point and mark.
      (let ((cell (nb-find-cell-by-name name))
	    )
	(if (equal cell nil)
	    (if (equal name "error") (message string))	;Put error as a message. 
	  ;; Otherwise, insert output.
	  (nb-set-cell-status cell 'processed)
	  (nb-set-colors cell)
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
	  )))))

(defconst nb-process nil "The notebook process.")
(make-variable-buffer-local 'nb-process)

(defun nb-start-process (&optional old)
  "This function is called in order to start a new process for the
current buffer. If it is passed an argument of true, it should try to find a
currently running process. (This doesn't work for the shell notebook.)"
  (interactive)
  (funcall nb-start-process old)
  )

(setq process-environment (cons "PAGER=cat" process-environment))

(defconst nb-start-process
  (lambda (&optional old)
    (if (not nb-process)
	(setq nb-process
	      (start-process
	       (buffer-name)	; name of process
	       (format "*%s-process*" (buffer-name)) ; process buffer.
	       "bash")))		; Program name.
    (process-kill-without-query nb-process)
    (set-process-filter nb-process 'nb-filter)
;    (process-send-string nb-process
;			 "alias ls='ls --no-color'\n")
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
       

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Changes in a cell.  These routines are called whenever a change
;;; is made inside of an overlay.


(defun nb-modify-prompt (overlay after beg end cell &optional len)
  "This is called when an prompt overlay is changed."
  (if after
      (nb-delete-cell-data cell)	; Only modification that can be done.
    (if (or  (> beg (nb-cell-begin cell))
	     (< end (overlay-end (nb-cell-output-overlay cell)))
	     )
	(error "Notebook prompt is read only."))))

(defun nb-insert-in-front-prompt (overlay after beg end cell &optional len)
  "Insert in front of prompt hook."
  (if after
      (progn
	(move-overlay overlay end	;Move the start of the prompt.
		      (overlay-end overlay)) ; Keep the end the same.
	(set-marker (nb-cell-begin cell) end) ; Move the start of the cell.
	)
    ))

(defun nb-modify-input (overlay after beg end cell &optional len)
  "This is called when an input overlay is changed."

  (if (and after
	   (not (equal 'unentered (nb-cell-status cell))))
      (progn
	(nb-set-cell-status cell 'unentered)
	(nb-set-colors cell))
    ))

(defun nb-insert-behind-input (overlay after beg end cell &optional len)
  "Insert behind an input overlay hook."
  (if (and after
	   (not (equal 'unentered (nb-cell-status cell))))
      (progn
	(nb-set-cell-status cell 'unentered)
	(nb-set-colors cell)))
  (if after
      (move-overlay overlay	       
		    (overlay-start overlay) ; Don't move the beginning.
		    end)		; Move the end.
      )
  )

(defun nb-modify-output (overlay after beg end cell &optional len)
  "This is called when an output overlay is changed."
  (if (not after)
    ;; The modification must be over the whole cell (a deletion, handled
    ;; by modify-prompt) or it should only cover the actual output and
    ;; not any stray control characters.
    (save-excursion
      (goto-char (nb-cell-begin cell))
      (looking-at nb-cell-regexp)
      (if (not (or			; not either
		(and (<= beg (point))	; all of cell...
		     (>= end (match-end 0)))
		(and (>= beg (match-beginning 4)) ; or only output part.
		     (<= end (match-end 4)))
		))
	  (error "Can not modify boundary between input and output.")
	))
    )
  )

(defun nb-insert-in-front-output (overlay after beg end cell &optional len)
  "Insert in front of output hook."
  (if after
      (move-overlay overlay end	;Move the start of the output.
		    (overlay-end overlay)) ; Keep the end the same.
    ))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Deleting and yanking a cell.

(defun nb-delete-cell-and-text (pos)
  "Delete the cell text and data at position POS"
  (interactive "d")
  (let ((cell (nb-find-cell-by-position pos t)))
    (if cell
	(progn 
	 (delete-region (nb-cell-begin cell)	; Delete the text.
			(overlay-end (nb-cell-output-overlay cell)))
	 (nb-delete-cell-data cell)
	 )
      (error (format "Position %d is not in an I/O cell." pos)))
    ))


(defun nb-delete-cell-data (cell)
  "Delete the data for this cell."
  (if (not cell)
      ()
    (delete-overlay (nb-cell-prompt-overlay cell))
    (delete-overlay (nb-cell-input-overlay cell))
    (delete-overlay (nb-cell-output-overlay cell))
    (setq nb-cell-list
	  (delete cell nb-cell-list))
    ))

			 
(defun nb-yank (&optional arg)
  "Like yank, but it also sets up data structures for any cells."
  (interactive "p")
  (yank arg)
  (nb-initialize-cells (mark) (point)))

(defun nb-yank-pop (&optional arg)
  "Like yank-pop, but it also sets up data structures for any cells."
  (interactive "p")
  (if (not arg) (setq arg 1))
  (delete-region (point) (mark))
  (nb-yank (+ 1 arg))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst nb-name-new-cell (lambda () (nb-default-name-new-cell))
  "This is called to pick a name for a newly generated cell.")
(make-variable-buffer-local 'nb-name-new-cell)

(defconst nb-next-cell-number 1
  "The current number of the input cell.")
(make-variable-buffer-local 'nb-next-cell-number)

(defun nb-default-name-new-cell ()
  (while (nb-find-cell-by-name (format "%d" nb-next-cell-number))
    (setq nb-next-cell-number (+ 1 nb-next-cell-number)))
  (format "%d" nb-next-cell-number))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
    (replace-string "\n" "\n# " )
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

