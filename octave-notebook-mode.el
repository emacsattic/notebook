;; $Id$
;; Set up a notebook mode for octave.


(provide 'octave-notebook-mode)
(require 'notebook-mode)
(require 'tex-mode)


(defvar octave-notebook-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tex-mode-map)
    (nb-setup-keymap map)
    (define-key map "\C-c\C-f" 'tex-octave-file)
    (define-key map "\C-c>" 'octave-toggle-prompt)
    (define-key map [menu-bar tex tex-file]
      '("Run TeX on Notebook" . tex-octave-file))
    map)
  "The key map for octave notebooks.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-derived-mode matlab-notebook-mode tex-mode "Matlab"
  "Major mode for a Matlab notebook.  It is a combination of TeX mode
and Notebook mode.

See documentation of `notebook-mode` for a description of a notebook,
and cells.

Special commands:
\\{matlab-notebook-mode-map}

See documentation for tex-mode for other commands.

In addition to `matlab-notebook-hook', and whatever hooks tex-mode runs, 
`common-notebook-mode-hook' is also run.

"

  (scratch "Running matlab notebook mode.\n")
  (nb-octave-regexpressions)
  (setq nb-adjust-input-string octave-notebook-adjust-input-string)
  (setq nb-adjust-output-string octave-notebook-adjust-output-string)
  (setq nb-start-process octave-start-process)
  (notebook-mode-initialize)
  (setq indent-tabs-mode nil)
  (setq nb-process (nb-find-octave-process))
  (if nb-process
    (setq mode-line-process (format ": %s"
				    (process-name nb-process)))
    )
  ) 


(defun nb-octave-regexpressions ()
  "Set regular expressions for octave mode."
  (let ( (name "\\([^ \t\n\b\f)(]*\\)")	; Possible name of function or cell.
	 (ws "\\s *")			; Whitespace.
	 (body "\\([^\b]*\\)")		; Body of input or output region.
	 )
    (setq nb-cell-regexp
	  (concat "\b\\(>>? \\)" 	; Prompt
		  "\\(\\)"		; No name in prompt.
		  body  "\b" body "\b" ; input and output.
		  ))
    (setq nb-prompt-format ">> ")
    (setq nb-empty-cell-format
	  (concat "\b>>   \b\n(no output yet)\b\n"))
    (setq nb-output-regexp
	  (concat "Begin" ws name ws name
		  "\n\\([^\f]*\\)" ws	;Body of output
		  "\fEnd" ws "\\2" ))
    (setq octave-function-regexp	; A function definition is...
	  (concat ws "function"		; the keyword function, 
		  "[^=]*=" ws		; some variables and an equal sign
		  name))		; and then the name.
    ))



(defconst octave-function-regexp
  "This matches the definition of a function.")

(defconst octave-notebook-adjust-input-string
  (lambda (string buffer name)
    (save-excursion
      (let ((cell (nb-find-cell-by-name name))
	    (file-name))
	(scratch (format "Adjusting input for cell named '%s'\n" name))
	(scratch (format "which is %s\n" cell))
	(goto-char (overlay-start (nb-cell-input-overlay cell)))
	(if (not (looking-at octave-function-regexp)) 
	    ;; If it's not a function, it's regular input, so send it along.
	    (concat "'Begin " buffer " " name "'\n"	
		    string
		    "\nfprintf('\\fEnd " name "')\n")
	  ;; If it is a function, save the file, and don't send anything
	  ;; to octave.
	  (setq file-name (format "%s.m"
				  (buffer-substring (match-beginning 1)
						    (match-end 1))))
	  (write-region (overlay-start (nb-cell-input-overlay cell))
			(overlay-end (nb-cell-input-overlay cell))
			file-name)
	  (nb-insert-output (current-buffer)
			    name (format "(function %s saved)" file-name))
	  " "				; Return a trivial string.
	  ))))
  "Adjust STRING so that it can be sent to octave."
  )

(defconst octave-notebook-adjust-output-string
  (lambda (i-beg i-end o-beg o-end cell)
    (scratch (format
	      "Adjusting output. input=<%d,%d> output=<%d,%d> name = %s\n"
	      i-beg i-end o-beg o-end (nb-cell-name cell)))
    (let ((input (buffer-substring i-beg i-end))
	  (beg (make-marker))
	  (end (make-marker))
	  )
      (move-marker beg o-beg)
      (move-marker end o-end)
      (save-excursion                     ;remember point.
	(goto-char end)                   ; strip ending off.
	(while (re-search-backward ".*fprintf.*\\f.*\n" beg t) 
	  (scratch (format "removing printf (%d - %d\n"
			   (match-beginning 0) (match-end 0)))
	  (delete-region (match-beginning 0) (match-end 0))
	  )
	(scratch "removing prompts ")
	(goto-char beg)                   ; strip all prompts.
	(while (re-search-forward  "\\s *octave:[0-9]+>\\s *" end t) 
	  (scratch "- ")
	  (delete-region (match-beginning 0) (match-end 0)) )
	(scratch "\nstripping echos.\n")
	(nb-delete-lines beg input end t)	;strip input echo.
	(goto-char beg)			; Delete blank lines.
	(while (re-search-forward "\\(\n\\s *\\)\n" end t)
	  (goto-char (match-beginning 0))
	  (delete-region (match-beginning 1) (match-end 1)))
	(goto-char beg)
	(if (re-search-forward "\a" end t) ;Warning, there was an error.
	    (progn
	      (goto-char (match-beginning 0))
	      (delete-char 1)
	      (nb-set-cell-status cell 'error)
	      (nb-set-colors cell)
	      ))
	(goto-char beg)			; If there's nothing left but
	(if (looking-at "\n\b")		; a newline, remove it.
	    (delete-char 1))
	))
    )
  "Adjust the output text from octave."
  )

(defun nb-find-octave-process ()
  "Find a octave process that is already running. If there is none,
return nil."
  (let ((list (process-list))		;Get a list of processes.
	(proc) (octave nil) (command))
    (while (and list (not octave))		;Look at each one.
      (setq proc (car list))
      (setq command (process-name proc))
      (if (string-match "octave" command)
	  (setq octave proc))
      (setq list (cdr list))
      )
    octave				; Return the result of the search.
    )
  )

(defconst octave-start-process
  (lambda (&optional old)
    (interactive)
    (if (and old (nb-find-octave-process))
	(setq nb-process (nb-find-octave-process))
      (save-excursion
	(let ((buff (generate-new-buffer "*octave*"))
	      (notebook-buffer (current-buffer))
	      (regexp nb-output-regexp))
	  (setq nb-process
		(start-process "octave"	; name of process
			       buff
			       "octave"))	; Program name.
	  (set-process-filter nb-process 'nb-filter)
	  (process-send-string nb-process "echo off\n")
	  (set-buffer buff)
	  (setq nb-output-regexp regexp)
	  ))
      )
    (if nb-process
	(setq mode-line-process
	      (format ": %s" (process-name nb-process)))
      )
    )
  "Start a octave process.  If the optional argument OLD is nil, a new
process will be started, even if an old one already exists.  "
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Convert to TeX and run TeX on the file:

(defun octave-prompt-short ()
  "Set the default to be short prompts."
  (interactive)
  (setq nb-empty-cell-format
	(concat "\b>   \b\n(no output yet)\b\n"))
  )
(defun octave-prompt-long ()
  "Set the default to be short prompts."
  (interactive)
  (setq nb-empty-cell-format
	(concat "\b>>   \b\n(no output yet)\b\n"))
  )

(defun octave-toggle-prompt (pos)
  "Give the cell at POS a short prompt."
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
	(if (looking-at ">> ") 
	    (delete-char 1)
	  (insert ">")
	  )
	(overlay-put prompt 'modification-hooks 
		     (nb-make-hook 'nb-modify-prompt cell))
	)))
  )

;; This starts tex-mode, and then modifies it a bit.
(defun nb-start-tex-mode ()		
  (tex-mode)				; run tex.
  )

(defun octave-to-tex-name (name)
  (string-match "\\(.*\\)\.m" name)	; anything before the .m
  (concat (substring name (match-beginning 1) (match-end 1))
	  ".tex")
  )

(defun tex-octave-file ()
  "Convert this file to a TeX file, and run the command tex-file on it."
  (interactive)
  (let ((octave-name (buffer-file-name))
	(octave-buffer (current-buffer))
	(file-dir (file-name-directory (buffer-file-name)))
	(tex-name)
	(cell-regexp nb-cell-regexp)	; Keep track of the local variable.
	(tex-buffer) )
    (setq tex-name (octave-to-tex-name octave-name))
    (setq tex-buffer (find-file-noselect tex-name))
    (save-excursion
      (set-buffer tex-buffer)
      (delete-region (point-min) (point-max))
      (insert-buffer-substring octave-buffer)
      (goto-char (point-min))		; Get rid of special characters
      (while (re-search-forward cell-regexp nil t)
	;; Check to see if this is short input:
	(if (equal "> " (buffer-substring (match-beginning 1)
					(match-end 1)))
	    (let ((body (buffer-substring ;short form.
			(match-beginning 4) (match-end 4))))
	      (goto-char (- (match-end 0) 1))
	      (delete-char 1)
	      (if (equal "" body)
		  (insert "}")		; no output
		(insert ")} " ))	; there was output preface it.
	      (goto-char (match-end 3))
	      (delete-char 1)
	      (if (equal "" body) ()	;no output
		(insert " \\, (" ))	;there was output preface it.
	      (goto-char (match-beginning 0))
	      (delete-char 3)
	      (insert "{ \\tt ")
	      )
					; long form.
	  (goto-char (- (match-end 0) 1))
	  (delete-char 1)
	  (insert "\\end{verbatim}")
	  (goto-char (match-end 3))
	  (delete-char 1)
	  (goto-char (match-beginning 0))
	  (delete-char 1)
	  (insert "\\begin{verbatim}")
	  ) )
      (goto-char (point-min))		; join nearby cells
      (while (re-search-forward 
	      "\\\\end{verbatim}[ \t]*\n\\\\begin{verbatim}"
	      nil t)
	(goto-char (match-beginning 0))
	(delete-region (match-beginning 0) (match-end 0) )
	(insert "\n"))
      ;; Now run TeX on it: (this is taken from tex-file...)
      (save-some-buffers)
      (if (tex-shell-running)
	  (tex-kill-job)
	(tex-start-shell))
      (tex-send-command tex-shell-cd-command file-dir)
      (tex-send-command tex-command tex-name)
      (setq tex-last-buffer-texed (current-buffer))
      (setq tex-print-file (buffer-file-name))
      )
    (tex-display-shell)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

