;; $Id$
;; Set up a notebook mode for octave.

(provide 'octave-notebook-mode)
(require 'notebook-mode)
(require 'octave-mod)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun octave-notebook-mode ()
  "Major mode for a Octave notebook.  It is a combination of TeX mode
and Notebook mode.

See documentation of `notebook-mode` for a description of a notebook,
and cells.

Special commands:
\\[octave-notebook-mode-map]

In addition to `octave-notebook-hook', and whatever hooks the text mode runs, 
`notebook-mode-hook' is also run.

"
  (interactive)
  (kill-all-local-variables)
  (nb-choose-text-mode)	;; pick a text mode (to inherit font-locking, etc).
  (nb-octave-regexpressions)
  (setq nb-adjust-input-string octave-notebook-adjust-input-string)
  (setq nb-adjust-output-string octave-notebook-adjust-output-string)
  (setq nb-start-process octave-start-process)
  (notebook-mode-initialize 'octave-mode nil) ; this also sets up font-lock mode.
  (setq major-mode 'octave-notebook-mode)
  (setq mode-name "Octave Notebook")
  (use-local-map			; M: notebook modes need the special keys.
   (nb-setup-keymap (copy-keymap (current-local-map))))

  (setq indent-tabs-mode nil)		; PENDING: needed?
  (setq nb-process (nb-find-process "octave"))
  (if nb-process
    (setq mode-line-process (format ": %s" (process-name nb-process))))
  (run-hooks 'notebook-mode-hook 'octave-notebook-mode-hook)
  )

;;
(defvar octave-notebook-mode-hook nil
  "If this is non-nil, it is the hook that's run for any shell notebook.")

;;
(defun nb-octave-regexpressions ()
  "Set regular expressions for octave like mode."
  (let ( (name "\\([^ \t\n\b\f)(]*\\)")	; Possible name of function or cell.
	 (ws "\\s *")			; Whitespace.
	 (body "\\([^\b\f]*\\)")	; Body of input or output region.
	 )
    (setq nb-cell-regexp
	  (concat "\b\\(>>?\\)" 	; Prompt
		  "\\(\\)"		; No name in prompt.
		  body  "\b" body "\b" ; input and output.
		  ))
    (setq nb-empty-cell-format
	  (concat "\b>>  \b\n(no output yet)\b\n"))
    (setq nb-output-regexp
	  (concat "Begin \f"  body "\f" ws name ws; buffer name, and cell name
		  "\n\\([^\f]*\\)" ws ;Body of output
		  "\fEnd" ws "\\2" ))
    (defconst octave-function-regexp	; A function definition is...
      (concat ws "function"		; the keyword function, 
	      "[^=]*=" ws		; some variables and an equal sign
	      name)			; and then the name.
      "This matches the definition of a function.")
    ))


(defconst octave-notebook-adjust-input-string
  (lambda (string buffer name)
    (save-excursion
      (let ((cell (nb-find-cell-by-name name))
	    (file-name))
	(goto-char (overlay-start (nb-cell-input-overlay cell)))
	(if (not (looking-at octave-function-regexp)) 
	    ;; If it's not a function, it's regular input, so send it along.
;;	    (concat "fprintf('Begin \\f" buffer "\\f " name " ')\n"	
	    (concat "'Begin \\f" buffer "\\f " name " '\n"	
		    string
		    "\nfprintf('\\fEnd " name " .')\n")
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
    (let ((input (buffer-substring i-beg i-end))
	  (beg (make-marker))
	  (end (make-marker))
	  )
      (move-marker beg o-beg)
      (move-marker end o-end)
      (save-excursion                     ;remember point.
	(goto-char end)                   ; strip ending off.
	;; (while (re-search-backward ".*fprintf.*\\f.*\n" beg t) 
	;; 	  (message (format "removing printf (%d - %d"
	;; 			   (match-beginning 0) (match-end 0)))
	;; 	  (delete-region (match-beginning 0) (match-end 0))
	;; 	  )

	;; Remove all the prompts:
	(goto-char beg)           
	(while (re-search-forward  "\\s *octave:[0-9]+>\\s *" end t) 
	  (delete-region (match-beginning 0) (match-end 0)) )
	;; Get rid of the trailing junk.
	(goto-char beg)           
	(if (re-search-forward  "\\([ \t\n]+\\)\b" (+ end 1) t) 
	    (progn
	      (delete-region (match-beginning 1) (match-end 1))
	      )
	  )
	(goto-char beg)		; Delete blank lines.
	(while (re-search-forward "\\(\n\\s *\\)\n" end t)
	  (delete-region (match-beginning 1) (match-end 1)))
	(goto-char beg)
	(if (re-search-forward "error:" end t) ;Warning, there was an error.
	    (progn
	      (nb-set-cell-status cell 'error)
 	      ))
	(goto-char beg)
	(insert " ")
	(goto-char end)
	(insert "  ")
	(goto-char beg)			; If there's nothing left but
	(if (looking-at "\\([ \t\n]+\\)\b")	; whitespace, remove it.
	    (delete-region (match-beginning 1) (match-end 1))
	  ;; Otherwise, if it is really just two lines.  x = \n...
	  ;; Then we should just make it one line.
	  (if (looking-at "[^\n]*=\\(\n\\)[^\n]*\b")
	      (delete-region (match-beginning 1) (match-end 1))
	    ;; If it's more than two lines, the we should put the "x =" on
	    ;; it's own line, too.
	    (if (looking-at "\\s *\\(\\sw* =\n\\)")
		(insert "\n"))
	    )
	  )
	(set-marker beg nil)
	(set-marker end nil)
	))
    )
  "Adjust the output text from octave."
  )

(defconst octave-start-process
  (lambda (&optional old)
    (interactive)
    (if (and old (nb-find-process "octave"))
	(setq nb-process (nb-find-process "octave"))
      (save-excursion
	(let ((buff (generate-new-buffer "*octave*"))
	      (notebook-buffer (current-buffer))
	      (regexp nb-output-regexp))
	  (setq nb-process
		(start-process "octave"	; name of process
			       buff
			       "octave"))	; Program name.
	  (process-kill-without-query nb-process)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

