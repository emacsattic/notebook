;; $Id$
;; Set up a notebook mode for mupad.


(provide 'mupad-notebook-mode)
(require 'mupad-notebook-mode)
(require 'notebook-mode)
(require 'tex-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; First, define a keymap to be used for the mode.

(defvar mupad-notebook-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tex-mode-map)
    (nb-setup-keymap map)
    (define-key map "\C-c\C-f" 'tex-mupad-file)
    (define-key map [menu-bar tex tex-file]
      '("Run TeX on Notebook" . tex-mupad-file))
    map)
  "The key map for mupad notebooks.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-derived-mode mupad-notebook-mode tex-mode "Mupad"
  "Major mode for a Mupad notebook.  It is a combination of TeX mode
and Notebook mode.

See documentation of `notebook-mode` for a description of a notebook,
and cells.

Special commands:
\\{mupad-notebook-mode-map}

See documentation for tex-mode for other commands.

In addition to `mupad-notebook-hook', and whatever hooks tex-mode runs, 
`common-notebook-mode-hook' is also run.

"

  (scratch "Running mupad notebook mode.\n")
  (nb-mupad-regexpressions)
  (setq nb-adjust-input-string mupad-notebook-adjust-input-string)
  (setq nb-adjust-output-string mupad-notebook-adjust-output-string)
  (setq nb-start-process mupad-start-process)
  (notebook-mode-initialize)
  (setq indent-tabs-mode nil)
  (setq nb-process (nb-find-mupad-process))
  (if nb-process
    (setq mode-line-process (format ": %s"
				    (process-name nb-process)))
    )
  ) 

(defun nb-mupad-regexpressions ()
  "Set regular expressions for mupad mode."
  (let ( (name "\\([^ \t\n\b\f)(]*\\)") ; Possible name of function or cell.
	 (ws "\\s *")			; Whitespace.
	 (body "\\([^\b]*\\)")		; Body of input or output region.
	 )
    (setq nb-cell-regexp
	  (concat "\b\\(>> \\)" 	; Prompt
		  "\\(\\)"		; No name in prompt.
		  body  "\b" body "\b" ; input and output.
		  ))
    (setq nb-prompt-format ">> ")
    (setq nb-empty-cell-format
	  (concat "\b>>   \b\n(no output yet)\b\n"))
    (setq nb-output-regexp
	  (concat "\"Begin" ws name ws name "\""
		  "\\([^\f]*\\)" ws		;Body of output
		  "\"End \\2\"" ))
    )
  )


(defconst mupad-notebook-adjust-input-string
  (lambda (string buffer name)
    (save-excursion
      (let ((cell (nb-find-cell-by-name name))
	    (file-name))
	(scratch (format "Adjusting input for cell named '%s'\n" name))
	(goto-char (overlay-start (nb-cell-input-overlay cell)))
	(concat "\"Beg\"  .  \"in " buffer " " name "\";\n"	
		string ";\n"
		"\"End \"  .   \"" name "\";\n")
	)))
  "Adjust STRING so that it can be sent to mupad."
  )

(defconst mupad-notebook-adjust-output-string
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
	(scratch (concat "Working on ---->"
			  (buffer-substring o-beg o-end)
			  "<---\n"))
	(goto-char end)                   ; strip ending off.
	(forward-char)
	(while (re-search-backward "\\(\n.*\"End \"[^\b]*\\)\b" beg t) 
	  (scratch (format "removing End (%d - %d) '%s'\n"
			   (match-beginning 1) (match-end 1)
			   (buffer-substring (match-beginning 1)
					     (match-end 1))))
	  (delete-region (match-beginning 1) (match-end 1))
	  (scratch (concat "What's left ---->"
			   (buffer-substring beg end)
			   "<---\n"))
	  )
	(scratch "removing prompts ")
	(goto-char beg)                   ; strip all prompts.
	(while (re-search-forward  "\\s *>> \\$.*\n" end t) 
	  (scratch "- ")
	  (delete-region (match-beginning 0) (match-end 0)) )
	(goto-char beg)                   ; strip all prompts.
	(while (re-search-forward  "\\s *\\(>>\\|&>\\)\\s *" end t) 
	  (scratch "- ")
	  (delete-region (match-beginning 0) (match-end 0)) )
	(scratch "\nstripping echos.\n")
	(nb-delete-lines beg input end t)	;strip input echo.
	(goto-char beg)			; Delete "Begin".
	(while (re-search-forward "\\(\n\\s *;?\\s *\\)\n" end t)
	  (goto-char (match-beginning 0))
	  (delete-region (match-beginning 1) (match-end 1)))
	(goto-char beg)			; Delete blank lines.
	(while (re-search-forward "\\(\n\\s *;?\\s *\\)\\(\n\\|\b\\)" end t)
	  (goto-char (match-beginning 0))
	  (delete-region (match-beginning 1) (match-end 1)))
	(goto-char beg)			; If there's nothing left but
	(if (looking-at "\\([ \n\t]*\\)\b") ; whitespace, remove it.
	    (delete-region (match-beginning 1) (match-end 1)))
	))
    )
  "Adjust the output text from mupad."
  )

(defun nb-find-mupad-process ()
  "Find a mupad process that is already running. If there is none,
return nil."
  (let ((list (process-list))		;Get a list of processes.
	(proc) (mupad nil) (command))
    (while (and list (not mupad))		;Look at each one.
      (setq proc (car list))
      (setq command (process-name proc))
      (if (string-match "mupad" command)
	  (setq mupad proc))
      (setq list (cdr list))
      )
    mupad				; Return the result of the search.
    )
  )

(defconst mupad-start-process
  (lambda (&optional old)
    (interactive)
    (if (and old (nb-find-mupad-process))
	(setq nb-process (nb-find-mupad-process))
      (save-excursion
	(let ((buff (generate-new-buffer "*mupad*"))
	      (notebook-buffer (current-buffer))
	      (regexp nb-output-regexp))
	  (setq nb-process
		(start-process "mupad"	; name of process
			       buff
			       "mupad"	; Program name.
;;			       "-S"	; Start without banner.
			       ))
	  (set-process-filter nb-process 'nb-filter)
;;	  (process-send-string nb-process
;;			       "TEXTWIDTH := 80;\n")
	  (set-buffer buff)
	  (setq nb-output-regexp regexp)
	  ))
      )
    (if nb-process
	(setq mode-line-process
	      (format ": %s" (process-name nb-process)))
      )
    )
  "Start a mupad process.  If the optional argument OLD is nil, a new
process will be started, even if an old one already exists.  "
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use Euoropean codes in notebook mode.
(let ((l 160) (h 255))
     (while (<= l h)
       (if (and (>= l ?\ ) (< l 127))
	   (aset nb-display-table l nil)
	 (aset nb-display-table l (vector l)))
       (setq l (1+ l)))
     )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar mupad-notebook-mode-map nil "Keymap for mupad mode.")
(setq mupad-notebook-mode-map (copy-keymap tex-mode-map))
(nb-setup-keymap mupad-notebook-mode-map)
(define-key mupad-notebook-mode-map "\C-c\C-f" 'tex-mupad-file)
;; (define-key mupad-notebook-mode-map [C-c end] 'nb-set-end-of-notebook)

(define-key mupad-notebook-mode-map
  [menu-bar tex tex-file]
  '("Run TeX on Notebook" . tex-mupad-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Convert to TeX and run TeX on the file:

;; This starts tex-mode, and then modifies it a bit.
(defun nb-start-tex-mode ()		
  (tex-mode)				; run tex.
  )

(defun mupad-to-tex-name (name)
  (string-match "\\(.*\\)\.m" name)	; anything before the .m
  (concat (substring name (match-beginning 1) (match-end 1))
	  ".tex")
  )

(defun tex-mupad-file ()
  "Convert this file to a TeX file, and run the command tex-file on it."
  (interactive)
  (let ((mupad-name (buffer-file-name))
	(mupad-buffer (current-buffer))
	(file-dir (file-name-directory (buffer-file-name)))
	(tex-name)
	(cell-regexp nb-cell-regexp)	; Keep track of the local variable.
	(tex-buffer) )
    (setq tex-name (mupad-to-tex-name mupad-name))
    (setq tex-buffer (find-file-noselect tex-name))
    (save-excursion
      (set-buffer tex-buffer)
      (delete-region (point-min) (point-max))
      (insert-buffer-substring mupad-buffer)
      (goto-char (point-min))		; Get rid of special characters
      (while (re-search-forward cell-regexp nil t)
	(goto-char (- (match-end 0) 1))
	(delete-char 1)
	(insert "\\end{verbatim}")
	(goto-char (match-end 3))
	(delete-char 1)
	(goto-char (match-beginning 0))
	(delete-char 1)
	(insert "\\begin{verbatim}")
	)
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
