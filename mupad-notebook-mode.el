;; $Id$
;; Set up a notebook mode for mupad.

(provide 'mupad-notebook-mode)
(require 'mupad-notebook-mode)
(require 'notebook-mode)
(require 'octave-notebook-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun octave-notebook-mode ()
  "Major mode for a Mupad notebook.  It is a combination of TeX mode
and Notebook mode.

See documentation of `notebook-mode` for a description of a notebook,
and cells.

Special commands:
\\{mupad-notebook-mode-map}

See documentation for tex-mode for other commands.

In addition to `mupad-notebook-hook', and whatever hooks tex-mode runs, 
`notebook-mode-hook' is also run.
"
  (interactive)
  (kill-all-local-variables)
  (nb-choose-text-mode)	;; pick a text mode (to inherit font-locking, etc).
  (nb-mupad-regexpressions)
  (setq nb-adjust-input-string mupad-notebook-adjust-input-string)
  (setq nb-adjust-output-string mupad-notebook-adjust-output-string)
  (setq nb-start-process mupad-start-process)
  (notebook-mode-initialize 'octave-mode nil) ; this also sets up font-lock mode.
  (setq major-mode 'mupad-notebook-mode)
  (setq mode-name "MuPAD Notebook")
  (use-local-map			; M: notebook modes need the special keys.
   (nb-setup-keymap (copy-keymap (current-local-map))))

  (setq indent-tabs-mode nil)		; PENDING: needed?
  (setq nb-process (nb-find-process "mupad"))
  (if nb-process
    (setq mode-line-process (format ": %s" (process-name nb-process))))
  (run-hooks 'notebook-mode-hook 'mupad-notebook-hook)
  )

(defun nb-mupad-regexpressions ()
  "Set regular expressions for mupad mode."
  (let ( (name "\\([^ \t\n\b\f)(]*\\)") ; Possible name of function or cell.
	 (ws "\\s *")			; Whitespace.
	 (body "\\([^\b]*\\)")		; Body of input or output region.
	 )
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
	;;(scratch (format "Adjusting input for cell named '%s'\n" name))
	(goto-char (overlay-start (nb-cell-input-overlay cell)))
	(concat "\"Beg\"  .  \"in " buffer " " name "\";\n"	
		string ";\n"
		"\"End \"  .   \"" name "\";\n")
	)))
  "Adjust STRING so that it can be sent to mupad."
  )

(defconst mupad-notebook-adjust-output-string
  (lambda (i-beg i-end o-beg o-end cell)
    ;;(scratch (format
    ;;  "Adjusting output. input=<%d,%d> output=<%d,%d> name = %s\n"
    ;;      i-beg i-end o-beg o-end (nb-cell-name cell)))
    (let ((input (buffer-substring i-beg i-end))
	  (beg (make-marker))
	  (end (make-marker))
	  )
      (move-marker beg o-beg)
      (move-marker end o-end)
      (save-excursion                     ;remember point.
	;;(scratch (concat "Working on ---->"
	;;	  (buffer-substring o-beg o-end)
	;;	  "<---\n"))
	(goto-char end)                   ; strip ending off.
	(forward-char)
	(while (re-search-backward "\\(\n.*\"End \"[^\b]*\\)\b" beg t) 
	  ;;(scratch (format "removing End (%d - %d) '%s'\n"
	  ;;	   (match-beginning 1) (match-end 1)
	  ;;	   (buffer-substring (match-beginning 1)
	  ;;			     (match-end 1))))
	  (delete-region (match-beginning 1) (match-end 1))
	  ;;(scratch (concat "What's left ---->"
	  ;;	   (buffer-substring beg end)
	  ;;	   "<---\n"))
	  )
	;;(scratch "removing prompts ")
	(goto-char beg)                   ; strip all prompts.
	(while (re-search-forward  "\\s *>>\\$.*\n" end t) 
	  ;;(scratch "- ")
	  (delete-region (match-beginning 0) (match-end 0)) )
	(goto-char beg)                   ; strip all prompts.
	(while (re-search-forward  "\\s *\\(>>\\|&>\\)\\s *" end t) 
	  ;;(scratch "- ")
	  (delete-region (match-beginning 0) (match-end 0)) )
	;;(scratch "\nstripping echos.\n")
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
	(set-marker beg nil)
	(set-marker end nil)
	))
    )
  "Adjust the output text from mupad."
  )


(defconst mupad-start-process
  (lambda (&optional old)
    (interactive)
    (if (and old (nb-find-process "mupad"))
	(setq nb-process (nb-find-process "mupad"))
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
	  (process-kill-without-query nb-process)
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
;; PENDING: Use Euoropean codes in notebook mode.  (Does this still work?)
;; (let ((l 160) (h 255))
;;      (while (<= l h)
;;        (if (and (>= l ?\ ) (< l 127))
;; 	   (aset nb-display-table l nil)
;; 	 (aset nb-display-table l (vector l)))
;;        (setq l (1+ l)))
;;      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
