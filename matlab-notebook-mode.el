;; $Id$
;; Set up a notebook mode for matlab.

(provide 'matlab-notebook-mode)
(require 'matlab-notebook-mode)
(require 'notebook-mode)
(require 'octave-notebook-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun matlab-notebook-mode ()
  "Major mode for a Matlab notebook.  It is a combination of TeX mode
and Notebook mode.

See documentation of `notebook-mode` for a description of a notebook,
and cells.

Special commands:
\\[matlab-notebook-mode-map]

In addition to `matlab-notebook-hook', and whatever hooks the text mode runs, 
`notebook-mode-hook' is also run.

"
  (interactive)
  (kill-all-local-variables)
  (nb-choose-text-mode)	;; pick a text mode (to inherit font-locking, etc).
  (nb-octave-regexpressions)
  (setq nb-adjust-input-string octave-notebook-adjust-input-string)
  (setq nb-adjust-output-string matlab-notebook-adjust-output-string)
  (setq nb-start-process matlab-start-process)
  (notebook-mode-initialize 'octave-mode nil) ; this also sets up font-lock mode.
  (setq major-mode 'matlab-notebook-mode)
  (setq mode-name "Matlab Notebook")
  (use-local-map			; M: notebook modes need the special keys.
   (nb-setup-keymap (copy-keymap (current-local-map))))

  (setq indent-tabs-mode nil)		; PENDING: needed?
  (setq nb-process (nb-find-process "matlab"))
  (if nb-process
    (setq mode-line-process (format ": %s" (process-name nb-process))))
  (run-hooks 'notebook-mode-hook 'matlab-notebook-mode--hook)
  )

;;
(defvar matlab-notebook-mode-hook nil
  "If this is non-nil, it is the hook that's run for any shell notebook.")

;;
(defconst matlab-notebook-adjust-output-string
  (lambda (i-beg i-end o-beg o-end cell)
    (let ((input (buffer-substring i-beg i-end))
	  (beg (make-marker))
	  (end (make-marker))
	  )
      (move-marker beg o-beg)
      (move-marker end o-end)
      (save-excursion                     ;remember point.
	(goto-char end)                   ; strip ending off.
	(while (re-search-backward ".*fprintf.*\\f.*\n" beg t) 
	  (delete-region (match-beginning 0) (match-end 0))
	  )
	(goto-char beg)                   ; strip all prompts.
	(while (re-search-forward  "\\s *>>\\s *" end t) 
	  (delete-region (match-beginning 0) (match-end 0)) )
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
	      ))
	(goto-char beg)			; If there's nothing left but
	(if (looking-at "\n\b")		; a newline, remove it.
	    (delete-char 1))
	(set-marker beg nil)
	(set-marker end nil)
	))
    )
  "Adjust the output text from matlab."
  )

;;
(defconst matlab-start-process
  (lambda (&optional old)
    (interactive)
    (if (and old (nb-find-process "matlab"))
	(setq nb-process (nb-find-process "matlab"))
      (save-excursion
	(let ((buff (generate-new-buffer "*matlab*"))
	      (notebook-buffer (current-buffer))
	      (regexp nb-output-regexp))
	  (setq nb-process
		(start-process "matlab"	; name of process
			       buff
			       "matlab"))	; Program name.
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
  "Start a matlab process.  If the optional argument OLD is nil, a new
process will be started, even if an old one already exists.  "
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

