
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DEBUGGING COMMANDS:

;;;(global-set-key [f5] 'nb-redo-matlab-mode) ; for debugging.

(defun nb-redo-matlab-mode ()
  "Reload this file and convert to notbook mode."
  (interactive)
  (nb-turn-off-mode)
  (save-excursion
    (set-buffer (find-file-noselect "~/source/emacs/notebook/notebook.el"))
    (eval-buffer)
    (set-buffer (find-file-noselect "~/source/emacs/notebook/matlab.el"))
    (eval-buffer))
  (matlab-notebook-mode)
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DEBUGGING COMMANDS:

;;;(global-set-key [f5] 'nb-redo-mupad-mode) ; for debugging.

(defun nb-redo-mupad-mode ()
  "Reload this file and convert to notbook mode."
  (interactive)
  (nb-turn-off-mode)
  (save-excursion
    (set-buffer (find-file-noselect "~/source/emacs/notebook/notebook.el"))
    (eval-buffer)
    (set-buffer (find-file-noselect "~/source/emacs/notebook/mupad.el"))
    (eval-buffer))
  (mupad-notebook-mode)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TODO
;; Set up a syntax and stuff like tex mode.
;; Set up info line.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DEBUGGING COMMANDS:

;;;(global-set-key [f5] 'nb-redo-octave-mode) ; for debugging.

(defun nb-redo-octave-mode ()
  "Reload this file and convert to notbook mode."
  (interactive)
  (nb-turn-off-mode)
  (save-excursion
    (set-buffer (find-file-noselect "~/source/emacs/notebook/notebook.el"))
    (eval-buffer)
    (set-buffer (find-file-noselect "~/source/emacs/notebook/octave.el"))
    (eval-buffer))
  (octave-notebook-mode)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TODO
;; Set up a syntax and stuff like tex mode.
;; Set up info line.
