;; $Id$
;;(eval-buffer)


;; I just put some junk code here so that I can debug the notebook mode.
;; You should not install this stuff for real use.

;;; DEBUGGING COMMANDS:

(global-set-key [M-f5] 'nb-reload-modes) ; for debugging.

(defun nb-reload-modes ()
  "Reload the notbook modes."
  (interactive)
  (save-current-buffer
    (save-excursion
      (set-buffer "debug-notebook.el")
      (eval-buffer)
      (eval-buffer (find-file-noselect "notebook-mode.el"))
      (eval-buffer (find-file-noselect "matlab-notebook-mode.el"))
      (eval-buffer (find-file-noselect "mupad-notebook-mode.el"))
      (eval-buffer (find-file-noselect "octave-notebook-mode.el"))
      )))


(defun nb-redo-matlab-mode ()
  "Reload this file and convert to notbook mode."
  (interactive)
  (nb-turn-off-mode)
  (mb-reload-modes)
  (matlab-notebook-mode)
  )


(defun nb-redo-mupad-mode ()
  "Reload this file and convert to notbook mode."
  (interactive)
  (nb-turn-off-mode)
  (mb-reload-modes)
  (mupad-notebook-mode)
  )


(defun nb-redo-octave-mode ()
  "Reload this file and convert to notbook mode."
  (interactive)
  (nb-turn-off-mode)
  (mb-reload-modes)
  (octave-notebook-mode)
  )



(defun turn-debug-on () 
  (interactive)
  (print (format  "Debug was:%s is now on" debug-on-error))
  (setq debug-on-error t))
(defun turn-debug-off () 
  (interactive) 
  (print (format  "Debug was:%s is now off" debug-on-error))
  (setq debug-on-error nil))
;;;(global-set-key [f3] 'turn-debug-on)
;;;(global-set-key [f4] 'turn-debug-off)


(defun kill-all-overlays (list)
  (if list
      (progn 
	(delete-overlay (car list))
	(kill-all-overlays (cdr list)))))

(defun nb-turn-off-mode ()
  "turn off notebook mode."
  (interactive)
  (setq inhibit-read-only t)
  (setq buffer-display-table nil) 
  (remove-text-properties (point-min) (point-max) (list 'read-only t))
  (setq inhibit-read-only nil)
  (kill-all-overlays (car (overlay-lists)))
  (kill-all-overlays (cdr (overlay-lists)))
  (nb-kill-process)
  )

(defun debug-notebook-to-script ()
  "" (interactive)
  (global-set-key [f12] 'debug-notebook-to-script)
  (set-buffer "notebook-mode.el")
  (eval-buffer)
  (find-file "samp4.shell")
  (notebook-to-script "samp4.sh")
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
