;; $Id$
;;(progn (eval-buffer) (nb-reload-modes))
;;(progn (setq debug-on-error t) (eval-buffer) (nb-reload-modes))
;;(progn (setq edebug-all-defs t) (eval-buffer) (nb-reload-modes))


;; I just put some junk code here so that I can debug the notebook mode.
;; You should not install this stuff for real use.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DEBUGGING COMMANDS:

(global-set-key [M-f4] 'nb-reload-modes) ; for debugging.
(global-set-key [M-f3] 'debug-syntax-and-face) ; for debugging.
(global-set-key [M-f7] 'nb-add-debug-buffer) ; for debugging.
(global-set-key [M-f8] 'nb-find-cell-by-position) ; for debugging.
(global-set-key [M-f9] 'open-message-buffer) ; for debugging.
(global-set-key [M-f10] 'debug-syntax-and-face) ; for debugging.

(require 'font-lock)

(defun nb-reload-modes ()
  "Reload the notbook modes."
  (interactive)
  (let ((buf (current-buffer)))
    (save-current-buffer
      (save-excursion
	(set-buffer "debug-notebook.el")
	(eval-buffer)
	(find-and-load "notebook-mode.el")
	(find-and-load "matlab-notebook-mode.el")
	(find-and-load "mupad-notebook-mode.el")
	(find-and-load "octave-notebook-mode.el")
	(find-and-load "debug-notebook.el")
	(find-and-load "font-lock-debug.el")
	(message "Finished reloading all the notebook modes.")
	))
    (switch-to-buffer buf)
    ))



(defun find-and-load (file)
  "Find the file and eval it, if the file is already a buffer, use the buffer
instead."
  ;; (let (buf  (find-file-noselect file))
  (save-current-buffer
    (let ((buf  (find-file file)))
      (message (format "loading file %s." buf))
      (save-excursion
	(eval-buffer buf)
	))))


(defun nb-debug-font-lock-stuff ()
  "Use the debug font-lock stuff.."
  (interactive)
  (nb-turnoff-font-lock)
  (nb-reload-modes)
  (setq font-lock-fontify-region-function 'nbd-font-lock-default-fontify-region)
  (notebook-mode)
  )

(defun nb-redo-matlab-mode ()
  "Reload this file and convert to notbook mode."
  (interactive)
  (nb-turn-off-mode)
  (nb-reload-modes)
  (matlab-notebook-mode)
  )


(defun nb-redo-mupad-mode ()
  "Reload this file and convert to notbook mode."
  (interactive)
  (nb-turn-off-mode)
  (nb-reload-modes)
  (mupad-notebook-mode)
  )


(defun nb-redo-octave-mode ()
  "Reload this file and convert to notbook mode."
  (interactive)
  (nb-turn-off-mode)
  (nb-reload-modes)
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
(defun scratch (string)
  (if debug-on-error 
      (save-excursion
        (set-buffer "*scratch*")
        (goto-char (point-max))
        (insert string))))

(defun nb-add-debug-buffer (name)
  "Create a debug buffer for debugging input-output"
  (interactive "Mbuffer name:")
  (setq notebook-debug-input-buffer (get-buffer-create (concat name "-input")))
  (setq notebook-debug-output-buffer (get-buffer-create (concat name "-output")))
  )

(setq message-log-max 500)

(add-hook 'font-lock-mode-hook
      '(lambda ()
	 (message (format "Hello. I am running font lock in mode %s."
			  major-mode))))



(defun open-message-buffer ()
  "open message."
  (interactive)
  (make-frame-command)
  (switch-to-buffer "*Messages*")
  )

(defun print-font-debug-stuff ()
  "print stuff."
  (interactive)
  (set (make-variable-buffer-local 'nbd-font-print) t)
  )

(defvar nbd-font-print nil "debug print stuff")

(defun nbd-message (&rest args)
  (if nbd-font-print
      (message (apply 'format  args))))

  


(defun debug-syntax-and-face (s table)
  "Print some stuff."
  (interactive (list  "--current--" (syntax-table)))
  (message (concat
	    (string (char-syntax ?\"))
	    (string (char-syntax ?$) )
	    (string (char-syntax ?z))
	    (string (char-syntax ?\())
	    (string (char-syntax ?\)))
	    "  "
	    (if (equal table         (syntax-table)) ", current")
	    (if (equal table        nb-syntax-table) ", nb")
	    (if (equal table   sh-mode-syntax-table) ", shell")
	    (if (equal table text-mode-syntax-table) ", text")
	    (if (equal table font-lock-syntax-table) ", font")
	    "  --> "
	    s))
  ) 
