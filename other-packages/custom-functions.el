;;; -*- lexical-binding: t -*-

;; Couple of handy function

;; widen window based on text length
;; =======================================================
(defun fit-window-to-buffer-width (&optional window max-width min-width)
  "Fit Window according to its buffer's width"
  (interactive)
  (let ((fit-window-to-buffer-horizontally 'only))
    (fit-window-to-buffer window nil nil max-width min-width)))
    
;;--------------------------------------------------------
;; split window right (will be used for dired)
;; =======================================================
(defun dired-in-vertical-split ()
  (interactive)
  (split-window-right)
  (other-window 1)
 )

;;--------------------------------------------------------
;; split window vertical
;; =======================================================
(defun buffer-in-vertical-split ()
  (interactive)
  (split-window-horizontally);; een beetje rare term, maar ze bedoelen dat 2 windows horizontaal worden gecreerd
  (other-window 1))

;;--------------------------------------------------------
;; avy-run - this function will go to a line and a word can be searched in that line
;; =======================================================
(defun yr/avy-run (arg)
(interactive "P")
(avy-goto-line)
(let ((avy-all-windows nil))
  (cl-letf (((symbol-function 'avy--find-visible-regions) (lambda (&rest args) `((,(point-at-bol) . ,(point-at-eol))))))
    (avy-goto-char-timer arg))))

(defun yr/avy-run-in-line (arg)
(interactive "P")
;;(avy-goto-line)			       
(let ((avy-all-windows nil))
  (cl-letf (((symbol-function 'avy--find-visible-regions) (lambda (&rest args) `((,(point-at-bol) . ,(point-at-eol))))))
    (avy-goto-char-timer arg))))



;;--------------------------------------------------------
;; delete next char
;; =======================================================
(defun delete-next-char ()
  "Delete the character after point."
  (interactive)
  (delete-char 1))

;;--------------------------------------------------------
;; indent text right (incrementally)
;; =======================================================
(defun my-indent-rigidly-right (arg)
  "Indent region rigidly to the right and keep region active."
  (interactive "p")
  (if (region-active-p)
      (progn
	(indent-rigidly (region-beginning) (region-end) arg)
	(setq deactivate-mark nil))
      (message "Region is not active")))

;;--------------------------------------------------------
;; indent text left (incrementally)
;; =======================================================
(defun my-indent-rigidly-left (arg)
  "Indent region rigidly to the right and keep region active."
  (interactive "p")
  (if (region-active-p)
      (progn
	(indent-rigidly (region-beginning) (region-end) (- arg))
	(setq deactivate-mark nil))
      (message "Region is not active")))

;;--------------------------------------------------------
;; widen window incrementally
;; =======================================================

(defun widen-window-incrementally ()
  "Widen the window by 10 columns."
  (interactive)
  (enlarge-window-horizontally 10)
  (enlarge-window 10))

;;--------------------------------------------------------
;; Delete other windows
;; =======================================================

(defun delete-other-windows-except-active ()
 "Delete all other windows except the active one."
 (interactive)
 (delete-other-windows (get-buffer-window (current-buffer) t)))

;;====================== Hooks ============================


;;--------------------------------------------------------
;; switch to Eshell and change to insert-mode
;; =======================================================
(defun my-switch-window-hook ()
 (interactive)
 "Function to run when switching to another window."
    (when (equal (buffer-name) "*eshell*")
      (boon-set-insert-state)))

;;--------------------------------------------------------
;; toggle-window
;; =======================================================

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))


(defun my-split-window-below ()
  (interactive)
  (split-window-below))

(defun my-split-window-right ()
  (interactive)
  (split-window-right))

;;================== Utility functions =============================


;;--------------------------------------------------------
;; a hook to switch to insert-mode when Eshell is opened.
;; =======================================================
;;(add-hook 'window-configuration-change-hook 'my-switch-window-hook)

;;--------------------------------------------------------
;; Interesting function call-interactively, my-switch-window-hook will run after ace-window
;; =======================================================
(advice-add 'ace-window :after '(lambda (&rest args)
                                  (call-interactively 'my-switch-window-hook)))
                                  
;;--------------------------------------------------------                                  
;; search files recursively !
;; =======================================================
(advice-add 'dired-recent-open :after '(lambda (&rest args)
                                   (call-interactively 'find-file-in-current-directory)))
                                   
;;--------------------------------------------------------
;; open dired in vertical window
;; =======================================================
;;(advice-add 'dired-in-vertical-split :after '(lambda (&rest args)
  ;;                                             (call-interactively 'dired)))
                                               
;;--------------------------------------------------------
;; consult-buffer is run after a vertical buffer is opened
;; =======================================================
;;(advice-add 'buffer-in-vertical-split :after '(lambda (&rest args)
  ;;                                             (call-interactively 'consult-buffer)))
;;--------------------------------------------------------
;; consult-buffer is run after a vertical window is opened
;; =======================================================
(advice-add 'my-split-window-right :after '(lambda (&rest args)
                                               (call-interactively 'consult-buffer)))
;;--------------------------------------------------------
;; consult-buffer is run after a horizontal window is opened
;; =======================================================
(advice-add 'my-split-window-below :after '(lambda (&rest args)
                                          (call-interactively 'consult-buffer)))
;;--------------------------------------------------------
;; Other window call hydra so that the switching of windows stays active
;; ==================================================================
(advice-add 'other-window :after '(lambda (&rest args)
                                          (call-interactively 'hydra-other-win/body)))

;;--------------------------------------------------------
;; Boon-insert is run after Wdired is activated
;; =======================================================

(defun my-wdired ()
  (interactive)
  (wdired-change-to-wdired-mode))

(advice-add 'my-wdired :after '(lambda (&rest args)
                                          (call-interactively 'boon-insert)))

;;--------------------------------------------------------
;; hulpfunctie om te checken of wdired is actief
;; =======================================================

(defun my/check-dired-mode-active (func)
    (if (derived-mode-p 'wdired-mode)
	(funcall func)))

;;======================================================================================

(provide 'custom-functions)
