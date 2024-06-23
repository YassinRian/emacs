;;; -*- lexical-binding: t -*-

;;; =========================================== hydra timer function ============================================================;;;

 (require 'pretty-hydra)


(defvar my-temporary-hydra-timer nil
  "Timer object for the timeout of my-temporary-hydra.")

(defvar my-temporary-hydra-option-selected nil
  "Flag to indicate if an option has been selected in my-temporary-hydra.")
    
(defvar my-temporary-original-cursor-color nil
  "Variable to store the original cursor color.")
 
(defvar my-hydra-message nil
  "variable to store the hydra message.")

(defun my-temporary-hydra-timeout (activation-char hydra-symbol)
  "Function to execute when a temporary hydra times out.
Inserts ACTIVATION-CHAR unless an option has been selected.
HYDRA-SYMBOL is the symbol representing the hydra."
  (setq my-temporary-hydra-timer nil)
  ;; Deactivate Hydra
  (when (bound-and-true-p hydra-symbol)
    (when (hydra-get-property hydra-symbol :is-active)
      (hydra-deactivate hydra-symbol)))
  ;; Insert activation character if no option has been selected
  (unless my-temporary-hydra-option-selected
    (insert (char-to-string activation-char))
    (setq hydra-deactivate t)
    (hydra-keyboard-quit)))

;; Handy if we want to add a extra hydra menu through a hydra for SPC
;;(bind-key "SPC" '(lambda () (interactive) (my-temporary-hydra-wrapper ?\s 'hydra-files/body 0.5)) boon-command-map)

;; (defun my-temporary-hydra-timeout (activation-char hydra-symbol)
;;   "Function to execute when a temporary hydra times out.
;; Inserts ACTIVATION-CHAR unless an option has been selected.
;; HYDRA-SYMBOL is the symbol representing the hydra."
;;   (setq my-temporary-hydra-timer nil)
;;   ;; Deactivate Hydra
;;   (when (bound-and-true-p hydra-symbol)
;;     (when (hydra-get-property hydra-symbol :is-active)
;;       (hydra-deactivate hydra-symbol)))
;;   ;; Insert activation character if no option has been selected
;;   (unless my-temporary-hydra-option-selected
;;     (if (eq activation-char ?\s)
;; 	(boon-drop-mark)
;;       (insert (char-to-string activation-char)))
;;     (setq hydra-deactivate t)
;;     (hydra-keyboard-quit)))
    
    
(defun my-temporary-hydra-wrapper (activation-char hydra timeout)
  "Create a temporary hydra with options and remove it after a timeout if no option is selected within TIMEOUT seconds.
ACTIVATION-CHAR is the character to be inserted upon timeout.
HYDRA is the pre-defined hydra symbol.
TIMEOUT is the time to wait before timing out."
  (interactive "cActivation character: \nXHydra body: \nnTimeout (seconds): ")
  (setq my-temporary-hydra-option-selected nil)
  (let* ((timeout-seconds (truncate timeout))  ;; Integer part of the timeout
         (timeout-milliseconds (truncate (* 1000 (mod timeout 1))))  ;; Milliseconds part of the timeout
         (timeout-fraction (truncate (* 1000 (mod timeout 1))))  ;; Fractional part of the timeout
         (timeout-string (format "%d.%d" timeout-seconds timeout-fraction)))  ;; Combined seconds and milliseconds
         (setf my-temporary-original-cursor-color (frame-parameter nil 'cursor-color))
    (setq my-temporary-hydra-timer
          (run-at-time
           timeout-string
           nil
           #'my-temporary-hydra-timeout
           activation-char hydra)))
  (funcall hydra) ; Assuming `hydra` is the symbol of the hydra definition
  )

;;=================================================================================================

(defhydra hydra-avy (:color red)
  ("g" avy-goto-line "go to line")
  ("f" avy-goto-char-in-line "go to char")
  ("n" avy-next "go to next")
  ("p" avy-prev "go to prev")
) 

;; (defhydra hydra-files (:hint nil)
;;   "
;;   files: _f_ search in recent dir  _d_ search file in dir _r_ search recent files _w_ write to new file
;;   "
;;   ("f" dired-recent-open :exit t)
;;   ("r" consult-recent-file :exit t)
;;   ("d" consult-dir :exit t)
;;   ("w" write-file)
;;   ("," nil :color blue)
;;   )


;; hydra for the insert-mode: it makes navigation easier in the insert-mode
(defhydra my-example-hydra (:hint nil :idle 2)

  ("f" (progn (setq my-temporary-hydra-option-selected t) (forward-char)) :exit nil)
  ("d" (progn (setq my-temporary-hydra-option-selected t) (backward-char)) :exit nil)
  ("a" (progn (setq my-temporary-hydra-option-selected t) (boon-beginning-of-line)) :exit t)
  ("e" (progn (setq my-temporary-hydra-option-selected t) (boon-end-of-line)) :exit t)
  ("w" (progn (setq my-temporary-hydra-option-selected t) (boon-open-line-and-insert)) :exit t)
  ("s" (progn (setq my-temporary-hydra-option-selected t) (boon-open-next-line-and-insert)) :exit nil)
  ("j" (progn (setq my-temporary-hydra-option-selected t) (undo)) :exit nil)
  ("F" (progn (setq my-temporary-hydra-option-selected t) (delete-next-char)) :exit nil)
  (";" (progn (setq my-temporary-hydra-option-selected t) (insert ";")) :exit t)
  )


;; ;; ;; Hydra to switch state from Boon-insert-map
;; (defhydra my-example-hydra_2 (:hint nil :idle 10)
  
;;   ("d" (progn (setq my-temporary-hydra-option-selected t) (boon-set-command-state)) :exit t)
;;   ) 

 ;; (defun my/check-dired-mode-active (func)
 ;;    (if (derived-mode-p 'wdired-mode)
 ;; 	(funcall func)))

;; (my/check-dired-mode-active 'wdired-finish-edit)

(defhydra hydra-change-mode (:color blue :body-pre (insert "f") :idle 1.0 :timeout 0.5)
  ("d" (progn (delete-char -1)
	      (my/check-dired-mode-active 'wdired-finish-edit)
	      (my-corfu-quit)
	      (boon-set-command-state))
              ))


;; Hydra for vertico mode
 (defhydra hydra-vertico (:hint nil :color blue)
 ("i" (progn (setq my-temporary-hydra-option-selected t) (vertico-previous)) :exit nil)
 ("o" (progn (setq my-temporary-hydra-option-selected t) (vertico-next)) :exit nil)
 ) 

;; Hydra for vertico mode
 (defhydra hydra-other-win (:hint nil :color blue)
   ("a" other-window :exit nil)) 


;; ----------------- Pretty hydra's -----------------------

(pretty-hydra-define hydra-windows (:color blue :quit-key "q")
  ("Actions"
   (("x" ace-delete-window "delete")
    ("a" ace-window "select" :exit nil)
    ("c" ace-swap-window "swap" :exit nil)
    ("m" delete-other-windows-except-active "maximize")
    ("t" toggle-window-split)
    )
   
   "Split"
   (("s" my-split-window-right "Split right")
    ("b" my-split-window-below "Split below")
    )

   "Resize"
   (
    ("k" balance-windows "balance")
    ("l" fit-window-to-buffer-width "fit")
    ("h" widen-window-incrementally "widen" :exit nil)
    )
   ))


(pretty-hydra-define hydra-search (:color blue :quit-key "q")
  (
   "Search"
   (("s" phi-search "Search forward")
    ("w" phi-search "Search backward"))
   "Replace"
   (("r" phi-replace-query "Replace"))
   "RegExp"
   (("i" isearch-forward-regexp)
    ("o" isearch-backward-regexp)
    ("u" isearch-query-replace-regexp))
   "Multi-line edit"
   (("e" iedit-mode "Edit multi symbols")
    ("g" iedit-rectangle-mode "Edit multi lines")
    ("c" rectangle-mark-mode "Mark rectangle"))
   ))

;; Hydra is activated during selections
(pretty-hydra-define hydra-selection (:color blue :quit-key "q")
  (
   "Capitalize"
   (("u" upcase-region "Upcase region")
    ("w" downcase-region "Downcase region"))
   "Indent"
   (("J" my-indent-rigidly-left "Tab left inc" :exit nil)
    (":" my-indent-rigidly-right "Tab right inc" :exit nil)
    ("TAB" indent-rigidly-right "Tab right" :exit nil)
    ("DEL" indent-rigidly-left "Tab left")
    )
   "Move"
   (("I" drag-stuff-up "Move up" :exit nil)
    ("O" drag-stuff-down "Move down" :exit nil))
   "Expand Selection"
   (("e" er/expand-region "Expand"))
   ))


(pretty-hydra-define hydra-buffers (:color blue :quit-key "q")
  (
   "Navigate"
   (("j" previous-buffer :exit nil)
    (";" next-buffer :exit nil))

   "Open in other Win"
   (("b" split-window-right "Open buffer"))

   "buffer funcs"
   (("k" kill-buffer "Close buffer")
    ("K" only-current-buffer "Kill other buffers")
    ("r" rename-buffer "Rename buffer")
    ("w" write-file "Write to new file")
    ("n" scratch "New scratch buffer"))
   ))
  

(pretty-hydra-define hydra-dired (:color blue :quit-key "q")
  (
   "Copy/Move/Paste"
   (("w" dired-ranger-copy "Copy" :exit nil)
    ("x" dired-ranger-move "Move" :exit nil)
    ("y" dired-ranger-paste "Paste" :exit nil))

   "De/Activate x-map"
   (("a" activate-boon-x-map "Activate" :exit t)
    ("d" deactivate-boon-x-map "Deactivate" :exit t))

   "Revert buffer"
   (("r" revert-buffer "Refresh" :exit t))
   ))

(pretty-hydra-define hydra-files (:color blue :quit-key "q")
  (
   "Find file"
   (
    ("a" dired-recent-open "Search file in dirs" :exit t)
    ("d" consult-dir-dired "Show current folder" :exit t)
    ("f" find-file "Find file" :exit t)
    ("r" consult-recent-file "Search recent files" :exit t)
   )
   
   "Write current buffer to new file"
   (
    ("w" write-file "Write to new file" :exit t)
   )

   "Search"
   (
    ("t" consult-find "Consult-find")
    ("g" search-string-in-directory "Search string in dir")
   )
   ))



;;=======================
(provide 'hydra-functions)


