;;; Modeline                                         -*- lexical-binding: t -*-
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Install mood-line if you haven't already
 (use-package mood-line
   :ensure t
   :config
   (mood-line-mode)
   )

(after! mood-line


(defun boon-state-string ()
  "Return a string describing the current state."
  (cond
   (boon-command-state "CMD")
   (boon-insert-state  "INS")
   (boon-special-state "SPC")
   (t "???")))


  (setq mood-line-format
      (mood-line-defformat
       :left
       (((mood-line-segment-buffer-status) . " ")
        ((mood-line-segment-buffer-name)   . " | ")
        (mood-line-segment-major-mode))
       :right
       (
        ((mood-line-segment-cursor-position)    . " | ")
        (boon-state-string)
        my-hydra-message
	)
       )))











