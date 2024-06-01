;; let op capslock is ctrl, de oude capslock werkt niet ...weet niet of het handig is

;; gs consult-line ;; gd consult-dir ;; gg consult-buffer ;; gb hydra-buffer
;; ga other-window
;; ws phi-search
;; wf hydra-files
;; ww hydra-windows
;; ee hydra-selection

;; (define-key boon-select-map "@"  'boon-select-occurences)
;; (define-key boon-select-map "*"  'boon-select-word-occurences)
;; (define-key boon-select-map "#"  'boon-select-all)
;; (define-key boon-select-map " "  'boon-select-line)
;; (define-key boon-select-map  "\"" 'boon-select-outside-quotes)
;; (define-key boon-select-map  "'" 'boon-select-to-mark)


(after! boon
(after! hydra
(after! vertico

(require 'hydra-functions)
(require 'custom-functions)

;; Added to BOON-INSERT-MAP
(bind-key ";" '(lambda () (interactive) (my-temporary-hydra-wrapper ?\; #'my-example-hydra/body 0.3)) boon-insert-map)
(bind-key ";" '(lambda () (interactive) (my-temporary-hydra-wrapper ?\; #'hydra-vertico/body 0.3)) vertico-map)
(bind-key "f" #'hydra-change-mode/body boon-insert-map) ;; the better solution

;; Added to BOON-GOTO-MAP
(bind-key "g" #'consult-buffer boon-goto-map)                ; g-g seach buffer
(bind-key "l" #'yr/avy-run boon-goto-map)                    ; g-l gotoline and search word
(bind-key "s" #'hydra-selection/body boon-goto-map)
(bind-key "a" #'other-window boon-goto-map)
(bind-key "v" #'vundo boon-goto-map)

;; Added to boon-moves-map
(bind-key "J" #'join-line boon-moves-map)                    ; S-j join-line
(bind-key "K" #'good-scroll-down-full-screen boon-moves-map)
(bind-key "L" #'good-scroll-up-full-screen boon-moves-map)
(bind-key "f" #'yr/avy-run-in-line boon-moves-map)
(bind-key "F" #'avy-goto-char-timer boon-moves-map)
(bind-key "h" #'boon-splice boon-moves-map)
(bind-key "H" #'yank-pop boon-moves-map)

;; Added to Boon-backward-search-map
(bind-key "w" #'hydra-windows/body boon-backward-search-map) ; w-w Windows menu
(bind-key "f" #'hydra-files/body boon-backward-search-map)   ; w-f File menu
(bind-key "s" #'hydra-search/body boon-backward-search-map)
(bind-key "SPC" #'hydra-buffers/body boon-backward-search-map)
(bind-key "c" #'boon-set-special-state boon-backward-search-map)

;; Added to boon-forward-search-map
(bind-key "e" #'er/expand-region boon-forward-search-map)    ; e-e expand selection
(bind-key "SPC" #'consult-line boon-forward-search-map)
(bind-key "d" #'consult-dir boon-forward-search-map)

;; Added to phi-default-search-map
(bind-key "C-." #'phi-search-again-or-next phi-search-default-map)
(bind-key "C-," #'phi-search-again-or-previous phi-search-default-map)
(bind-key "C-l" #'(lambda() (interactive) (scroll-other-window -1)) phi-search-default-map)
(bind-key "C-k" #'(lambda() (interactive) (scroll-other-window 1)) phi-search-default-map)

;; --------------- Special map ------------------------------------------
;; Dired makes use of Special-mode, so a specific mapping has to be created.

(bind-key "V" #'boon-set-command-state boon-special-map)

(add-hook! dired-mode (local-set-key (kbd "i") 'dired-previous-line))
(add-hook! dired-mode (local-set-key (kbd "o") 'dired-next-line))
(add-hook! dired-mode (local-set-key (kbd "j") 'dired-up-directory))
(add-hook! dired-mode (local-set-key (kbd ";") 'dired-find-file))
(add-hook! dired-mode (local-set-key (kbd "e") 'consult-line))
(add-hook! dired-mode (local-set-key (kbd "v") 'my-wdired))

;;(bind-key "SPC" #'hydra-dired/body dired-mode-map)


(bind-keys :map dired-mode-map
           :prefix-map my-customized-pref-map_1
	   :prefix "g"
	   ("g" . consult-buffer)
	   ("a" . other-window))

(bind-keys :map dired-mode-map
           :prefix-map my-customized-pref-map_2
	   :prefix "w"
	   ("w" . hydra-windows/body)
	   ("s" . consult-line)
	   ("SPC" . hydra-dired/body))

	
(defun activate-boon-x-map ()
  "Activate boon-x-map."
  (interactive)
  (when (bound-and-true-p boon-special-map)
    (define-key boon-special-map "x" 'boon-x-map))
  (message "boon-x-map activated"))

(defun deactivate-boon-x-map ()
  "Deactivate boon-x-map."
  (interactive)
  (when (bound-and-true-p boon-special-map)
    (define-key boon-special-map "x" nil))
  (message "boon-x-map deactivated"))

;; hook om boon-x-map te deactiveren
(add-hook 'dired-mode-hook 'deactivate-boon-x-map )

;; viper
(autoload 'viper-ex "viper")
(bind-key ":" #'viper-ex boon-moves-map)

)))


	
