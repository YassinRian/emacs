;;; Coding.el           -*- lexical-binding: t -*-
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;====================================
;; Smartparens
;;
;;====================================
(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :init
  (setq sp-override-key-bindings
        '(("C-<right>" . nil)
          ("C-<left>" . nil)
          ("C-." . sp-forward-slurp-sexp)
          ("M-<backspace>" . nil)
          ("C-," . sp-forward-barf-sexp)))
  :config
  (use-package smartparens-config)
   (sp-use-smartparens-bindings)
   (sp--update-override-key-bindings)
  :commands (smartparens-mode show-smartparens-mode)
  :custom (sp-escape-quotes-after-insert nil))

;;====================================
;; Company mode
;;
;;====================================

;; (use-package company
;;   :ensure t
;;   :diminish
;;   :custom
;;   (company-idle-delay .1)
;;   (company-minimum-prefix-length 1)
;;   (global-company-mode t))

;; ;; is een beetje traag

;; (use-package company-flx
;;   :after company)

;; (use-package company-fuzzy
;;   :hook (company-mode . company-fuzzy-mode)
;;   :init
;;   (setq company-fuzzy-sorting-backend 'flx
;;         company-fuzzy-reset-selection t
;;         company-fuzzy-prefix-on-top nil
;;         company-fuzzy-trigger-symbols '("." "->" "<" "\"" "'" "@"))
;;   :custom
;;   (global-company-fuzzy-mode 1))


;;====================================
;; Corfu mode
;;
;;====================================

;; Init corfu for auto completion
(use-package corfu
  ;; Load extensions
  :ensure ( :files ( :defaults "extensions/*"))
  :demand t
  :functions
  ( corfu-mode
    global-corfu-mode)
  :bind
  ( :map corfu-map
    ("SPC" . corfu-insert-separator)
    ;; We want TAB to complete
    ("TAB" . corfu-next)
    ([tab] . corfu-next)
    ;;("/" . corfu-complete)
    ;; We don't want RET to complete
    ("RET" . nil)
    ;; We don't want next/previous line to change selection
    ("<remap> <next-line>" . nil)
    ("<remap> <previous-line>" . nil)
    ("C-l" . corfu-next)
    ("C-k" . corfu-previous))

  :custom
  ;; Enable auto completion
  ( corfu-auto t)
  ;; Keep popup only if there is a match or the separator was inserted
  ;;( corfu-quit-no-match 'separator)
  (corfu-quit-no-match t)
  ;; Quit on exact match
  ( corfu-on-exact-match 'quit)
  ;; Set auto completion to be more responsive
  ( corfu-auto-delay 0)
  ( corfu-auto-prefix 0)
  ;; Sort candidates by calling corfu-sort-function on top of display-sort-function
  ( corfu-sort-override-function #'mo-corfu-combined-sort)
  (corfu-preselect 'prompt)
  :hook
  ;; Conditionally enable Corfu in the minibuffer
  ( minibuffer-setup . corfu-enable-in-minibuffer)
  ;; Disable auto mode in eshell
  ( eshell-mode . mo-corfu-enable-no-auto)
  ;; Disable auto mode in shell
  ( shell-mode . mo-corfu-enable-no-auto)
  ;; ;; Close popup when exiting CMD insert state
  ;; ( boon-set-command-state . corfu-quit)
  :config
  (defun mo-corfu-enable-no-auto()
    "Enable corfu without auto completion."
    (setq-local corfu-auto nil)
    (corfu-mode))

  (defun mo-corfu-combined-sort (candidates)
    "Sort CANDIDATES using both display-sort-function and corfu-sort-function."
    (let ((candidates
           (let ((display-sort-func (corfu--metadata-get 'display-sort-function)))
             (if display-sort-func
                 (funcall display-sort-func candidates)
               candidates))))
      (if corfu-sort-function
          (funcall corfu-sort-function candidates)
        candidates)))

  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (setq-local corfu-echo-delay nil
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))

  ;; Send selected candidate to shell, avoiding the need to press RET
  ;; twice when popup is visible
  (defun corfu-send-shell (&rest _)
    "Send completion candidate when inside comint/eshell."
    (cond
     ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
      (eshell-send-input))
     ((and (derived-mode-p 'comint-mode)  (fboundp 'comint-send-input))
      (comint-send-input))))
  ;;
  (advice-add #'corfu-insert :after #'corfu-send-shell)
  ;;
  (global-corfu-mode)
  ;;
  (advice-add 'corfu-insert-separator :after #'(lambda () 
	(if (= corfu--index -1)
	    (when (= corfu--total 0) 
	      (corfu-quit))
	  (corfu-insert)
	  (insert " "))))

;; Rice it up 
    ;; (set-face-attribute 'corfu-default nil :background "#1e1e2e")
    ;; (set-face-attribute 'corfu-current nil :background "#2a2e38" :box "#cba6f7")
    ;; (set-face-attribute 'corfu-border nil :background "#89b4fa")
    ;; (set-face-attribute 'corfu-bar nil :background "#cba6f7")

  )


(use-package corfu-history
  :after ( corfu savehist)
  :ensure nil
  :config
  (corfu-history-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history))

;; Init corfu-popupinfo for extra info on auto-completion candidates
(use-package corfu-popupinfo
  :after corfu
  :ensure nil
  :config
  ;; Set a short popup delay
  (setq corfu-popupinfo-delay '( 0.5 . 0.5))
  (corfu-popupinfo-mode 1))

;; Init corfu-echo for auto-completion candidate doc in the echo area
(use-package corfu-echo
  :after corfu
  :ensure nil
  :config
  (corfu-echo-mode 1))

;; niet echt nodig
(use-package corfu-quick
  :after corfu
  :ensure nil
  :bind
  (:map corfu-map
    ("C-;" . corfu-quick-complete)))

;; Init corfu-terminal for using corfu in the terminal
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :functions corfu-terminal-mode
  :config
  (corfu-terminal-mode +1))

;; Init cape for completion at point extensions
(use-package cape
  :functions
  ( cape-capf-prefix-length
    cape-file cape-dabbrev)
  :custom
  ;; Do not scan every buffer with dabbrev (see dabbrev configuration)
  ( cape-dabbrev-check-other-buffers 'some)
  :config
  ;; Add completion functions
  (add-hook 'completion-at-point-functions (cape-capf-prefix-length #'cape-dabbrev 3))
  (add-hook 'completion-at-point-functions #'cape-file))

(add-to-list 'completion-at-point-functions #'cape-dabbrev)
(add-to-list 'completion-at-point-functions #'cape-emoji)
(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-elisp-block)
(add-to-list 'completion-at-point-functions #'cape-keyword)


(use-package dabbrev
  :ensure nil
  :config
  (setq dabbrev-ignored-buffer-regexps '( "^\\*.+::stderr\\*$"))
  ;; Do not scan buffers that are too big
  (setq dabbrev-check-all-buffers nil)
  (defvar mo-dabbrev-max-file-size 1000000)
  (setq dabbrev-friend-buffer-function
        (lambda (buffer)
          (< (buffer-size buffer) mo-dabbrev-max-file-size))))

;;====================================
;; Clojure mode
;;
;;====================================
(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode))
  :init
  (defun my-clojure-mode-hook ()
    (smartparens-mode 1)
    (eglot-ensure)
    ;;(company-mode)
    )
    (add-hook 'clojure-mode-hook #'my-clojure-mode-hook))

;;====================================
;; Cider
;;
;;====================================

(use-package cider
  :ensure t
  :defer t
  :init (add-hook 'cider-mode-hook #'clj-refactor-mode)
  :diminish subword-mode
  :config
  (setq nrepl-log-messages t                  
        cider-repl-display-in-current-window t
        cider-repl-use-clojure-font-lock t    
        cider-prompt-save-file-on-load 'always-save
        cider-font-lock-dynamically '(macro core function var)
        nrepl-hide-special-buffers t            
        cider-overlays-use-font-lock t)         
  (cider-repl-toggle-pretty-printing))

;;====================================
;; Python
;;
;;====================================

(add-hook 'python-mode-hook #'eglot-ensure)
(add-hook 'python-mode-hook #'company-mode)

;;====================================
;; Javascript
;;
;;====================================

;; dit moet nog gefixed worden !!!!!

(use-package js-ts-mode
  :ensure nil
  :hook ((js-ts-mode . eglot-ensure)
         (js-ts-mode . company-mode)))


;;(dolist (hook-yassin '(js2-mode-hook tsx-mode-hook web-mode-hook))
  ;;(add-hook hook-yassin #'eglot-ensure))

;;====================================
;; Emmet mode (zie init.el)
;;
;;====================================

;;====================================
;; typescript mode
;;
;; https://merrick.luois.me/posts/typescript-in-emacs-29
;; https://www.reddit.com/r/emacs/comments/1apj22g/typescript_treesitter_mode_syntax_highlighting/  ==> gebruikt veel packages met elpaca
;;
;;====================================

(use-package treesit-auto
  :defines treesit-auto-langs
  :functions global-treesit-auto-mode
  :custom
  ( treesit-auto-install t)
  :config
  ;; Do not auto enable treesit for the following languages
  (setq treesit-auto-langs
        (cl-set-difference treesit-auto-langs '( rust
                                                 c
                                                 cpp
                                                 c-sharp)))
  (global-treesit-auto-mode))



(after! treesit
  (setq treesit-language-source-alist
        '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src" nil nil)
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src" nil nil))))


(use-package tsx-ts-mode
  :ensure nil
  :hook ((tsx-ts-mode . eglot-ensure)
         (tsx-ts-mode . company-mode)))

 (use-package typescript-ts-mode
  :ensure nil
  :mode (("\\.ts\\'" . typescript-ts-mode)
	("\\.tsx\\'" . tsx-ts-mode))
  :config
  (add-hook! '(typescript-ts-mode-hook tsx-ts-mode-hook) #'eglot-ensure)
  (add-hook! '(typescript-ts-mode-hook tsx-ts-mode-hook) #'company-mode)
  )

