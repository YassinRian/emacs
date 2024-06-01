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
;; Corfu
;;
;;====================================
  
(use-package corfu
  ;; Optional customizations

  :bind (:map corfu-map
	  
	  ("<escape>". corfu-quit)
          ("<return>" . corfu-insert)
          ("M-d" . corfu-show-documentation)
          ("M-l" . 'corfu-show-location)
          ("TAB" . corfu-next)
          ([tab] . corfu-next)
          ("S-TAB" . corfu-previous)
          ([backtab] . corfu-previous))
  :custom
  
  (tab-always-indent 'complete)
  (completion-cycle-threshold nil)      ; Always show candidates in menu
  (corfu-auto t)
  (corfu-quit-no-match t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.25)
  
  (corfu-count 14)
  (corfu-scroll-margin 8)
  (corfu-cycle nil)
  ;; (corfu-echo-documentation nil)        ; Already use corfu-doc
  (corfu-separator ?\s)                 ; Necessary for use with orderless
  (corfu-quit-no-match 'separator)
  (corfu-preview-current 'insert)       ; Preview current candidate?
  (corfu-preselect-first t)  
  
  ;; Enable Corfu only for certain modes.
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode)
	 (clojure-mode . corfu-mode)
	 (javascript-mode . corfu-mode)
	 (elisp-mode . corfu-mode))

  :init
  (global-corfu-mode))


(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-blend-background t)
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

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
;;====================================
;; Javascript
;;
;;====================================

;; dit werkt overigens alleen wanneer Emacs wordt gestart vanuit de terminal
(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'" . js2-mode))
  )
  
(add-hook 'js-mode-hook #'eglot-ensure)



(use-package typescript-mode
  :after tree-sitter
  :config
  ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")

  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

(fset #'jsonrpc--log-event #'ignore)
