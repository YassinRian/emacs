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

(use-package company
  :ensure t
  :diminish
  :custom
  (company-idle-delay .1)
  (company-minimum-prefix-length 1)
  (global-company-mode t))

;; is een beetje traag

(use-package company-flx
  :after company)

(use-package company-fuzzy
  :hook (company-mode . company-fuzzy-mode)
  :init
  (setq company-fuzzy-sorting-backend 'flx
        company-fuzzy-reset-selection t
        company-fuzzy-prefix-on-top nil
        company-fuzzy-trigger-symbols '("." "->" "<" "\"" "'" "@"))
  :custom
  (global-company-fuzzy-mode 1))



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

