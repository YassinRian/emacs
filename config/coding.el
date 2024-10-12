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
  ( corfu-auto-prefix 2)
  (corfu-popupinfo-delay '(0.5 . 0.2))
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


(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error) ; optional but recommended error navigation
              ("M-p" . flycheck-previous-error)))

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


;;====================================
;; treesitter
;;
;;====================================
(use-package treesit-auto
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'"  . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . typescript-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.json\\'" .  json-ts-mode)
         ("\\.Dockerfile\\'" . dockerfile-ts-mode)
         ("\\.prisma\\'" . prisma-ts-mode)
         ;; More modes defined here...
         )
  :preface
  (defun os/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (bash "https://github.com/tree-sitter/tree-sitter-bash")
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
               (markdown "https://github.com/ikatyang/tree-sitter-markdown")
               (make "https://github.com/alemuller/tree-sitter-make")
               (elisp "https://github.com/Wilfred/tree-sitter-elisp")
               (cmake "https://github.com/uyha/tree-sitter-cmake")
               (c "https://github.com/tree-sitter/tree-sitter-c")
               (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
               (toml "https://github.com/tree-sitter/tree-sitter-toml")
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
               (prisma "https://github.com/victorhqc/tree-sitter-prisma")))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;;
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js-mode . typescript-ts-mode)
             (js2-mode . typescript-ts-mode)
             (c-mode . c-ts-mode)
             (c++-mode . c++-ts-mode)
             (c-or-c++-mode . c-or-c++-ts-mode)
             (bash-mode . bash-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
             (sh-mode . bash-ts-mode)
             (sh-base-mode . bash-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (os/setup-install-grammars)
  
  
  (use-package combobulate
    :preface
    ;; You can customize Combobulate's key prefix here.
    ;; Note that you may have to restart Emacs for this to take effect!
    (setq combobulate-key-prefix "C-c o")

    ;; Optional, but recommended.
    ;;
    ;; You can manually enable Combobulate with `M-x
    ;; combobulate-mode'.
    :hook
    ((python-ts-mode . combobulate-mode)
     (js-ts-mode . combobulate-mode)
     (go-mode . go-ts-mode)
     (html-ts-mode . combobulate-mode)
     (css-ts-mode . combobulate-mode)
     (yaml-ts-mode . combobulate-mode)
     (typescript-ts-mode . combobulate-mode)
     (json-ts-mode . combobulate-mode)
     (tsx-ts-mode . combobulate-mode))
    ;; Amend this to the directory where you keep Combobulate's source
    ;; code.
    :load-path ("~/workspace/combobulate"))
  
  
  
  
  
  
  
  )



;;====================================
;; LSP mode
;;
;;====================================

(use-package lsp-mode
  :hook
  ((lsp-mode . lsp-diagnostics-mode)
   ((java-mode         ; eclipse-jdtls
     ;;js-mode         ; ts-ls (tsserver wrapper)
     ;;js-jsx-mode     ; ts-ls (tsserver wrapper)
     ;;typescript-mode ; ts-ls (tsserver wrapper)
     tsx-ts-mode
     typescript-ts-mode
     js-ts-mode
     python-mode     ; pyright
     web-mode        ; ts-ls/HTML/CSS
     ) . lsp-deferred))
  :commands lsp
  :custom
  (lsp-keymap-prefix "C-c l")           ; Prefix for LSP actions
  ;; (lsp-completion-provider :none)       ; Using Corfu as the provider
  ;; (lsp-diagnostics-provider :flycheck)
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))
  (lsp-log-io nil)                      ; IMPORTANT! Use only for debugging! Drastically affects performance
  (lsp-keep-workspace-alive nil)        ; Close LSP server if all project buffers are closed
  ;; ;; core
  (lsp-enable-xref t)                   ; Use xref to find references
  :config
  (setq lsp-diagnostics-provider :flycheck)
  (setq lsp-completion-provider :none)       ; Using Corfu as the provider
  (setq lsp-auto-guess-root t)
  (setq lsp-log-io nil)
  (setq lsp-restart 'auto-restart)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-eldoc-hook nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-semantic-tokens-enable nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-imenu nil)
  (setq lsp-enable-snippet nil)
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  (setq lsp-idle-delay 0.5)

  :init
  (setq lsp-use-plists t)

  :preface
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)

       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  :init
  (setq lsp-use-plists t)
  ;; Initiate https://github.com/blahgeek/emacs-lsp-booster for performance
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
  )

(use-package lsp-completion
  :no-require
  :ensure (:host github :repo "emacs-lsp/lsp-mode")
  :hook ((lsp-mode . lsp-completion-mode)))

(use-package lsp-eslint
  :demand t
  :ensure (:host github :repo "emacs-lsp/lsp-mode")
  :after lsp-mode)

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-border (face-foreground 'default))
  (setq lsp-ui-sideline-show-code-action t)
  (setq lsp-ui-sideline-delay 0.05))

(use-package lsp-pyright
  :hook (python-mode . (lambda () (require 'lsp-pyright)))
  :init (when (executable-find "python")
	  (setq lsp-pyright-python-executable-cmd "python")))

(use-package lsp-java
  :after lsp)


(defun my-completion-category()
  (setq-local completion-category-defaults
              (assoc-delete-all 'lsp-capf completion-category-defaults)))

(add-hook 'lsp-completion-mode-hook #'my-completion-category)

;;; APHELEIA
;; auto-format different source code files extremely intelligently
;; https://github.com/radian-software/apheleia

(use-package apheleia
  :ensure apheleia
  :diminish ""
  :defines
  apheleia-formatters
  apheleia-mode-alist
  :functions
  apheleia-global-mode
  :config
  (setf (alist-get 'prettier-json apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))
  (apheleia-global-mode +1))
