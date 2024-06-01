;;; Various-packages                                         -*- lexical-binding: t -*-
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;====================================
;; Hydra
;;
;;====================================

(use-package hydra :ensure t)
;;====================================
;; Ace-window
;; Jump among windows
;;
;;====================================

(use-package ace-window
  :config (setq
           aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
           aw-scope 'frame
           aw-background t)
  )
;;====================================
;; Vertico
;;
;; Better completion UI for the minibuffer (i.e for M-x)
;;====================================
(use-package vertico
  :custom
  (vertico-scroll-margin 2)
  :init
  (vertico-mode)
  :bind (:map vertico-map
              ("C-<backspace>" . vertico-directory-up))
  )
  
;;====================================
;; Which-key
;;
;;====================================

(use-package which-key
  :defer 0.2
  :delight
  :custom (which-key-idle-delay 0.3)
  :config (which-key-mode))

;;====================================
;; Helpful
;;
;;====================================

(use-package helpful
  :commands (helpful-at-point
             helpful-callable
             helpful-command
             helpful-function
             helpful-key
             helpful-macro
             helpful-variable)
  :bind
  ([remap display-local-help] . helpful-at-point)
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command))


;;====================================
;; savehist
;;
;;====================================
(use-package savehist
  :ensure nil
  :custom
  (history-delete-duplicates t)
  (history-length 25)
  :config (savehist-mode))
;;====================================
;; Marginalia
;;
;;====================================
(use-package marginalia
  :after vertico
  :init (marginalia-mode)
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))
;;====================================
;; Orderless
;;
;;====================================
(use-package orderless
  :custom
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion)))))
  (completion-styles '(orderless)))
;;====================================
;; Editorconfig
;;
;;====================================
(use-package editorconfig
  :defer 0.3
  :config (editorconfig-mode))
;;====================================
;; Avy
;;
;;====================================
(use-package avy
  :init
  (setq avy-keys-alist
		`((avy-goto-char-2			. (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))
		  (avy-goto-word-1			. (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))
		  (avy-goto-char-in-line	. (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))
		  (avy-goto-char-2-above	. (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))
		  (avy-goto-char-2-below	. (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))
		  (avy-goto-char-timer  	. (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))
		  (avy-goto-line			. (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))))
  
   :custom
   (avy-timeout-seconds 0.3 "The default is too long."))
;;====================================
;; Crux
;;
;;====================================
(use-package crux
  :defer t)
;;====================================
;; Expand-region
;;
;;====================================
(use-package expand-region
  :defer t)
;;====================================
;; Other Packages
;; See other-packages folder
;;
;;====================================
(require 'rainbow-mode)
(require 'dired-recent)
(require 'drag-stuff)
(require 'find-file-in-project)
(require 'sedition)
(require 'phi-search)
(require 'phi-replace)
(require 'dimmer)
(require 'init-font)
(require 'sync-recentf)
(require 'visual-regexp)
(require 'visual-regexp-steroids)
(require 'zoxide)
(require 'emmet-mode)
;;====================================
;; Recent files
;;
;;====================================
(use-package recentf
  :ensure nil
  :init (recentf-mode)
  :custom
  (recentf-exclude (list "/scp:"
                         "/ssh:"
                         "/sudo:"
                         "/tmp/"
                         "~$"
                         "COMMIT_EDITMSG"))
  (recentf-max-menu-items 15)
  (recentf-max-saved-items 200)
  (recentf-save-file (expand-file-name "~/.emacs.d/recentf"))
  ;; Save recent files every 5 minutes to manage abnormal output.
  ;;:config (run-at-time nil (* 5 60) 'recentf-save-list)
  )
;;====================================
;; all-the-icons
;;
;;====================================  
(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))  
;;====================================
;; Consult
;;
;;====================================
(use-package consult
  :ensure t)
  
(use-package consult-dir
  :ensure t)
  


;; =============== Dired Ranger  ============================
;;
;; =================================================

(use-package dired-ranger)

;; =============== Good-Scroll  ============================
;;
;; =================================================
(use-package good-scroll
  :ensure t
  :init
  (good-scroll-mode 1))

;; =============== solaire-mode  ============================
;;
;; =================================================

(use-package solaire-mode
  :config
  (solaire-global-mode +1))

;; =============== tron theme  ============================
;;
;; =================================================

(use-package tron-legacy-theme
  :config
  (setq tron-legacy-theme-vivid-cursor t)
  ;;(load-theme 'tron-legacy)
  )
;; =============== Catpuccin theme  ============================
;;
;; =================================================

(use-package catppuccin-theme
  :config
  (setq catppuccin-height-title1 1.5)
  ;; (load-theme 'catppuccin t)
  )

;; =============== Multiple cursors  ============================
;;
;; =================================================
(use-package multiple-cursors)

;; =============== Iedit  ============================
;;
;; =================================================

(use-package iedit)

;; =============== Focus  ============================
;;
;; =================================================

(use-package focus) ;; even checken hoe dit werkt

;; =============== smartscan  ============================
;;
;; =================================================

(use-package smartscan
  :config
  (smartscan-mode 1))

;; =============== Undo tree  ============================
;;
;; =================================================

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(use-package vundo)

;; =============== Good-Scroll  ============================
;;
;; =================================================

(use-package yasnippet
  :diminish yas-minor-mode
  :custom (yas-snippet-dirs '("~/.emacs.d/snippets"))
  :hook (after-init . yas-global-mode))
;; =============== Good-Scroll  ============================
;;
;; =================================================

(use-package google-translate
  :bind
  ("M-o t" . google-translate-at-point)
  ("M-o T" . google-translate-at-point-reverse)
  :custom
  (google-translate-default-source-language "en")
  (google-translate-default-target-language "nl"))

;; =============== Good-Scroll  ============================
;;
;; =================================================
(use-package google-this)

;; =============== Good-Scroll  ============================
;;
;; =================================================

(use-package beacon
  :custom (beacon-color "#f1fa8c")
  :hook (after-init . beacon-mode))

;; =============== Good-Scroll  ============================
;;
;; =================================================

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))
;; =============== Good-Scroll  ============================
;;
;; =================================================

(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))
;; =============== Good-Scroll  ============================
;;
;; =================================================

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t doom-themes-enable-italic t)
    (doom-themes-visual-bell-config)
    (doom-themes-org-config))

(use-package scratch)

