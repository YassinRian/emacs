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

;; (use-package vertico
;;   :custom
;;   (vertico-scroll-margin 2)
;;   :init
;;   (vertico-mode)
;;   :bind (:map vertico-map
;;               ("C-<backspace>" . vertico-directory-up))
;;   )

;;=========================

;; Init vertico for item list selection
(use-package vertico
  ;; Load extensions
  :ensure ( :files ( :defaults "extensions/*"))
  :functions vertico-mode
  :custom
  ( vertico-count 20)
  ( vertico-cycle t)
  ( vertico-sort-function #'vertico-sort-history-alpha)
  :config
  (defun mo-vertico-combined-sort (candidates)
    "Sort CANDIDATES using both display-sort-function and vertico-sort-function."
    (let ((candidates
           (let ((display-sort-func (vertico--metadata-get 'display-sort-function)))
             (if display-sort-func
                 (funcall display-sort-func candidates)
               candidates))))
      (if vertico-sort-function
          (funcall vertico-sort-function candidates)
        candidates)))

  (vertico-mode))


;; Init vertico-directory for directory navigation commands
(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind
  ( :map vertico-map
    ("C-<backspace>" . vertico-directory-up)
    ))

  ;; Init vertico-multiform for per command vertico configuration
(use-package vertico-multiform
  :after vertico
  :ensure nil
  :config
  (vertico-multiform-mode))

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

;; (use-package orderless
;;   :custom
;;   (completion-category-defaults nil)
;;   (completion-category-overrides '((file (styles . (partial-completion)))))
;;   (completion-styles '(orderless)))

;; ======================================
;; afkomstig van Modus_Operandi config
;; Init orderless for advanced (e.g. fuzzy) completion styles

(use-package orderless
  :demand t
  :functions orderless-escapable-split-on-space
  :config
  ;; Set matching style to regexp and literal
  :custom
  ( orderless-matching-styles '( orderless-regexp orderless-literal))
  ( orderless-component-separator #'orderless-escapable-split-on-space)
  ( orderless-style-dispatchers '( mo-orderless-exclude-dispatcher))
  ( completion-styles '( orderless basic))
  :config
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '( ( file ( styles basic partial-completion))))

  ;; Add exclude pattern style
  (defun mo-orderless-exclude-dispatcher (pattern _index _total)
    "Handle orderless exclude pattern."
    (cond
     ((equal "!" pattern)
      '( orderless-literal . ""))
     ((string-prefix-p "!" pattern)
      `( orderless-without-literal . ,(substring pattern 1))))))



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
(require 'burly)
(require 'persp-mode)
(require 'fzf)
(require 'sk-org)
(require 'eglot-java)
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
  :ensure t
  :config
  (setq consult-dir-default-command #'consult-dir-dired) ;; default was find-file, maar deze is beter
)
  
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

(use-package affe :defer t)

(use-package direnv
 :config
 (direnv-mode))

;; nodig voor de copy, move, past functie in dired
(use-package dired-ranger)
  
(use-package which-key
  :defer 0.2
  :delight
  :custom (which-key-idle-delay 0.5)
  :config (which-key-mode))
