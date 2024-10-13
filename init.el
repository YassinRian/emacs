;;; init.el --- Emacs initialization file -*- lexical-binding: t -*-
;;;
;;;
;;;   _    ___ _             _
;;;   _ ___ __ ___  __    _ ___
;;;   __   _     ___    __  ___
;;;       _           ___     _
;;;      _  _ __             _
;;;      ___   __            _
;;;            __           _
;;;             _      _   _
;;;            _      _    _
;;;               _  _    _
;;;           __  ___
;;;          _   _ _     _
;;;         _   _
;;;       _    _
;;;      _    _
;;;     _
;;;   __
;;;  
;;;
;;;
;;; Yassin's Emacs
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; This configuration depends on Emacs 29+.
;;;
;;; Tips:
;;;  * C-x C-e  ----  Evaluate expression
;;;  * C-h f    ----  Describe function
;;;  * C-h v    ----  Describe variable
;;;
;;;
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; Imports


;;; Package management  -*- lexical-binding: t -*-
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


;; ;; Note: package.el is disabled on early-init.el

;; load-config function

(defun load-config (config-file-name)
  "Load a configuration file from ./config/"
  (load (concat
         (concat (locate-user-emacs-file "config/")) config-file-name)))


;;;
;;; Essentials
(load-config "package-management.el")   	; straight.el, use-package
(load-config "optimization.el")         	; gcmh


;; load files from Config folder
(add-to-list 'load-path "~/.emacs.d/other-packages")





;;;
;;; UX improvements

(load-config "modeline.el")			; Themes
(load-config "saner-emacs.el")	        	; misc config for built in modes
(load-config "themes.el")			; Themes

;;; Important Packages
(load-config "boon.el")				; Boon
(load-config "various-packages.el")             ; Various packages
(load-config "coding.el")                       ; packages for various langs

;;; Keybindings
(load-config "keybindings.el")                  ; Keybindings


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; reload-init-file


(defun my-reload-emacs-configuration ()
  "Reload the Emacs configuration without using Elpaca cache."
  (interactive)
  (load-file user-init-file))

;; Bind the function to a key
(global-set-key (kbd "<f5>") 'my-reload-emacs-configuration)


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; Show init time and additional info

(defun display-startup-echo-area-message ()
  (message
   "Emacs loaded in %s with %d garbage collections."
   (format "%.2f seconds"
           (float-time
            (time-subtract
             (current-time)
             before-init-time)))
   gcs-done))

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; abbrev

;; Define a function to set up abbreviations in eshell
(defun setup-eshell-abbrevs ()
  "Set up abbreviations in eshell."
  (setq-local abbrev-table 'eshell-mode-abbrev-table))

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; activate globally

(dired-recent-mode 1)
(dimmer-mode t)
(recentf-mode 1)
(electric-pair-mode 1)
(abbrev-mode 1)

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; Hooks

;; Activate eglot when in python
(add-hook 'python-mode-hook 'eglot-ensure)

;; Add the function to eshell-mode-hook
(add-hook 'eshell-mode-hook 'setup-eshell-abbrevs)

;;; emmet mode

(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(setq emmet-move-cursor-between-quotes t) ;; default nil


(setq lsp-clients-angular-language-server-command
      '("node"
	"~/Coding/Node_Projs/angular_projs/node_modules/@angular/language-server"
	"--ngProbeLocations"
	"~/Coding/Node_Projs/angular_projs/node_modules"
	"--tsProbeLocations"
	"~/Coding/Node_Projs/angular_projs/node_modules"
	"--stdio"))

;; eglot sneller maken
(fset #'jsonrpc--log-event #'ignore)


;; undo-tree - declutter
;; Create a directory for undo-tree files if it doesn't exist
(defvar undo-tree-history-dir (expand-file-name "~/.emacs.d/undo/"))
(unless (file-exists-p undo-tree-history-dir)
  (make-directory undo-tree-history-dir t))

;; Set the undo-tree directory for history files
(setq undo-tree-history-directory-alist `((".*" . ,undo-tree-history-dir)))

;; Enable persistent undo history
(setq undo-tree-auto-save-history t)


(provide 'init)
;;; init.el ends here


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4ade6b630ba8cbab10703b27fd05bb43aaf8a3e5ba8c2dc1ea4a2de5f8d45882" "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "4e2e42e9306813763e2e62f115da71b485458a36e8b4c24e17a2168c45c9cf9d" "7ec8fd456c0c117c99e3a3b16aaf09ed3fb91879f6601b1ea0eeaee9c6def5d9" "0cd9a37b7d1a96f2e3fa8886343e3b5ea191687dbfa293ff818f953308dd9779" "ec8ff5e2c8a9eb38e49a9bea6297c2194bbe0c03982630d66db1570f5ae83d90" "baf6946d46390fd34b5f92b778be544c7dc1286bafb81c314dc7ee8e3f8875d3" "8cf8f08741be235cee6eefb96406b413ec613659eb8bf4ad401b1e5e0d6021ef" "98f6151faaa5f8f9eceb007fb038b2e8ea61d502283f8fd81fce27da3b6bced5" "a9eeab09d61fef94084a95f82557e147d9630fbbb82a837f971f83e66e21e5ad" "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" "2ca3da7d36b0d326f984530a07be54b272b5c313b1361989acf747d8b5616162" "80214de566132bf2c844b9dee3ec0599f65c5a1f2d6ff21a2c8309e6e70f9242" "821c37a78c8ddf7d0e70f0a7ca44d96255da54e613aa82ff861fe5942d3f1efc" default))
 '(package-selected-packages '(eglot-booster clojure-ts-mode clojure-mode))
 '(package-vc-selected-packages
   '((eglot-booster :vc-backend Git :url "https://github.com/jdtsmith/eglot-booster")))
 '(send-mail-function 'mailclient-send-it))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t :background "#1a1b26")))
 '(header-line ((t :box (:line-width 4 :color "grey20" :style nil))))
 '(header-line-highlight ((t :box (:color "black"))))
 '(keycast-key ((t)))
 '(line-number ((t :background "#1a1b26")))
 '(mode-line ((t :background "#1a1b26" :overline "black" :box (:line-width 4 :color "#1a1b26" :style nil))))
 '(mode-line-active ((t :background "#1a1b26" :overline "black" :box (:line-width 4 :color "#1a1b26" :style nil))))
 '(mode-line-highlight ((t :box (:color "black"))))
 '(mode-line-inactive ((t :background "#1a1b26" :overline "grey80" :box (:line-width 4 :color "#1a1b26" :style nil))))
 '(tab-bar-tab ((t :box (:line-width 4 :color "grey85" :style nil))))
 '(tab-bar-tab-inactive ((t :box (:line-width 4 :color "grey75" :style nil))))
 '(tab-line-tab ((t)))
 '(tab-line-tab-active ((t)))
 '(tab-line-tab-inactive ((t)))
 '(vertical-border ((t :background "#1a1b26" :foreground "#1a1b26")))
 '(window-divider ((t (:background "#1a1b26" :foreground "#1a1b26"))))
 '(window-divider-first-pixel ((t (:background "#1a1b26" :foreground "#1a1b26"))))
 '(window-divider-last-pixel ((t (:background "#1a1b26" :foreground "#1a1b26")))))
