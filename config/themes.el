;;; saner-emacs.el                                   -*- lexical-binding: t -*-
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;
;; Themes

;; ef-themes
(use-package ef-themes :ensure t)

;; Spacious-padding
(use-package spacious-padding
:ensure t
:config
 ;; These is the default value, but I keep it here for visiibility.
	(setq spacious-padding-widths
      		'( 
      		:internal-border-width 30
         	:header-line-width 4
         	:mode-line-width 4
         	:tab-width 4
         	:right-divider-width 15
         	:scroll-bar-width 1
         	:left-fringe-width 10
         	:right-fringe-width 10
         	))
         	
;; Read the doc string of `spacious-padding-subtle-mode-line' as it
;; is very flexible and provides several examples.
(setq spacious-padding-subtle-mode-line
        `( :mode-line-active 'default
           :mode-line-inactive 'vertical-border))
(spacious-padding-mode 1)

) 

(after! ef-themes (load-theme 'ef-bio :no-confirm) )



