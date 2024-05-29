(set-face-attribute 'default nil :font (font-spec :family "SourceCodePro" :size 18.0 :weight 'light))
(set-face-attribute 'fixed-pitch nil :font (font-spec :family "SourceCodePro" :size 18.0 :weight 'light))
(set-face-attribute 'variable-pitch nil :font (font-spec :family "SourceCodePro" :size 18.0 :weight 'light))
(set-fontset-font t 'unicode "SourceCodePro")


(defun f_iosevka ()
    (interactive)
    (set-face-attribute 'default nil :font (font-spec :family "Iosevka Comfy Motion" :size 16.0 :weight 'regular))
    (set-face-attribute 'fixed-pitch nil :font (font-spec :family "Iosevka Comfy Motion" :size 16.0 :weight 'regular))
    (set-face-attribute 'variable-pitch nil :font (font-spec :family "Iosevka Etoile" :size 18.0 :weight 'medium))
    (set-fontset-font t 'unicode "Ubuntu Mono")
)

;;(set-face-attribute 'default nil :font (font-spec :family "Iosevka Comfy Motion" :size 16.0 :weight 'regular))
;;(set-face-attribute 'fixed-pitch nil :font (font-spec :family "Iosevka Comfy Motion" :size 16.0 :weight 'regular))
;;(set-face-attribute 'variable-pitch nil :font (font-spec :family "Iosevka Etoile" :size 18.0 :weight 'medium))
;;(set-fontset-font t 'unicode "Ubuntu Mono")

(defun f_ubuntu ()
    (interactive)
    (set-face-attribute 'default nil :font (font-spec :family "Ubuntu Mono" :size 18.0))
    (set-face-attribute 'fixed-pitch nil :font (font-spec :family "Ubuntu Mono" :size 18.0))
    (set-face-attribute 'variable-pitch nil :font (font-spec :family "Ubuntu Mono" :size 18.0))
    (set-fontset-font t 'unicode "Ubuntu Mono")
  )

(defun f_dejavu ()
(interactive)
(set-face-attribute 'default nil :font (font-spec :family "DejaVu Sans Mono" :size 18.0 :weight 'light))
(set-face-attribute 'fixed-pitch nil :font (font-spec :family "DejaVu Sans Mono" :size 18.0 :weight 'light))
(set-face-attribute 'variable-pitch nil :font (font-spec :family "DejaVu Sans Mono" :size 18.0 :weight 'light))
(set-fontset-font t 'unicode "Ubuntu Mono"))

(defun f_sourcecode ()
(interactive)
(set-face-attribute 'default nil :font (font-spec :family "SourceCodePro" :size 18.0 :weight 'light))
(set-face-attribute 'fixed-pitch nil :font (font-spec :family "SourceCodePro" :size 18.0 :weight 'light))
(set-face-attribute 'variable-pitch nil :font (font-spec :family "SourceCodePro" :size 18.0 :weight 'light))
(set-fontset-font t 'unicode "SourceCodePro"))

(provide 'init-font)
