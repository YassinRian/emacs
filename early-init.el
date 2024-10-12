;;; early-init.el --- Early Emacs initialization -*- lexical-binding: t -*-
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Set window color to black while loading to avoid FOUC
(push '(background-color . "#1a1b26") initial-frame-alist)

(setq byte-compile-warnings '(cl-functions))

(setq
 ;;
 ;; GC config
 gc-cons-threshold 200000000 ; Stop GC at startup. MUST be reset later to avoid freezes. gcmh will manage this
 read-process-output-max (* 16 1024 1024)     ; LSP-mode performance tweak (16mb)


 ;; Lockfiles, autosave and backups
 create-lockfiles nil                                                   ; Stop Emacs from creating .#foo.txt if editing foo.txt
 backup-directory-alist         `((".*" . ,temporary-file-directory))   ; Save backups to /tmp/
 auto-save-file-name-transforms `((".*"   ,temporary-file-directory t)) ; Save autosaves to /tmp/

 delete-old-versions t                  ; Automatically delete old backup files
 
 ;; Avoid auto resize of window while loading
 ;; (Significant performance boost)
 frame-inhibit-implied-resize t
 
 ;; Optimizations to jit lock
 ;; From https://tychoish.com/post/towards-faster-emacs-start-times/
 jit-lock-stealth-time nil
 jit-lock-defer-time   nil
 jit-lock-defer-time   0.05
 jit-lock-stealth-load 200

 ;;
 ;; Hide modeline and headerline while loading
 mode-line-format   nil
 header-line-format nil

 ;;
 ;; Don't load the startup screen
 inhibit-startup-screen t
 inhibit-startup-message t
 inhibit-startup-echo-area-message t

 ;;
 ;; Make the initial mode for *scratch* fundamental-mode instead of
 ;; lisp-interaction-mode, so it starts faster
 ;;initial-major-mode 'fundamental-mode
 
 ;;
 ;; Change default scratch message
 initial-scratch-message "
   _    ___ _             _
   _ ___ __ ___  __    _ ___
   __   _     ___    __  ___
       _           ___     _
      _  _ __             _
      ___   __            _
            __           _
             _      _   _
            _      _    _
               _  _    _
           __  ___
          _   _ _     _
         _   _
       _    _
      _    _
     _
   __
  
"

 ;;
 ;; Don't load `package.el' as we'll be using `elpaca'
 package-enable-at-startup  nil
 package--init-file-ensured nil
 package-quickstart         nil)
 
 ;;
;; Disable UI elements
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)
      
;; If Emacs build has native comp enabled...
(when (featurep 'native-compile)
  ;; shut up with the "*Warning* i'm compiling and the variable is unused/
  ;; line is too long" whatevers
  ;; Annoying errors got me acting unwise
  (setq native-comp-async-report-warnings-errors nil))
  
 
 (setenv "LSP_USE_PLISTS" "true") ;; in early-init.el
  
  
;;; ======================= Functions to be loaded before packages ======================

(defun nox-unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun nox-enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (listp exp) exp (list exp)))

(defun nox-resolve-hook-forms (hooks)
  (declare (pure t) (side-effect-free t))
  (cl-loop with quoted-p = (eq (car-safe hooks) 'quote)
           for hook in (nox-enlist (nox-unquote hooks))
           if (eq (car-safe hook) 'quote)
           collect (cadr hook)
           else if quoted-p
           collect hook
           else collect (intern (format "%s-hook" (symbol-name hook)))))

(defmacro add-hook! (&rest args)
  "A convenience macro for `add-hook'. Takes, in order:

   1. Optional properties :local and/or :append, which will make the hook
      buffer-local or append to the list of hooks (respectively),
   2. The hooks: either an unquoted major mode, an unquoted list of major-modes,
      a quoted hook variable or a quoted list of hook variables. If unquoted, the
      hooks will be resolved by appending -hook to each symbol.
   3. A function, list of functions, or body forms to be wrapped in a lambda.

Examples:
    (add-hook! 'some-mode-hook 'enable-something)   (same as `add-hook')
    (add-hook! some-mode '(enable-something and-another))
    (add-hook! '(one-mode-hook second-mode-hook) 'enable-something)
    (add-hook! (one-mode second-mode) 'enable-something)
    (add-hook! :append (one-mode second-mode) 'enable-something)
    (add-hook! :local (one-mode second-mode) 'enable-something)
    (add-hook! (one-mode second-mode) (setq v 5) (setq a 2))
    (add-hook! :append :local (one-mode second-mode) (setq v 5) (setq a 2))

Body forms can access the hook's arguments through the let-bound variable `args'."
  (declare (indent defun) (debug t))
  (let ((hook-fn 'add-hook)
        append-p local-p)
    (while (keywordp (car args))
      (pcase (pop args)
        (:append (setq append-p t))
        (:local  (setq local-p t))
        (:remove (setq hook-fn 'remove-hook))))
    (let ((hooks (nox-resolve-hook-forms (pop args)))
          (funcs (let ((arg (car args)))
                   (if (memq (car-safe arg) '(quote function))
                       (if (cdr-safe (cadr arg))
                           (cadr arg)
                         (list (cadr arg)))
                     (list args))))
          forms)
      (dolist (fn funcs)
        (setq fn (if (symbolp fn)
                     `(function ,fn)
                   `(lambda (&rest _) ,@args)))
        (dolist (hook hooks)
          (push (if (eq hook-fn 'remove-hook)
                    `(remove-hook ',hook ,fn ,local-p)
                  `(add-hook ',hook ,fn ,append-p ,local-p))
                forms)))
      `(progn ,@(if append-p (nreverse forms) forms)))))

(defmacro remove-hook! (&rest args)
  "Convenience macro for `remove-hook'. Takes the same arguments as `add-hook!'."
  (declare (indent defun) (debug t))
  `(add-hook! :remove ,@args))

(defmacro setq-hook! (hooks &rest rest)
  "Convenience macro for setting buffer-local variables in a hook.

  (setq-hook! 'markdown-mode-hook
    line-spacing 2
    fill-column 80)"
  (declare (indent 1))
  (unless (= 0 (% (length rest) 2))
    (signal 'wrong-number-of-arguments (length rest)))
  `(add-hook! ,hooks
	      ,@(let (forms)
		  (while rest
		    (let ((var (pop rest))
			  (val (pop rest)))
		      (push `(setq-local ,var ,val) forms)))
		  (nreverse forms))))

(defmacro add-transient-hook! (hook-or-function &rest args)
  "Attaches a self-removing function to HOOK-OR-FUNCTION.

HOOK-OR-FUNCTION can be a quoted hook or a sharp-quoted function (which will be
advised).

ARGS can be a function, list of functions, or body forms to be wrapped in a lambda.
When it is a function or a list of functions, they will be called with the hooks args."
  (declare (indent 1))
  (let ((append (if (eq (car args) :after) (pop args)))
        ;; NOTE(nox):
        ;; If args is a function or list of functions, funcs will be a list of functions
        ;; If args is a list of forms, funcs will be a list containing only the list of forms
        (funcs (let ((arg (car args)))
                 (if (memq (car-safe arg) '(quote function))
                     (if (cdr-safe (cadr arg))
                         (cadr arg)
                       (list (cadr arg)))
                   (list args))))
        (func-name (gensym "nox|transient-hook-")))
    `(progn
       (fset ',func-name
             (lambda (&rest call-args)
               ,@(cl-loop for fn in funcs
                          collect (if (symbolp fn)
                                      `(apply #',fn call-args)
                                    `(progn ,@args)))
               (cond ((functionp ,hook-or-function) (advice-remove ,hook-or-function #',func-name))
                     ((symbolp ,hook-or-function)   (remove-hook ,hook-or-function #',func-name)))
               (unintern ',func-name nil)))
       (cond ((functionp ,hook-or-function)
              (advice-add ,hook-or-function ,(if append :after :before) #',func-name))
             ((symbolp ,hook-or-function)
              (put ',func-name 'permanent-local-hook t)
              (add-hook ,hook-or-function #',func-name ,append))))))

(defmacro after! (targets &rest body)
  "A smart wrapper around `with-eval-after-load'. Supresses warnings during
compilation. This will no-op on features that have been disabled by the user."
  (declare (indent defun) (debug t))
  (list (if (or (not (bound-and-true-p byte-compile-current-file))
                (dolist (next (nox-enlist targets))
                  (unless (keywordp next)
                    (if (symbolp next)
                        (require next nil :no-error)
                      (load next :no-message :no-error)))))
            #'progn
          #'with-no-warnings)
        (if (symbolp targets)
            `(with-eval-after-load ',targets ,@body)
          (pcase (car-safe targets)
            ((or :or :any)
             (macroexp-progn
              (cl-loop for next in (cdr targets)
                       collect `(after! ,next ,@body))))
            ((or :and :all)
             (dolist (next (cdr targets))
               (setq body `((after! ,next ,@body))))
             (car body))
            (_ `(after! (:and ,@targets) ,@body))))))
  

(provide 'early-init)
;;; early-init.el ends here
