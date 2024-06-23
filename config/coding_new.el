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

;; Init vertico for item list selection
(use-package vertico
  ;; Load extensions
  :straight ( :files ( :defaults "extensions/*"))
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

;; Init vertico-repeat for repeating the last minibuffer command
(use-package vertico-repeat
  :after vertico
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "z"
    "v" #'vertico-repeat-select)
  :hook
  ( minibuffer-setup . vertico-repeat-save))

;; Init vertico-directory for directory navigation commands
(use-package vertico-directory
  :after vertico
  :straight nil
  :general
  ( :keymaps 'vertico-map
    "M-<backspace>" #'vertico-directory-up))

;; Init vertico-quick for quick result selection
(use-package vertico-quick
  :after vertico
  :straight nil
  :general
  ( :keymaps 'vertico-map
    "M-k" #'vertico-quick-jump
    "M-j" #'vertico-quick-exit))

;; Init vertico-multiform for per command vertico configuration
(use-package vertico-multiform
  :after vertico
  :straight nil
  :config
  (vertico-multiform-mode))

;; Init vertico-buffer for viewing vertico results in a separate buffer
(use-package vertico-buffer
  :after vertico
  :straight nil
  :general
  ( :keymaps 'override
    "C-|" #'mo-vertico-buffer-next-command)
  :config
  (defun mo-vertico-buffer-next-command ()
    "Run next vertico command in a separate buffer."
    (interactive)
    (defun mo--vertico-buffer-disable-next-command ()
      "Called by a hook to disable vertico-buffer."
      (remove-hook 'minibuffer-setup-hook #'mo--vertico-buffer-disable-next-command)
      (vertico-buffer-mode -1))
    (add-hook 'minibuffer-setup-hook #'mo--vertico-buffer-disable-next-command 100)
    (vertico-buffer-mode)
    (message "Display the next vertico command in a dedicated buffer...")))

;; Init recursion-indicator for indicating minibuffer recursions
(use-package recursion-indicator
  :functions recursion-indicator-mode
  :config
  (recursion-indicator-mode))

;; Init corfu for auto completion
(use-package corfu
  ;; Load extensions
  :straight ( :files ( :defaults "extensions/*"))
  :demand t
  :functions
  ( corfu-mode
    global-corfu-mode)
  :general
  ( :keymaps 'corfu-map
    "M-SPC" #'corfu-insert-separator
    ;; We want TAB to complete
    "TAB" #'corfu-complete
    [tab] #'corfu-complete
    ;; We don't want RET to complete
    "RET" nil
    ;; We don't want next/previous line to change selection
    "<remap> <next-line>" nil
    "<remap> <previous-line>" nil)
  :custom
  ;; Enable auto completion
  ( corfu-auto t)
  ;; Keep popup only if there is a match or the separator was inserted
  ( corfu-quit-no-match 'separator)
  ;; Quit on exact match
  ( corfu-on-exact-match 'quit)
  ;; Set auto completion to be more responsive
  ( corfu-auto-delay 0)
  ( corfu-auto-prefix 0)
  ;; Sort candidates by calling corfu-sort-function on top of display-sort-function
  ( corfu-sort-override-function #'mo-corfu-combined-sort)
  :hook
  ;; Conditionally enable Corfu in the minibuffer
  ( minibuffer-setup . corfu-enable-in-minibuffer)
  ;; Disable auto mode in eshell
  ( eshell-mode . mo-corfu-enable-no-auto)
  ;; Disable auto mode in shell
  ( shell-mode . mo-corfu-enable-no-auto)
  ;; Close popup when exiting evil insert state
  ( evil-insert-state-exit . corfu-quit)
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
  (advice-add #'corfu-insert :after #'corfu-send-shell)
  (global-corfu-mode))

;; Init corfu-history for auto-completion sorting based on history
(use-package corfu-history
  :after ( corfu savehist)
  :straight nil
  :config
  (corfu-history-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history))

;; Init corfu-popupinfo for extra info on auto-completion candidates
(use-package corfu-popupinfo
  :after corfu
  :straight nil
  :config
  ;; Set a short popup delay
  (setq corfu-popupinfo-delay '( 0.5 . 0.5))
  (corfu-popupinfo-mode 1))

;; Init corfu-echo for auto-completion candidate doc in the echo area
(use-package corfu-echo
  :after corfu
  :straight nil
  :config
  (corfu-echo-mode 1))

;; Init corfu-quick for selecting auto-completion candidates using quick keys
(use-package corfu-quick
  :after corfu
  :straight nil
  :general
  ( :keymaps 'corfu-map
    "M-k" #'corfu-quick-jump
    "M-j" #'corfu-quick-complete))

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
  (add-hook 'completion-at-point-functions #'cape-file)

  (defun mo-cape-disable-file-comp-evil-search ()
    "Disable cape file completion on evil search."
    (when (or (eq current-minibuffer-command 'evil-ex-search-forward)
              (eq current-minibuffer-command 'evil-ex-search-backward))
      (make-local-variable 'completion-at-point-functions)
      (remove-hook 'completion-at-point-functions #'cape-file t)))

  :hook
  (minibuffer-setup . mo-cape-disable-file-comp-evil-search))

;; Init dabbrev for the automatic completion of dynamic abbreviations
(use-package dabbrev
  :straight nil
  :config
  (setq dabbrev-ignored-buffer-regexps '( "^\\*.+::stderr\\*$"))
  ;; Do not scan buffers that are too big
  (setq dabbrev-check-all-buffers nil)
  (defvar mo-dabbrev-max-file-size 1000000)
  (setq dabbrev-friend-buffer-function
        (lambda (buffer)
          (< (buffer-size buffer) mo-dabbrev-max-file-size))))
