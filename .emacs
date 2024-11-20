;; don't want to see the startup screen
(setq inhibit-startup-screen t)

;; accept y/n for yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; ask confirmation before quitting
(setq confirm-kill-emacs 'y-or-n-p)

;; don't want these widgets
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (and (fboundp 'menu-bar-mode) (not (eq system-type 'darwin)))
  (menu-bar-mode -1))

;; don't want backup files
(setq make-backup-files nil)

;; don't want auto-save files
(auto-save-mode -1)

;; show the column number
(column-number-mode t)

;; truncate lines (don't wrap)
(set-default 'truncate-lines t)

;; show matching parenthesis
(show-paren-mode t)

;; highlight the current line
(global-hl-line-mode)

;; use two spaces for tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; treat all themes as safe
(setq custom-safe-themes t)

;; disable other themes before loading new one
(defadvice load-theme (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

;; store customizations in a separate file
(setq custom-file "~/.emacs-custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; prevent non-ascii characters from slowing emacs down
(setq inhibit-compacting-font-caches t)

;; the external Common Lisp implementation to use
(setq inferior-lisp-program "sbcl")

;; swallow shells echoing of entered commands
(add-hook 'comint-mode-hook (lambda ()
                              (setq comint-proces-echoes t)))

;; undo when pressing ^z, suspend with the super key
(global-set-key (kbd "C-z")   'undo)
(global-set-key (kbd "C-s-z") 'suspend-frame)

;; window movement
(global-set-key (kbd "C-x <left>")  'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <up>")    'windmove-up)
(global-set-key (kbd "C-x <down>")  'windmove-down)

;; Mac-specifics
(when (eq system-type 'darwin)

  ;; use the Mac's option/command keys for super/meta
  (setq mac-option-modifier  'super)
  (setq mac-command-modifier 'meta)

  ;; command-h should hide Emacs
  (global-set-key (kbd "M-h") 'ns-do-hide-emacs))

;; Linux-specifics
(when (eq system-type 'gnu/linux)

  ;; allow copy & paste between Emacs and X
  (setq x-select-enable-clipboard t)
  (setq interprogram-paste-function 'x-cut-buffer-or-selection-value))

;; Windows-specifics
(when (eq system-type 'windows-nt)

  ;; open links in the default browser
  (setq browse-url-browser-function 'browse-url-default-windows-browser))

;; the packages to install via package-install-selected-pages
(setq package-selected-packages
      '(ido-vertical-mode
	magit
	markdown-mode
	multiple-cursors
	which-key))

;; ido
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;; ido-vertical-mode
(when (package-installed-p 'ido-vertical-mode)
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys '(C-n-and-C-p-only)))

;; c-mode
(add-hook 'c-mode-common-hook
          (lambda ()
            ;; primarily use the linux style
            (c-set-style "linux")

            ;; but insert a small number of spaces instead of a tab
            (setq indent-tabs-mode nil)
            (setq c-basic-offset tab-width)

            ;; indent switch-case statements
            (c-set-offset 'case-label '+)))

;; markdown-mode
(when (package-installed-p 'markdown-mode)
  (setq markdown-fontify-code-blocks-natively t)
  (custom-set-faces '(markdown-code-face ((t nil)))))

;; multiple-cursors
(when (package-installed-p 'multiple-cursors)
  (global-set-key (kbd "M-<down>")    'mc/mark-next-like-this)
  (global-set-key (kbd "M-<up>")      'mc/mark-previous-like-this)
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this))

;; which-key
(when (package-installed-p 'which-key)
  ;; C-h C-h should page
  (define-key help-map (kbd "C-h") 'which-key-C-h-dispatch)
  (which-key-mode))
