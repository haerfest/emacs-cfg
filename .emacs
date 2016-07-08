;; -----------------------------------------------------------------------------
;;  general behaviour
;; -----------------------------------------------------------------------------

;; don't want to see the startup screen
(setq inhibit-startup-screen t)

;; don't want any bell sounds
(setq ring-bell-function 'ignore)

;; don't want any fancy GUI widgets
(when (> emacs-major-version 22)
  (scroll-bar-mode -1)
  (tool-bar-mode -1))

;; don't want backup files
(setq make-backup-files nil)

;; don't want auto-save files
(auto-save-mode -1)

;; save all files with Unix line endings
(setq-default buffer-file-coding-system 'utf-8-unix)

;; window movement
(global-set-key (kbd "C-x <left>")  'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <up>")    'windmove-up)
(global-set-key (kbd "C-x <down>")  'windmove-down)

;; show the column number
(column-number-mode t)

;; show matching parenthesis
(show-paren-mode t)

;; use two spaces for tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; enable on-the-fly indentation
(if (>= emacs-major-version 24)
    (electric-indent-mode t)
  (global-set-key "\r" 'newline-and-indent))

;; if two dired windows are open, suggest copy to the other window
(setq dired-dwim-target t)

;; accept y/n for yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; undo when pressing ^z, suspend with the super key
(global-set-key [(control z)] 'undo)
(global-set-key [(super control z)] 'suspend-frame)

;; skip .svn directories when doing a grep-find
(setq grep-find-command
      (concat "find . -type f '!' -wholename '*/.svn/*' -print0 | "
              "xargs -0 grep -nH -e "))

;; treat all themes as safe
(setq custom-safe-themes t)

;; figure out which OS we're running on
(defvar on-mac     (eq system-type 'darwin)     "t if OS is Mac OS X")
(defvar on-windows (eq system-type 'windows-nt) "t if OS is Windows")
(defvar on-linux   (eq system-type 'gnu/linux)  "t if OS is Linux")

;; -----------------------------------------------------------------------------
;;  Mac OS X
;; -----------------------------------------------------------------------------
(when on-mac
  ;; use this font
  (set-face-attribute 'default nil
                      :family "Source Code Pro"
                      :weight 'normal
                      :height 140)

  ;; use the Command key as the Meta key
  (setq mac-option-modifier  'super)
  (setq mac-command-modifier 'meta))

;; -----------------------------------------------------------------------------
;;  Linux
;; -----------------------------------------------------------------------------
(when (and on-linux (display-graphic-p))
  ;; use this font
  (set-face-attribute 'default nil
                      :family "Anonymous Pro"
                      :weight 'extra-light
                      :height 120)

  ;; allow copy & paste between Emacs and X
  (setq x-select-enable-clipboard t)
  (setq interprogram-paste-function 'x-cut-buffer-or-selection-value))

;; -----------------------------------------------------------------------------
;;  Windows
;; -----------------------------------------------------------------------------
(when on-windows
  ;; use this font
  (set-face-attribute 'default nil
                      :family "Consolas"
                      :height 100)

  ;; open links with Windows' default browser
  (setq browse-url-browser-function 'browse-url-default-windows-browser))

;; -----------------------------------------------------------------------------
;;  handy functions
;; -----------------------------------------------------------------------------

(defun erase-interactive-buffer ()
  "Erases an interactive buffer (shell, REPL) but leaves the prompt alone."
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
  "Kill up to the ARG'th occurence of CHAR, and leave CHAR. If
you are deleteing forward, the CHAR is replaced and the point is
put before CHAR"
  (insert char)
  (if (< 0 arg) (forward-char -1)))

(defun euro (&optional arg)
  "Inserts a euro symbol."
  (interactive "p")
  (kmacro-exec-ring-item
   (quote ([24 56 return 35 120 50 48 65 67 return] 0 "%d")) arg))

;; -----------------------------------------------------------------------------
;;  ido                                                                built-in
;; -----------------------------------------------------------------------------

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;; -----------------------------------------------------------------------------
;;  c-mode                                                             built-in
;; -----------------------------------------------------------------------------

(setq-default c-default-style "linux"
              c-basic-offset 2)
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-offset 'case-label '+)))

;; -----------------------------------------------------------------------------
;;  shell mode                                                         built in
;; -----------------------------------------------------------------------------

;; press C-c M-o (as in Slime) in a shell to clear the buffer
(add-hook 'shell-mode-hook
          (lambda ()
            (local-set-key "\C-c\M-o" #'erase-interactive-buffer)))

;; -----------------------------------------------------------------------------
;;  ispell                                                             built-in
;; -----------------------------------------------------------------------------

(when on-mac
  (setq ispell-program-name "/opt/local/bin/aspell"))

;; -----------------------------------------------------------------------------
;;  packages
;; -----------------------------------------------------------------------------

;; credits to http://www.aaronbedra.com/emacs.d/
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))

(package-initialize)

;; default packages to have installed
(defvar who/packages '(
                       company
                       exec-path-from-shell
                       evil
                       ido-vertical-mode
                       markdown-mode
                       multiple-cursors
                       solarized-theme
                       which-key
                       yasnippet

                       ;; for clojure development
                       better-defaults
                       cider
                       clojure-mode
                       clojure-mode-extra-font-locking
                       clojure-snippets
                       projectile
                       rainbow-delimiters

                       ;; for python development
                       elpy

                       ;; for php development, yikes!
                       php-mode
                       geben

                       ;; for common lisp development
                       slime
                       slime-company

                       ;; for f# development
                       fsharp-mode
                       ))

;; define the filter function if not there
(unless (fboundp 'filter)
  (defun filter (pred-p items)
    (let (filtered)
      (dolist (item items)
        (when (funcall pred-p item)
          (setq filtered (cons item filtered))))
      (reverse filtered))))

;; returns which packages are missing
(defun who/missing-packages ()
  "Returns a list of missing packages."
  (filter (lambda (pkg)
            (not (package-installed-p pkg)))
          who/packages))

;; make sure all packages are installed
(defun who/install-missing-packages ()
  "Installs missing packages."
  (let* ((packages (who/missing-packages))
         (missing  (length packages))
         (prompt   (format "%d missing packages: %s. Install?"
                           missing packages)))
    ;; if any packages are missing, ask the user whether to install
    (when (and packages
               (y-or-n-p-with-timeout prompt 30 nil))
      ;; yes, go ahead
      (package-refresh-contents)
      (setq install-all nil
            install-none nil)
      ;; loop over each package and install if desired
      (while (and packages (not install-none))
        (setq pkg (car packages)
              packages (cdr packages))
        ;; install when 'install-all is set, or the user confirms
        (when (or install-all
                  (let ((prompt (format "Install package %s? (y, n, N, or a) "
                                        pkg)))
                    (pcase (read-char-choice prompt '(?y ?n ?N ?a))
                      (?y t)                           ; => t
                      (?a (setq install-all t))        ; => t
                      (?N (not (setq install-none t))) ; => nil
                      (?n nil))))                      ; => nil
          (package-install pkg))))))

(who/install-missing-packages)

;; force loading of packages now, so we can use them from here on in .emacs
(setq package-enable-at-startup nil)
(package-initialize)

;; -----------------------------------------------------------------------------
;;  multiple-cursors                                                    package
;; -----------------------------------------------------------------------------

(when (package-installed-p 'multiple-cursors)
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->")         'mc/mark-next-like-this)
  (global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this))

;; -----------------------------------------------------------------------------
;;  exec-path-from-shell                                                package
;; -----------------------------------------------------------------------------

(when (and on-mac (package-installed-p 'exec-path-from-shell))
  (exec-path-from-shell-copy-env "PS1")
  (exec-path-from-shell-initialize))

;; -----------------------------------------------------------------------------
;;  company                                                             package
;; -----------------------------------------------------------------------------

(when (package-installed-p 'company)
  (add-hook 'after-init-hook 'global-company-mode))

;; -----------------------------------------------------------------------------
;;  elpy                                                                package
;; -----------------------------------------------------------------------------

(when (package-installed-p 'elpy)

  ;; press C-c M-o (as in Slime) in a shell to clear the buffer
  (add-hook 'inferior-python-mode-hook
            (lambda ()
              (local-set-key "\C-c\M-o" #'erase-interactive-buffer)))

  ;; set encoding of the Python shell to UTF-8
  (setenv "LC_CTYPE" "UTF-8")
  (setenv "LC_ALL" "en_US.UTF-8")
  (setenv "LANG" "en_US.UTF-8")

  (elpy-enable)
  (when on-mac
    (elpy-use-ipython)))

;; -----------------------------------------------------------------------------
;;  rainbow-delimiters                                                  package
;; -----------------------------------------------------------------------------

(when (and (package-installed-p 'rainbow-delimiters)
           (package-installed-p 'clojure-mode))
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'cider-mode-hook 'rainbow-delimiters-mode))

;; -----------------------------------------------------------------------------
;;  yasnippet                                                           package
;; -----------------------------------------------------------------------------

(when (package-installed-p 'yasnippet)
  (yas-global-mode 1))

;; -----------------------------------------------------------------------------
;;  which-key                                                           package
;; -----------------------------------------------------------------------------

(when (package-installed-p 'which-key)
  (which-key-mode))

;; -----------------------------------------------------------------------------
;;  slime                                                               package
;; -----------------------------------------------------------------------------

(when (package-installed-p 'slime)
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  (slime-setup '(slime-fancy)))

;; -----------------------------------------------------------------------------
;;  fsharp-mode                                                         package
;; -----------------------------------------------------------------------------

(when (package-installed-p 'fsharp-mode)
  (require 'fsharp-mode)
  (setq inferior-fsharp-program "/usr/local/bin/fsharpi --readline-")
  (setq fsharp-compiler "/usr/local/bin/fsharpc"))

;; -----------------------------------------------------------------------------
;;  evil                                                                package
;; -----------------------------------------------------------------------------

(when (package-installed-p 'evil)
  (require 'evil)
  (evil-mode 1)

  ;; change mode-line color by evil state
  (lexical-let ((default-color (cons (face-background 'mode-line)
                                      (face-foreground 'mode-line))))
     (add-hook 'post-command-hook
       (lambda ()
         (let ((color (cond ((minibufferp) default-color)
                            ((evil-insert-state-p) '("#e80000" . "#ffffff"))
                            ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
                            ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
                            (t default-color))))
           (set-face-background 'mode-line (car color))
           (set-face-foreground 'mode-line (cdr color)))))))

;; -----------------------------------------------------------------------------
;;  ido-vertical-mode                                                  built-in
;; -----------------------------------------------------------------------------

(when (package-installed-p 'ido-vertical-mode)
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys '(C-n-and-C-p-only)))

;; -----------------------------------------------------------------------------
;;  geben-mode                                                         built-in
;; -----------------------------------------------------------------------------

(when (and (package-installed-p 'geben)
           (package-installed-p 'evil))
  (add-hook 'geben-mode-hook 'evil-emacs-state))

