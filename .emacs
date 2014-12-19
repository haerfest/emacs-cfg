;; -----------------------------------------------------------------------------
;;  general behaviour
;; -----------------------------------------------------------------------------

;; don't want to see the startup screen
(setq inhibit-startup-screen 1)

;; don't want any fancy GUI widgets
(scroll-bar-mode -1)
(tool-bar-mode -1)

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

;; do not truncate long lines
(setq-default truncate-lines t)

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
                      :family "Source Code Pro for Powerline"
                      :weight 'extra-light
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
                      :family "Source Code Pro for Powerline"
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
                      :font   "Sauce Code Powerline"
                      :weight 'extra-light
                      :height  110))

;; -----------------------------------------------------------------------------
;;  handy functions
;; -----------------------------------------------------------------------------

(defun erase-interactive-buffer ()
  "Erases an interactive buffer (shell, REPL) but leaves the prompt alone."
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

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
;;  MELPA packages
;; -----------------------------------------------------------------------------

;; credits to http://www.aaronbedra.com/emacs.d/
(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; default packages to have installed
(defvar who/packages '(ac-slime
                       auto-complete
                       erlang
                       seti-theme))

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
  (filter (lambda (pkg)
            (not (package-installed-p pkg)))
          who/packages))

;; make sure all packages are installed
(defun who/install-missing-packages ()
  (let* ((packages (who/missing-packages))
         (missing  (length packages))
         (prompt   (format "%d packages are missing; go over them to install?" missing)))
    (when (and packages
               (y-or-n-p-with-timeout prompt 30 nil))
      (package-refresh-contents)
      (dolist (pkg packages)
        (when (y-or-n-p (format "Install package %s?" pkg))
          (package-install pkg))))))

(who/install-missing-packages)

;; force loading of packages now, so we can use them from here on in .emacs
(setq package-enable-at-startup nil)
(package-initialize)

;; -----------------------------------------------------------------------------
;;  theme                                                                 MELPA
;; -----------------------------------------------------------------------------

(load-theme 'seti)

;; -----------------------------------------------------------------------------
;;  auto-complete mode                                                    MELPA
;; -----------------------------------------------------------------------------

(global-auto-complete-mode)

;; -----------------------------------------------------------------------------
;;  erlang mode                                                           MELPA
;; -----------------------------------------------------------------------------

;; set the path to the Erlang installation
(setq erlang-root-dir (cond
                       (on-mac     "/opt/local/lib/erlang")
                       (on-windows "C:/Program Files/erl6.3")
                       (on-linux   "")))

;; make sure Emacs can find the Erlang executables
(setq exec-path
      (cons (concat (file-name-as-directory erlang-root-dir) "bin")
            exec-path))

;; press C-c M-o (as in Slime) in a shell to clear the buffer
(add-hook 'erlang-shell-mode-hook
          (lambda ()
            (local-set-key "\C-c\M-o" #'erase-interactive-buffer)))
