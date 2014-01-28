;; -----------------------------------------------------------------------------
;;  general behaviour
;; -----------------------------------------------------------------------------

;; don't want to see the startup screen
(setq inhibit-startup-screen 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 
 ;; all themes are deemed safe
 '(custom-safe-themes t)

 ;; disable certain user interface elements
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))

;; window movement
(global-set-key [s-left]  'windmove-left)
(global-set-key [s-right] 'windmove-right)
(global-set-key [s-up]    'windmove-up)
(global-set-key [s-down]  'windmove-down)

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

;; this is where my configuration lives
(setq emacs-d "~/.emacs.d/")

;; this where the themes live
(when (>= emacs-major-version 24)
  (add-to-list 'custom-theme-load-path (concat emacs-d "themes")))

;; -----------------------------------------------------------------------------
;;  behaviour specific to Mac OS X
;; -----------------------------------------------------------------------------

(when (eq system-type 'darwin)
  ;; use the Command key as the Meta key
  (setq mac-option-modifier  'super)
  (setq mac-command-modifier 'meta)

  ;; use this font
  (set-face-attribute 'default nil
                      :family "DejaVu Sans Mono"
                      :height 130))

;; -----------------------------------------------------------------------------
;;  behaviour specific to Linux
;; -----------------------------------------------------------------------------

(when (and (eq system-type 'gnu/linux)
           (display-graphic-p))
  ;; use this font
  (set-face-attribute 'default nil
                      :family "DejaVu Sans Mono"
                      :height 100)

  ;; allow copy & paste between Emacs and X
  (setq x-select-enable-clipboard t)
  (setq interprogram-paste-function 'x-cut-buffer-or-selection-value))

;; -----------------------------------------------------------------------------
;;  handy functions
;; -----------------------------------------------------------------------------

(defun create-tags (dir-name)
  "Generates a TAGS file for code navigation."
  (interactive "DDirectory: ")
  (shell-command
   (format "cd %s ; find . -name '*.[chCH]' -print | etags -"
           (directory-file-name dir-name))))

(defun erase-interactive-buffer ()
  "Erases an interactive buffer (shell, REPL) but leaves the prompt alone."
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

;; -----------------------------------------------------------------------------
;;  ido                                                               built-in
;; -----------------------------------------------------------------------------

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;; -----------------------------------------------------------------------------
;;  c-mode                                                            built-in
;; -----------------------------------------------------------------------------

(setq-default c-default-style "linux"
              c-basic-offset 2)

;; -----------------------------------------------------------------------------
;;  shell mode                                                        built in
;; -----------------------------------------------------------------------------

;; press C-c M-o (as in Slime) in a shell to clear the buffer
(add-hook 'shell-mode-hook
          (lambda ()
            (local-set-key "\C-c\M-o" #'erase-interactive-buffer)))

;; -----------------------------------------------------------------------------
;;  org-mode                                                          built-in
;; -----------------------------------------------------------------------------

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(setq org-log-done t)

;; org-mode does not play nice with electric-indent-mode:
;; http://foldl.me/2012/disabling-electric-indent-mode/
(add-hook 'org-mode-hook
          (lambda ()
            (set (make-local-variable 'electric-indent-functions)
                 (list (lambda (arg) 'no-indent)))))

;; -----------------------------------------------------------------------------
;;  rainbow delimiters              https://github.com/jlr/rainbow-delimiters/
;; -----------------------------------------------------------------------------

(add-to-list 'load-path (concat emacs-d "rainbow-delimiters"))
(autoload 'rainbow-delimiters-mode "rainbow-delimiters" t)

(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook    'rainbow-delimiters-mode)

;; -----------------------------------------------------------------------------
;;  haskell                           https://github.com/haskell/haskell-mode/
;; -----------------------------------------------------------------------------

(add-to-list 'load-path (concat emacs-d "haskell-mode/"))
(require 'haskell-mode-autoloads)
(add-to-list 'Info-default-directory-list (concat emacs-d "haskell-mode/"))

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(setq haskell-program-name
      (cond
       ((eq system-type 'darwin)        "/opt/local/bin/ghci")
       ((eq system-type 'gnu/linux)     "/usr/bin/ghci")))

;; press C-c M-o (as in Slime) to clear the buffer
(add-hook 'inferior-haskell-mode-hook
          (lambda ()
            (local-set-key "\C-c\M-o" #'erase-interactive-buffer)))

;; -----------------------------------------------------------------------------
;;  clojure                       https://github.com/technomancy/clojure-mode/
;;                                https://github.com/kingtim/nrepl.el
;; -----------------------------------------------------------------------------

(add-to-list 'load-path (concat emacs-d "clojure-mode"))
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
(autoload 'clojure-mode "clojure-mode" t)

(add-to-list 'load-path (concat emacs-d "nrepl"))
(add-hook 'clojure-mode-hook (lambda () (require 'nrepl)))
(add-hook 'nrepl-interaction-hook 'nrepl-turn-on-eldoc-mode)

(setq nrepl-popup-stacktraces nil)

;; -----------------------------------------------------------------------------
;;  js2-mode                                https://github.com/mooz/js2-mode/
;; -----------------------------------------------------------------------------

(when (>= emacs-major-version 23)
  (add-to-list 'load-path (concat emacs-d (if (>= emacs-major-version 24)
                                              "js2-mode"
                                            "js2-mode-emacs23")))
  (autoload 'js2-mode "js2-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (setq-default js2-basic-offset 4))

;; -----------------------------------------------------------------------------
;;  slime                                http://common-lisp.net/project/slime/
;; -----------------------------------------------------------------------------

(add-to-list 'load-path (concat emacs-d "slime"))
(setq inferior-lisp-program
      (cond
       ((eq system-type 'darwin)        "/opt/local/bin/sbcl")
       ((eq system-type 'gnu/linux)     "/usr/bin/sbcl")))
(require 'slime)
(slime-setup '(slime-fancy))

;; -----------------------------------------------------------------------------
;;  auto-complete                      http://cx4a.org/software/auto-complete/
;;                                     https://github.com/mr-om/haskell-dict/
;; -----------------------------------------------------------------------------

;; does not play nice with Emacs 22
(when (> emacs-major-version 22)
  (add-to-list 'load-path (concat emacs-d "auto-complete"))
  (require 'auto-complete-config)
  (ac-config-default)
  (add-to-list 'ac-dictionary-directories (concat emacs-d "auto-complete/dict"))
  (add-to-list 'ac-modes 'haskell-mode)
  (add-to-list 'ac-modes 'lisp-mode)
  (add-hook 'js2-mode-hook (lambda () (setq ac-ignores '("//")))))

;; -----------------------------------------------------------------------------
;;  ac-slime                              https://github.com/purcell/ac-slime/
;; -----------------------------------------------------------------------------

(when (require 'auto-complete nil 'noerror)
  (add-to-list 'load-path (concat emacs-d "ac-slime"))
  (require 'ac-slime)
  (add-hook 'slime-mode-hook 'set-up-slime-ac))

;; -----------------------------------------------------------------------------
;;  ugly automatically added section
;; -----------------------------------------------------------------------------

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
