;; --------------------------------------------------------------------------
;;  Behaviour.
;; --------------------------------------------------------------------------

;; Don't want to see the startup screen.
(setq inhibit-startup-screen 1)

;; Use this font.
(set-face-attribute 'default nil :family "Anonymous Pro" :height 160)

;; Disable all kinds of GUI elements.
(custom-set-variables '(scroll-bar-mode nil)
                      '(tool-bar-mode nil)
                      '(menu-bar-mode nil))

;; Window movement.
(global-set-key [s-left]  'windmove-left)
(global-set-key [s-right] 'windmove-right)
(global-set-key [s-up]    'windmove-up)
(global-set-key [s-down]  'windmove-down)

;; Show the column number.
(column-number-mode t)

;; Show matching parenthesis.
(show-paren-mode t)

;; Use two spaces for tabs.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; Enable on-the-fly indentation.
(if (> emacs-major-version 23)
  (electric-indent-mode t)
  (global-set-key "\r" 'newline-and-indent))

;; Do not truncate long lines.
(setq-default truncate-lines t)

;; This is where my configuration lives.
(setq emacs-d "~/.emacs.d/")

;; --------------------------------------------------------------------------
;;  Behaviour specific to Mac OS X.
;; --------------------------------------------------------------------------

(when (eq system-type 'darwin)
  ;; Use the Command key as the Meta key.
  (setq mac-option-modifier  'super)
  (setq mac-command-modifier 'meta))

;; --------------------------------------------------------------------------
;;  Behaviour specific to Linux.
;; --------------------------------------------------------------------------

(when (eq system-type 'gnu/linux)
  ;; Allow copy & paste between Emacs and X.
  (setq x-select-enable-clipboard t)
  (setq interprogram-paste-function 'x-cut-buffer-or-selection-value))

;; --------------------------------------------------------------------------
;;  Handy functions.
;; --------------------------------------------------------------------------

;; Generates a TAGS file for code navigation.
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "cd %s ; find . -name '*.[chCH]' -print | etags -" (directory-file-name dir-name))))

;; --------------------------------------------------------------------------
;;  C-mode.  Built-in.
;; --------------------------------------------------------------------------

(setq-default c-default-style "linux"
              c-basic-offset 2)

;; --------------------------------------------------------------------------
;;  Shell mode.  Built-in.
;; --------------------------------------------------------------------------

;; Press C-c l in a shell to clear the buffer.
(add-hook 'shell-mode-hook
          (lambda ()
            (local-set-key "\C-cl"
                           (lambda ()
                             (interactive)
                             (let ((comint-buffer-maximum-size 0))
                               (comint-truncate-buffer))))))

;; --------------------------------------------------------------------------
;;  Org-mode.  Built-in.
;; --------------------------------------------------------------------------

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(setq org-log-done t)

;; Org-mode does not play nice with electric-indent-mode.
(add-hook 'org-mode-hook (lambda () (when electric-indent-mode
                                      (electric-indent-mode -1))))

;; --------------------------------------------------------------------------
;;  Rainbow delimiters.  See https://github.com/jlr/rainbow-delimiters/.
;; --------------------------------------------------------------------------

(add-to-list 'load-path (concat emacs-d "rainbow-delimiters"))
(autoload 'rainbow-delimiters-mode "rainbow-delimiters" t)

(add-hook 'haskell-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook    'rainbow-delimiters-mode)

;; --------------------------------------------------------------------------
;;  Haskell.  See https://github.com/haskell/haskell-mode/.
;; --------------------------------------------------------------------------

(load (concat emacs-d "haskell-mode/haskell-site-file"))

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; --------------------------------------------------------------------------
;;  Clojure.  See https://github.com/technomancy/clojure-mode/
;;              + https://github.com/kingtim/nrepl.el.
;; --------------------------------------------------------------------------

(add-to-list 'load-path (concat emacs-d "clojure-mode"))
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
(autoload 'clojure-mode "clojure-mode" t)

(add-to-list 'load-path (concat emacs-d "nrepl"))
(add-hook 'clojure-mode-hook (lambda () (require 'nrepl)))
(add-hook 'nrepl-interaction-hook 'nrepl-turn-on-eldoc-mode)

(setq nrepl-popup-stacktraces nil)

;; --------------------------------------------------------------------------
;;  JS2-mode.  See https://github.com/mooz/js2-mode/.
;; --------------------------------------------------------------------------

(when (> emacs-major-version 23)
  (add-to-list 'load-path (concat emacs-d "js2-mode"))
  (autoload 'js2-mode "js2-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (setq-default js2-basic-offset 4))

;; --------------------------------------------------------------------------
;;  Auto-complete.  See http://cx4a.org/software/auto-complete/
;;                    + https://github.com/mr-om/haskell-dict/.
;; --------------------------------------------------------------------------

;; Does not play nice with Emacs 22.
(when (> emacs-major-version 22)
  (add-to-list 'load-path (concat emacs-d "auto-complete"))
  (require 'auto-complete-config)
  (ac-config-default)
  (add-to-list 'ac-dictionary-directories (concat emacs-d "auto-complete/dict"))
  (add-to-list 'ac-modes 'haskell-mode))

;; --------------------------------------------------------------------------
;; Filesystem navigation.  See http://code.google.com/p/emacs-nav/.
;; --------------------------------------------------------------------------

(add-to-list 'load-path (concat emacs-d "nav"))
(require 'nav)
(nav-disable-overeager-window-splitting)
(global-set-key [f8] 'nav-toggle)
