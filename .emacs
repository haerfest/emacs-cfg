;; --------------------------------------------------------------------------
;;  Behaviour.
;; --------------------------------------------------------------------------

;; Don't want to see the startup screen.
(setq inhibit-startup-screen 1)

;; Use this font.
(set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 150)

;; Disable scroll bars and tool bars.
(custom-set-variables '(scroll-bar-mode nil)
                      '(tool-bar-mode nil))

;; Window movement.
(global-set-key [M-left]  'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up]    'windmove-up)
(global-set-key [M-down]  'windmove-down)

;; Show matching parenthesis.
(show-paren-mode t)

;; Use spaces for tabs.
(setq-default indent-tabs-mode nil)

;; Indent each new line automatically.
(global-set-key "\r" 'newline-and-indent)

;; Do not truncate long lines.
(setq-default truncate-lines t)

;; This is where my configuration lives.
(setq emacs-d "~/.emacs.d/")

;; --------------------------------------------------------------------------
;;  Behaviour specific to Mac OS X.
;; --------------------------------------------------------------------------

(if (eq system-type 'darwin)
    (progn
      ;; Use the Command key as the Meta key.
      (setq mac-option-modifier  'super)
      (setq mac-command-modifier 'meta)))

;; --------------------------------------------------------------------------
;;  Behaviour specific to Linux.
;; --------------------------------------------------------------------------

(if (eq system-type 'gnu/linux)
    (progn
      ;; Allow copy & paste between Emacs and X.
      (setq x-select-enable-clipboard t)
      (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)))

;; --------------------------------------------------------------------------
;;  Rainbow delimiters.  See https://github.com/jlr/rainbow-delimiters.
;; --------------------------------------------------------------------------

(add-to-list 'load-path (concat emacs-d "rainbow-delimiters"))
(autoload 'rainbow-delimiters-mode "rainbow-delimiters" t)
(add-hook 'haskell-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook    'rainbow-delimiters-mode)

;; --------------------------------------------------------------------------
;;  Haskell.  See https://github.com/haskell/haskell-mode.
;; --------------------------------------------------------------------------

(load (concat emacs-d "haskell-mode/haskell-site-file"))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; --------------------------------------------------------------------------
;;  Clojure.  See https://github.com/technomancy/clojure-mode
;;              + https://github.com/kingtim/nrepl.el.
;; --------------------------------------------------------------------------

(add-to-list 'load-path (concat emacs-d "clojure-mode"))
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
(autoload 'clojure-mode "clojure-mode" t)

(add-to-list 'load-path (concat emacs-d "nrepl"))
(add-hook 'clojure-mode-hook (lambda () (require 'nrepl)))
(add-hook 'nrepl-interaction-hook 'nrepl-turn-on-eldoc-mode)
(setq nrepl-popup-stacktraces nil)

