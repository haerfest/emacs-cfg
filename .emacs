;; --------------------------------------------------------------------------
;;  Behaviour.
;; --------------------------------------------------------------------------

;; Don't want to see the startup screen.
(setq inhibit-startup-screen 1)

;; Use this font.
(set-face-attribute 'default nil :family "Anonymous Pro" :height 160)

;; Disable scroll bars and tool bars.
(custom-set-variables '(scroll-bar-mode nil)
                      '(tool-bar-mode nil))

;; Use the Command key as the Meta key (Mac-only).
(setq mac-option-modifier  'super)
(setq mac-command-modifier 'meta)

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

;; This is where my configuration lives.
(setq emacs-d "~/.emacs.d/")

;; --------------------------------------------------------------------------
;;  Haskell.  See https://github.com/haskell/haskell-mode.
;; --------------------------------------------------------------------------

(load (concat emacs-d "haskell-mode/haskell-site-file"))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; --------------------------------------------------------------------------
;;  Clojure.  See https://github.com/technomancy/clojure-mode.
;;            See https://github.com/kingtim/nrepl.el.
;; --------------------------------------------------------------------------

(add-to-list 'load-path (concat emacs-d "clojure-mode"))
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
(autoload 'clojure-mode "clojure-mode" t)

(add-to-list 'load-path (concat emacs-d "nrepl"))
(add-hook 'clojure-mode-hook (lambda () (require 'nrepl)))
(add-hook 'nrepl-interaction-hook 'nrepl-turn-on-eldoc-mode)
(setq nrepl-popup-stacktraces nil)
