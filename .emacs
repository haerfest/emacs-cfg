;; --------------------------------------------------------------------------
;;  Behaviour.
;; --------------------------------------------------------------------------

;; Don't want to see the startup screen.
(setq inhibit-startup-screen 1)

;; Use this font.
(set-face-attribute 'default nil :family "Anonymous Pro" :height 160)

 ;; Disable certain user interface elements.
(custom-set-variables
  '(scroll-bar-mode nil)
  '(tool-bar-mode nil))

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
;; - tabs ('tabs')
;; - empty lines at beginning/end of buffer ('empty')
;; - trailing whitespace ('trailing')
;; - lines with columns beyond 80 ('lines-tail')
(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(tabs empty tabs lines-tail trailing))
(global-whitespace-mode t)

;; Enable on-the-fly indentation.
(if (> emacs-major-version 23)
  (electric-indent-mode t)
  (global-set-key "\r" 'newline-and-indent))

;; Do not truncate long lines.
(setq-default truncate-lines t)

;; Accept y/n for yes/no.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Undo when pressing ^z, suspend with the super key.
(global-set-key [(control z)] 'undo)
(global-set-key [(super control z)] 'suspend-frame)

;; Use CLisp as my Lisp.
(setq inferior-lisp-program "/usr/local/bin/clisp")

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
;;  Interactively Do Things (IDO).  Built-in.
;; --------------------------------------------------------------------------

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

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

;; --------------------------------------------------------------------------
;;  JS2-mode.  See https://github.com/mooz/js2-mode/.
;; --------------------------------------------------------------------------

(add-to-list 'load-path (concat emacs-d (if (> emacs-major-version 23)
                                          "js2-mode"
                                          "js2-mode-emacs23")))
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq-default js2-basic-offset 4)

;; --------------------------------------------------------------------------
;;  Filesystem navigation.  See http://code.google.com/p/emacs-nav/.
;; --------------------------------------------------------------------------

(add-to-list 'load-path (concat emacs-d "nav"))
(require 'nav)
(nav-disable-overeager-window-splitting)
(global-set-key [f8] 'nav-toggle)

;; --------------------------------------------------------------------------
;;  Minimap.  See http://www.emacswiki.org/emacs/MiniMap.
;; --------------------------------------------------------------------------

(add-to-list 'load-path (concat emacs-d "minimap"))
(require 'minimap)

;; --------------------------------------------------------------------------
;;  Slime.  See http://common-lisp.net/project/slime/.
;; --------------------------------------------------------------------------

(add-to-list 'load-path (concat emacs-d "slime"))
(setq inferior-lisp-program "/usr/local/bin/clisp")
(require 'slime)
(slime-setup '(slime-fancy))

;; --------------------------------------------------------------------------
;;  Auto-complete.  See http://cx4a.org/software/auto-complete
;;                    + https://github.com/mr-om/haskell-dict.
;; --------------------------------------------------------------------------

;; Does not play nice with Emacs 22.
(when (> emacs-major-version 22)
  (add-to-list 'load-path (concat emacs-d "auto-complete"))
  (require 'auto-complete-config)
  (ac-config-default)
  (add-to-list 'ac-dictionary-directories (concat emacs-d "auto-complete/dict"))
  (add-to-list 'ac-modes 'haskell-mode)
  (add-to-list 'ac-modes 'lisp-mode)
  (add-hook 'js2-mode-hook (lambda ()
                             (setq ac-ignores '("//")))))

;; --------------------------------------------------------------------------
;;  AC-slime.  See https://github.com/purcell/ac-slime.
;; --------------------------------------------------------------------------

;; No need if auto-complete is not available.
(when (> emacs-major-version 22)
  (add-to-list 'load-path (concat emacs-d "ac-slime"))
  (require 'ac-slime)
  (add-hook 'slime-mode-hook 'set-up-slime-ac)
  (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
  (eval-after-load "auto-complete"
    '(add-to-list 'ac-modes 'slime-repl-mode)))

;; --------------------------------------------------------------------------
;;  Paredit.  See http://www.emacswiki.org/emacs/ParEdit.
;; --------------------------------------------------------------------------

(add-to-list 'load-path (concat emacs-d "paredit"))
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
