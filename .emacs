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
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(column-number-mode t)
 '(custom-safe-themes t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))

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

;; this is where my configuration lives
(setq emacs-d "~/.emacs.d/")

;; load this theme
(when (>= emacs-major-version 24)
  (add-to-list 'custom-theme-load-path (concat emacs-d "themes"))
  (load-theme 'charcoal-black t))

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
                      :height 130)

  ;; search for external programs here
  (setq exec-path
        (append exec-path `("/opt/local/bin"
                            ,(substitute-in-file-name "$HOME/Git/toy-programs/go/bin")))))

;; -----------------------------------------------------------------------------
;;  behaviour specific to Linux
;; -----------------------------------------------------------------------------

(when (eq system-type 'gnu/linux)

  (when (display-graphic-p)
    ;; use this font
    (set-face-attribute 'default nil
                        :family "M+ 1mn"
                        :height 120)

    ;; allow copy & paste between Emacs and X
    (setq x-select-enable-clipboard t)
    (setq interprogram-paste-function 'x-cut-buffer-or-selection-value))

  ;; shortcuts for quickly viewing images on the W-drive
  (global-set-key (kbd "<f4>") 'show-next-image-from-w)
  (global-set-key (kbd "<f5>") 'show-image-from-w)

  ;; search for external programs here
  (setq exec-path
        (append exec-path (list (substitute-in-file-name "$HOME/Git/toy-programs/go/bin")))))

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

(defun show-next-image-from-w ()
  "Locates the next W-drive image specifier in the current buffer and opens it with eom (Eye of Mate)."
  (interactive)
  (if (re-search-forward "[wW]:\\\\[[:digit:]]\\{5\\}\\\\Images\\\\[[:digit:]]\\{4\\}\\\\[[:digit:]]\\{5\\}_[[:digit:]]\\{6\\}\.[[:alnum:]]+" nil t)
      (let* ((name-org (match-string 0))
             (name-new (format "/run/user/1000/gvfs/smb-share\:server\=mdnlfsc02.domain2.local\,share\=alprdata/%s"
                               (replace-regexp-in-string "\\\\" "/" (substring name-org 3)))))
        (if (file-exists-p name-new)
            (progn
              (shell-command-to-string (format "eom %s > /dev/null 2>&1 &" name-new))
              (message "Showing %s" name-org))
          (message "Could not find %s; did you forget to mount the W-drive?" name-org)))
    (message "No W-drive image filename found to the right of point")))

(defun show-image-from-w ()
  "Locates a single W-drive image specifier in the current buffer and opens it with eom (Eye of Mate)."
  (interactive)
  (let* ((current-line (thing-at-point 'line))
         (image-regexp "[wW]:\\\\[[:digit:]]\\{5\\}\\\\Images\\\\[[:digit:]]\\{4\\}\\\\[[:digit:]]\\{5\\}_[[:digit:]]\\{6\\}\.[[:alnum:]]+")
         (start        (string-match image-regexp current-line)))
    (if start
        (let* ((name-org (substring current-line start (match-end 0)))
               (name-new (format "/run/user/1000/gvfs/smb-share\:server\=mdnlfsc02.domain2.local\,share\=alprdata/%s"
                                 (replace-regexp-in-string "\\\\" "/" (substring name-org 3)))))
          (if (file-exists-p name-new)
              (progn
                (shell-command-to-string (format "eom %s > /dev/null 2>&1 &" name-new))
                (message "Showing %s" name-org))
            (message "Could not find %s; did you forget to mount the W-drive?" name-org)))
      (message "No W-drive image filename found to the right of point"))))

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
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-offset 'case-label '+)))

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
;;  graphviz-dot-mode                 http://users.skynet.be/ppareit/projects/
;;                                    graphviz-dot-mode/graphviz-dot-mode.html
;; -----------------------------------------------------------------------------

;(load-file (concat emacs-d "graphviz-dot-mode/graphviz-dot-mode.el"))

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
;;  go-mode / godef / gocod             https://github.com/dominikh/go-mode.el
;; -----------------------------------------------------------------------------

(add-to-list 'load-path (concat emacs-d "go-mode"))
(require 'go-mode-load)

(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "M-.") 'godef-jump)))

(add-to-list 'load-path (concat emacs-d "gocode"))
(require 'go-autocomplete)
(require 'auto-complete-config)

;; -----------------------------------------------------------------------------
;;  rust-mode        https://github.com/mozilla/rust/tree/master/src/etc/emacs
;; -----------------------------------------------------------------------------

(add-to-list 'load-path (concat emacs-d "rust-mode"))
(require 'rust-mode)

;; -----------------------------------------------------------------------------
;;  erlang-mode  http://www.erlang.org/doc/apps/tools/erlang_mode_chapter.html
;; -----------------------------------------------------------------------------

(add-to-list 'load-path (concat emacs-d "erlang-mode"))
(setq erlang-root-dir "/opt/local/lib/erlang")
(add-to-list 'exec-path "/opt/local/lib/erlang/bin")
(require 'erlang-start)

;; -----------------------------------------------------------------------------
;;  ugly automatically added section
;; -----------------------------------------------------------------------------

