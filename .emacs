;; ----------------------------------------------------------------------------
;;  general behaviour
;; ----------------------------------------------------------------------------

;; don't want to see the startup screen
(setq inhibit-startup-screen t)

;; don't want any bell sounds
(setq ring-bell-function 'ignore)

;; don't want any fancy GUI widgets
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

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

;; press F5 to revert the current buffer
(global-set-key [f5] 'revert-buffer)

;; press F2 ro rename the current buffer
(global-set-key [f2] 'rename-buffer)

;; skip .svn directories when doing a grep-find
(setq grep-find-command
      (concat "find . -type f '!' -wholename '*/.svn/*' -print0 | "
              "xargs -0 grep -nH -e "))

;; figure out which OS we're running on
(defvar on-mac     (eq system-type 'darwin)     "t if OS is Mac OS X")
(defvar on-windows (eq system-type 'windows-nt) "t if OS is Windows")
(defvar on-linux   (eq system-type 'gnu/linux)  "t if OS is Linux")

;; treat all themes as safe
(setq custom-safe-themes t)

;; when going full-screen, disable the menu bar
(setq is-fullscreen nil)
(defun my-toggle-frame-fullscreen ()
  (interactive)
  (toggle-frame-fullscreen)
  (setq is-fullscreen (not is-fullscreen))
  (menu-bar-mode (if is-fullscreen -1 +1)))
(global-set-key [f11]'my-toggle-frame-fullscreen)

;; show the time
(setq display-time-24hr-format t)
(display-time-mode 1)

;; highlight the current line
(global-hl-line-mode 1)

;; truncate lines (i.e. don't wrap)
(set-default 'truncate-lines t)

;; ----------------------------------------------------------------------------
;;  Mac OS X
;; ----------------------------------------------------------------------------
(when on-mac
  ;; activate dark mode
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  
  ;; use this font
  (set-face-attribute 'default nil
                      :family "Courier"
                      :weight 'normal
                      :height 160)

  ;; use the Command key as the Meta key
  (setq mac-option-modifier  'super)
  (setq mac-command-modifier 'meta))

;; ----------------------------------------------------------------------------
;;  Linux
;; ----------------------------------------------------------------------------
(when (and on-linux (display-graphic-p))
  ;; use this font
  (set-face-attribute 'default nil
                      :family "DejaVu Sans Mono"
                      :height 120)

  ;; allow copy & paste between Emacs and X
  (setq x-select-enable-clipboard t)
  (setq interprogram-paste-function 'x-cut-buffer-or-selection-value))

;; ----------------------------------------------------------------------------
;;  Windows
;; ----------------------------------------------------------------------------
(when on-windows
  ;; use this font
  (set-face-attribute 'default nil
                      :family "Source Code Pro"
                      :height 120)

  ;; open links with Windows' default browser
  (setq browse-url-browser-function 'browse-url-default-windows-browser))

;; ----------------------------------------------------------------------------
;;  handy functions
;; ----------------------------------------------------------------------------

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

(defun aweta-fix-id-attributes-region (start end)
  "Numbers all XML id=... tags in a region, starting with 0."
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (let ((id 0))
      (while (search-forward-regexp " id=\"[0-9]+\"" nil t)
        (replace-match (format " id=\"%d\"" id))
        (setq id (1+ id)))
      (message (format "Updated %d id's" id)))))

;; ----------------------------------------------------------------------------
;;  uniquify                                                          built-in
;; ----------------------------------------------------------------------------

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; ----------------------------------------------------------------------------
;;  hideshow                                                          built-in
;; ----------------------------------------------------------------------------

;; press F6 to toggle folding
(global-set-key [f6] 'hs-toggle-hiding)

;; fix hideshow XML folding
(add-to-list 'hs-special-modes-alist
             (list 'nxml-mode
                   "<!--\\|<[^/>]*[^/]>"
                   "-->\\|</[^/>]*[^/]>"
                   "<!--"
                   'nxml-forward-element
                   nil))

;; fix hideshow HTML folding
(dolist (mode '(sgml-mode
                html-mode
                html-erb-mode))
  (add-to-list 'hs-special-modes-alist
               (list mode
                     "<!--\\|<[^/>]*[^/]>"
                     "-->\\|</[^/>]*[^/]>"
                     "<!--"
                     'sgml-skip-tag-forward
                     nil)))

;; activate hideshow when editing certain files
(dolist (mode '(nxml-mode
                sgml-mode
                html-mode
                html-erg-mode
                c-mode-common-hook))
  (add-hook mode 'hs-minor-mode))

;; ----------------------------------------------------------------------------
;;  semantic                                                          built-in
;; ----------------------------------------------------------------------------

(require 'cc-mode)
(require 'semantic)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)

(when on-windows
  (semantic-add-system-include "D:/work/awu")
  (semantic-add-system-include "D:/work/thirdparty/include"))

(semantic-mode 1)

;; ----------------------------------------------------------------------------
;;  ido                                                                built-in
;; ----------------------------------------------------------------------------

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;; ----------------------------------------------------------------------------
;;  c-mode                                                             built-in
;; ----------------------------------------------------------------------------

(setq-default c-default-style "linux"
              c-basic-offset 2)
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-offset 'case-label '+)))

;; ----------------------------------------------------------------------------
;;  shell mode                                                         built in
;; ----------------------------------------------------------------------------

;; press C-c M-o (as in Slime) in a shell to clear the buffer
(add-hook 'shell-mode-hook
          (lambda ()
            (local-set-key "\C-c\M-o" #'erase-interactive-buffer)))

;; ----------------------------------------------------------------------------
;;  ispell                                                             built-in
;; ----------------------------------------------------------------------------

(when on-mac
  (setq ispell-program-name "/opt/local/bin/aspell"))

;; ----------------------------------------------------------------------------
;;  whitespace mode                                                    built-in
;; ----------------------------------------------------------------------------

(require 'whitespace)
(global-set-key (kbd "C-c C-w") 'whitespace-mode)

;; ----------------------------------------------------------------------------
;;  packages
;; ----------------------------------------------------------------------------

;; credits to http://www.aaronbedra.com/emacs.d/
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
	       '("melpa-stable" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives
	       '("elpy" . "http://jorgenschaefer.github.io/packages/"))

  (package-initialize))

;; default packages to have installed
(defvar who/packages '(
                       better-defaults
                       company
                       exec-path-from-shell
                       ido-vertical-mode
                       markdown-mode
                       multiple-cursors
                       which-key
                       yasnippet

                       ;; themes
                       material-theme
                       noctilux-theme
                       planet-theme
                       solarized-theme
                       zenburn-theme

                       ;; git support
                       magit

                       ;; python development
                       elpy
                       flycheck
                       py-autopep8

                       ;; common lisp development
                       slime
                       slime-company

                       ;; C/C++ development
                       ggtags

                       ;; web development
                       multi-web-mode
                       ))

;; define the filter function if not there
(unless (fboundp 'filter)
  (defun filter (pred-p items)
    (let (filtered)
      (dolist (item items)
        (when (funcall pred-p item)
          (setq filtered (cons item filtered))))
      (reverse filtered))))

(when (>= emacs-major-version 24)

  ;; returns which packages are missing
  (defun who/missing-packages ()
    "Returns a list of missing packages."
    (filter (lambda (pkg)
              (not (package-installed-p pkg)))
            who/packages))

  ;; make sure certain packages are installed
  (defun who/install-packages (packages)
    "Installs missing packages."
    (let* ((missing  (length packages))
           (prompt   (format "%d missing packages: %s. Install?"
                             missing packages)))
      ;; if any packages are missing, ask the user whether to install
      (when (and packages
                 (y-or-n-p-with-timeout prompt 3 nil))
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

  ;; returns whether the package servers are accessible
  ;; http://emacs.stackexchange.com/questions/7653/
  ;;    elisp-code-to-check-for-internet-connection
  (defun who/can-retrieve-packages ()
    (cl-loop for url in (mapcar 'cdr package-archives)
             do (condition-case e
                    (kill-buffer (url-retrieve-synchronously url))
                  (error (cl-return)))
             finally (cl-return t)))

  (defun who/update ()
    (let ((missing-packages (who/missing-packages)))
      (when (and missing-packages
                 (who/can-retrieve-packages))
        (who/install-packages missing-packages))))

  ;; force loading of packages now, so we can use them from here on in .emacs
  (setq package-enable-at-startup nil)
  (package-initialize))

(unless (fboundp 'package-installed-p)
  (defun package-installed-p (package)
    nil))

;; ----------------------------------------------------------------------------
;;  ggtags                                                              package
;; ----------------------------------------------------------------------------

(when (package-installed-p 'ggtags)
  (require 'ggtags)
  
  (add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))

  (when on-windows
    (setq ggtags-executable-directory "C:/Program Files (x86)/GNU/GLOBAL/bin"))

  (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
  (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
  (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
  (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
  (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
  (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
  
  (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark))

;; ----------------------------------------------------------------------------
;;  markdown-mode                                                       package
;; ----------------------------------------------------------------------------

(when (package-installed-p 'markdown-mode)
  (setq markdown-fontify-code-blocks-natively t)
  (custom-set-faces '(markdown-code-face ((t nil)))))

;; ----------------------------------------------------------------------------
;;  multiple-cursors                                                    package
;; ----------------------------------------------------------------------------

(when (package-installed-p 'multiple-cursors)
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->")         'mc/mark-next-like-this)
  (global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this))

;; ----------------------------------------------------------------------------
;;  exec-path-from-shell                                                package
;; ----------------------------------------------------------------------------

(when (and on-mac (package-installed-p 'exec-path-from-shell))
  (exec-path-from-shell-copy-env "PS1")
  (exec-path-from-shell-initialize))

;; ----------------------------------------------------------------------------
;;  company                                                             package
;; ----------------------------------------------------------------------------

(when (package-installed-p 'company)
  (add-hook 'after-init-hook 'global-company-mode))

;; ----------------------------------------------------------------------------
;;  elpy                                                                package
;; ----------------------------------------------------------------------------

(when (package-installed-p 'elpy)

  ;; press C-c M-o (as in Slime) in a shell to clear the buffer
  (add-hook 'inferior-python-mode-hook
            (lambda ()
              (local-set-key "\C-c\M-o" #'erase-interactive-buffer)))

  ;; activate hideshow
  (add-hook 'elpy-mode-hook (lambda () (hs-minor-mode)))

  ;; set encoding of the Python shell to UTF-8
  (setenv "LC_CTYPE" "UTF-8")
  (setenv "LC_ALL" "en_US.UTF-8")
  (setenv "LANG" "en_US.UTF-8")

  ;; prevent an error message about python not supporting readline
  (setq python-shell-completion-native-enable nil)

  (when on-mac
    (setq python-shell-interpreter "python3")
    (setq elpy-rpc-python-command "python3"))

  (when on-windows
    (setq python-shell-interpreter "py")
    (setq elpy-rpc-python-command "py"))

  (elpy-enable)

  ;; replace flymake by flycheck
  (when (package-installed-p 'flycheck)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))

  ;; activate py-autopep8 when saving
  (when (package-installed-p 'py-autopep8)
    (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)))

;; ----------------------------------------------------------------------------
;;  rainbow-delimiters                                                  package
;; ----------------------------------------------------------------------------

(when (and (package-installed-p 'rainbow-delimiters)
           (package-installed-p 'clojure-mode))
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'cider-mode-hook 'rainbow-delimiters-mode))

;; ----------------------------------------------------------------------------
;;  yasnippet                                                           package
;; ----------------------------------------------------------------------------

(when (package-installed-p 'yasnippet)
  (yas-global-mode 1))

;; ----------------------------------------------------------------------------
;;  which-key                                                           package
;; ----------------------------------------------------------------------------

(when (package-installed-p 'which-key)
  (which-key-mode))

;; ----------------------------------------------------------------------------
;;  slime                                                               package
;; ----------------------------------------------------------------------------

(when (package-installed-p 'slime)
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  (slime-setup '(slime-fancy slime-company)))

;; ----------------------------------------------------------------------------
;;  fsharp-mode                                                         package
;; ----------------------------------------------------------------------------

(when (package-installed-p 'fsharp-mode)
  (require 'fsharp-mode)
  (setq inferior-fsharp-program "/usr/local/bin/fsharpi --readline-")
  (setq fsharp-compiler "/usr/local/bin/fsharpc"))

;; ----------------------------------------------------------------------------
;;  ido-vertical-mode                                                  package
;; ----------------------------------------------------------------------------

(when (package-installed-p 'ido-vertical-mode)
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys '(C-n-and-C-p-only)))

;; ----------------------------------------------------------------------------
;;  csharp-mode                                                        package
;; ----------------------------------------------------------------------------

(when (package-installed-p 'csharp-mode)
  (add-hook 'csharp-mode-hook
            (lambda ()
              (electric-pair-mode 1)
              (c-set-offset 'inline-open 0))))

;; ----------------------------------------------------------------------------
;;  intero                                                             package
;; ----------------------------------------------------------------------------

(when (package-installed-p 'intero)
  (add-hook 'haskell-mode-hook 'intero-mode))

;; ----------------------------------------------------------------------------
;;  multi-web-mode                                                     package
;; ----------------------------------------------------------------------------

(when (package-installed-p 'multi-web-mode)
  (require 'multi-web-mode)
  (setq mweb-default-major-mode 'html-mode)
  (setq mweb-tags
        '((php-mode "<\\?php\\|<\\? \\|<\\?="
                    "\\?>")
          (js-mode "<script>\\|<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>"
                   "</script>")
          (css-mode "<style>\\|<style +type=\"text/css\"[^>]*>"
                    "</style>")))
  (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
  (multi-web-global-mode 1))

;; ----------------------------------------------------------------------------
;;  neotree                                                            package
;; ----------------------------------------------------------------------------

(when (package-installed-p 'neotree)
  (require 'neotree)
  (global-set-key [f8] 'neotree-toggle))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (protobuf-mode zweilight-theme zerodark-theme zeno-theme zenburn-theme zen-and-art-theme yoshi-theme xresources-theme white-theme white-sand-theme which-key warm-night-theme waher-theme underwater-theme ujelly-theme ubuntu-theme twilight-theme tramp-theme toxi-theme tommyh-theme termbright-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme suscolors-theme sunny-day-theme sunburn-theme sublime-themes subatomic256-theme subatomic-theme srcery-theme spacemacs-theme spacegray-theme sourcerer-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme snazzy-theme smyx-theme slime-theme slime-company silkworm-theme seti-theme seoul256-theme select-themes rimero-theme reykjavik-theme reverse-theme rebecca-theme rand-theme rainbow-delimiters railscasts-theme racket-mode py-autopep8 purple-haze-theme purp-theme punpun-theme projectile professional-theme prassee-theme poet-theme planet-theme plan9-theme plain-theme php-mode per-buffer-theme peacock-theme pastelmac-theme paper-theme panda-theme paganini-theme overcast-theme org-beautify-theme one-themes oldlace-theme oceanic-theme occidental-theme obsidian-theme nyx-theme nova-theme northcode-theme nordless-theme nord-theme nofrils-acme-theme noctilux-theme nimbus-theme night-owl-theme neotree naquadah-theme mustard-theme mustang-theme multiple-cursors multi-web-mode monotropic-theme monokai-theme monokai-alt-theme monochrome-theme molokai-theme moe-theme minimal-theme metalheart-theme mellow-theme melancholy-theme mbo70s-theme material-theme mandm-theme majapahit-theme magit madhat2r-theme lush-theme load-theme-buffer-local liso-theme light-soap-theme leuven-theme lenlen-theme lavender-theme labburn-theme lab-themes kosmos-theme kooten-theme klere-theme kaolin-themes jbeans-theme jazz-theme ir-black-theme iodine-theme inverse-acme-theme intero intellij-theme inkpot-theme ido-vertical-mode idea-darkula-theme heroku-theme hemisu-theme hemera-theme helm-themes hc-zenburn-theme hamburg-theme habamax-theme gruvbox-theme greymatters-theme green-screen-theme grayscale-theme grandshell-theme gotham-theme goose-theme github-theme geben gandalf-theme fsharp-mode forest-blue-theme foggy-night-theme flucui-themes flatui-theme flatui-dark-theme flatland-theme firecode-theme fill-column-indicator farmhouse-theme faff-theme eziam-theme exotica-theme exec-path-from-shell espresso-theme elpy eink-theme eclipse-theme dracula-theme doom-themes doneburn-theme django-theme darktooth-theme darkokai-theme darkmine-theme darkburn-theme dark-mint-theme dark-krystal-theme darcula-theme danneskjold-theme dakrone-theme cyberpunk-theme csharp-mode creamsody-theme constant-theme commentary-theme colonoscopy-theme clues-theme clojure-snippets clojure-mode-extra-font-locking cider chyla-theme caroline-theme cargo busybee-theme bubbleberry-theme brutalist-theme boron-theme borland-blue-theme bliss-theme blackboard-theme better-defaults basic-theme badwolf-theme badger-theme avk-emacs-themes autumn-light-theme atom-one-dark-theme atom-dark-theme arjen-grey-theme apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes airline-themes ahungry-theme afternoon-theme abyss-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-code-face ((t nil))))
