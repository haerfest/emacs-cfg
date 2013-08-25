;; ----------------------------------------------------------------------------
;;  Behaviour.
;; ----------------------------------------------------------------------------

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

;; Skip .svn directories when doing a grep-find.
(setq grep-find-command
      "find . -type f '!' -wholename '*/.svn/*' -print0 | xargs -0 -e grep -nH -e ")

;; This is where my configuration lives.
(setq emacs-d "~/.emacs.d/")

;; ----------------------------------------------------------------------------
;;  Behaviour specific to Mac OS X.
;; ----------------------------------------------------------------------------

(when (eq system-type 'darwin)
  ;; Use the Command key as the Meta key.
  (setq mac-option-modifier  'super)
  (setq mac-command-modifier 'meta))

;; ----------------------------------------------------------------------------
;;  Behaviour specific to Linux.
;; ----------------------------------------------------------------------------

(when (eq system-type 'gnu/linux)
  ;; Allow copy & paste between Emacs and X.
  (setq x-select-enable-clipboard t)
  (setq interprogram-paste-function 'x-cut-buffer-or-selection-value))

;; ----------------------------------------------------------------------------
;;  Handy functions.
;; ----------------------------------------------------------------------------

(defun create-tags (dir-name)
  "Generates a TAGS file for code navigation."
  (interactive "DDirectory: ")
  (shell-command
   (format "cd %s ; find . -name '*.[chCH]' -print | etags -"
           (directory-file-name dir-name))))

;; ----------------------------------------------------------------------------
;;  Interactively Do Things (IDO).  Built-in.
;; ----------------------------------------------------------------------------

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;; ----------------------------------------------------------------------------
;;  C-mode.  Built-in.
;; ----------------------------------------------------------------------------

(setq-default c-default-style "linux"
              c-basic-offset 2)

;; ----------------------------------------------------------------------------
;;  Shell mode.  Built-in.
;; ----------------------------------------------------------------------------

;; Press C-c l in a shell to clear the buffer.
(add-hook 'shell-mode-hook
          (lambda ()
            (local-set-key "\C-cl"
                           (lambda ()
                             (interactive)
                             (let ((comint-buffer-maximum-size 0))
                               (comint-truncate-buffer))))))

;; ----------------------------------------------------------------------------
;;  Org-mode.  Built-in.
;; ----------------------------------------------------------------------------

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(setq org-log-done t)

;; Org-mode does not play nice with electric-indent-mode.
(add-hook 'org-mode-hook (lambda ()
                           (when electric-indent-mode
                             (electric-indent-mode -1))))
