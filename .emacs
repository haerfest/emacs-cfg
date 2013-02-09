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

;; Show matching braces.
(show-paren-mode t)

;; Use spaces for tabs.
(setq-default indent-tabs-mode nil)

;; Indent each new line automatically.
(global-set-key "\r" 'newline-and-indent)
