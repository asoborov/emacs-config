(require 'package)
(require 'cl)
(require 'saveplace)

(package-initialize)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defvar asoborov/packages '(auto-complete
                            ace-jump-mode
                            color-theme-sanityinc-tomorrow
                            flx
                            flx-ido
                            yasnippet
                            magit
                            markdown-mode
                            sass-mode
                            coffee-mode
                            haml-mode
                            org
                            autopair
                            undo-tree
                            projectile
                            rvm
                            php-mode)
  "Default packages")

(defun asoborov/packages-installed-p ()
  (loop for pkg in asoborov/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (asoborov/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg asoborov/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; enable mainstream copy/paste keybindings
(cua-mode t)

;; wrap lines by words
(global-visual-line-mode t)

;; ace jump hotkey
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

;; replace everything while typing when region selected
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

;; change window title
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

;; turn on empty lines indication
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; show parentheses
(show-paren-mode t)

;; electric indent 
(electric-indent-mode t)
(electric-pair-mode t)

;; no backups, no autosave, no startup message
(setq make-backup-file nil)
(setq auto-save-default nil)
(setq inhibit-startup-message t)

;; default tab-width
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; saveplace file
(setq save-place-file (concat user-emacs-directory ".saveplace"))
(setq-default save-place t)

;; replace yes/no with y/p prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; ido mode w/flex
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-use-faces nil)

;; autocomplete config
(require 'auto-complete-config)
(ac-config-default)

;; remove toolbars and other stuff
(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1))

(setq echo-keystrokes 0.1
      use-dialog-box nil)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes (quote ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(display-battery-mode t)
 '(display-time-mode t)
 '(jabber-roster-line-format " %c %-25n %u %-8s  %S")
 '(tool-bar-mode nil)
 '(tool-bar-position (quote top)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
