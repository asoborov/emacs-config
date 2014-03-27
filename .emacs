;; my username and email
(setq user-mail-address "soborov@actimind.com")

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
                            projectile-rails
                            helm-projectile
                            dired+
                            rvm
                            textile-mode
                            ample-theme
                            expand-region
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

;; expand-region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; my keybindings
;; other-window-kill-buffer
(defun other-window-kill-buffer ()
  "Kill the buffer in the other window"
  (interactive)
  ;; Window selection is used because point goes to a different window
  ;; if more than 2 windows are present
  (let ((win-curr (selected-window))
        (win-other (next-window)))
    (select-window win-other)
    (kill-this-buffer)
    (select-window win-curr)))

(global-set-key (kbd "C-x K") 'other-window-kill-buffer)
;; wrap lines by words
(global-visual-line-mode t)

;; ace jump hotkey
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

;; replace everything while typing when region selected
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

;; no blinking cursor
(blink-cursor-mode 0)
(global-hl-line-mode t)

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

;; enable undo-tree everywhere
(global-undo-tree-mode t)

;; autocomplete config
(require 'auto-complete-config)
(ac-config-default)

;; textile mode
(require 'textile-mode)
(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))

;; add Gemfile to 'auto-mode
(add-to-list 'auto-mode-alist '("\\Gemfile\\'" . ruby-mode))

;; projectile settings
(projectile-global-mode)

;; remove toolbars and other stuff
(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1))

(setq echo-keystrokes 0.1
      use-dialog-box nil)

(define-key global-map (kbd "RET") 'newline-and-indent)

;; show line numbers
(global-linum-mode t)

;; hotkey for helm-mini
(global-set-key (kbd "<f7>") 'helm-mini)

;; hook for projectile-rails
(add-hook 'projectile-mode-hook 'projectile-rails-on)

;; make undo commands using undo-tree
(global-set-key (kbd "C-/") 'undo-tree-undo)
(global-set-key (kbd "C-?") 'undo-tree-redo)


;; Custom set variables
;; 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector (vector "#4d4d4c" "#c82829" "#718c00" "#eab700" "#4271ae" "#8959a8" "#3e999f" "#ffffff"))
 '(column-number-mode t)
 '(custom-enabled-themes (quote (ample)))
 '(custom-safe-themes (quote ("ed81411169b1b3e3d4cfc39b09d68ea13e0ff7708dc5b9d0bedb319e071968ad" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(display-battery-mode t)
 '(display-time-mode t)
 '(fci-rule-color "#efefef")
 '(jabber-roster-line-format " %c %-25n %u %-8s  %S")
 '(tool-bar-mode nil)
 '(tool-bar-position (quote top))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map (quote ((20 . "#c82829") (40 . "#f5871f") (60 . "#eab700") (80 . "#718c00") (100 . "#3e999f") (120 . "#4271ae") (140 . "#8959a8") (160 . "#c82829") (180 . "#f5871f") (200 . "#eab700") (220 . "#718c00") (240 . "#3e999f") (260 . "#4271ae") (280 . "#8959a8") (300 . "#c82829") (320 . "#f5871f") (340 . "#eab700") (360 . "#718c00"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "gray13" :foreground "#bdbdb3" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "unknown" :family "Ubuntu Mono"))))
 '(fringe ((t (:background "#1f1f1f" :foreground "#888")))))
