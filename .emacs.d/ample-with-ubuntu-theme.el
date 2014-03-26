(deftheme ample-with-ubuntu
  "Created 2014-03-26.")

(custom-theme-set-variables
 'ample-with-ubuntu
 '(tool-bar-position (quote top))
 '(tool-bar-mode nil)
 '(display-time-mode t)
 '(display-battery-mode t)
 '(column-number-mode t)
 '(custom-safe-themes (quote ("ed81411169b1b3e3d4cfc39b09d68ea13e0ff7708dc5b9d0bedb319e071968ad" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default))))

(custom-theme-set-faces
 'ample-with-ubuntu
 '(default ((t (:inherit nil :stipple nil :background "gray13" :foreground "#bdbdb3" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "unknown" :family "Ubuntu Mono")))))

(provide-theme 'ample-with-ubuntu)
