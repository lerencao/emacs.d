;;; package --- summary
;;; Commentary:

;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(icomplete-mode t)
 '(menu-bar-mode nil)
 '(show-paren-mode t)
 '(global-linum-mode t)
 '(indent-tabs-mode nil)
 '(require-final-newline t)
 '(show-trailing-whitespace t)
 '(tab-width 2)
 '(make-backup-files nil))

(global-hl-line-mode 1)
;; customize the background color
(set-face-background 'hl-line "white")
(set-face-foreground 'highlight nil)

(add-hook 'before-save-hook 'whitespace-cleanup)

(windmove-default-keybindings)


;; magit mode global key set
(global-set-key (kbd "C-x g") 'magit-status)

;; font config
;; (defun set-font (english chinese english-size chinese-size)
;;   (set-face-attribute 'default nil :font
;;                       (format "%s:pixelsize=%d" english english-size))
;;   (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;     (set-fontset-font (frame-parameter nil 'font) charset
;;                       (font-spec :family chinese :size chinese-size))))

;; (set-font "WenQuanYi Zen Hei Mono" "WenQuanYi Zen Hei Mono" 14 14)

;;; 00-global.el ends here
