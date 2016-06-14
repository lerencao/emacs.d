;;; package --- summary
;;; Commentary:

;;; Code:

;;; Text mode and Auto Fill mode
;; The next two lines put Emacs into Text mode
;; and Auto Fill mode, and are for writers who
;; want to start writing prose rather than code.
(setq-default major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

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

;; By default, C-x C-b runs the list-buffers command.
;; This command lists your buffers in another window.
;; Since I almost always want to do something in that window,
;; I prefer the buffer-menu command, which not only lists the buffers,
;; but moves point into that window.
(global-set-key (kbd "C-x C-b") 'buffer-menu)

;; font config
;; (set-face-attribute 'default nil :height 150)

(defun set-font (english chinese english-size chinese-size)
  (set-face-attribute 'default nil :font
                      (format "%s:pixelsize=%d" english english-size))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family chinese :size chinese-size))))
(if (display-graphic-p)
    (set-font "WenQuanYi Micro Hei Mono" "WenQuanYi Micro Hei Mono" 14 14))

(load-theme 'solarized-light t)

(setq-default
 whitespace-line-column 80
 whitespace-style '(face lines-tail trailing tabs))
(add-hook 'prog-mode-hook 'whitespace-mode)
;;; 00-global.el ends here
