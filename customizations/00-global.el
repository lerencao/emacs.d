;;; package --- summary
;;; Commentary:

;;; Code:

;;; Text mode and Auto Fill mode
;; The next two lines put Emacs into Text mode
;; and Auto Fill mode, and are for writers who
;; want to start writing prose rather than code.
(setq-default major-mode 'text-mode)
;; (add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(icomplete-mode t)
 '(menu-bar-mode nil)
 '(tool-bar-mode nil)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(global-linum-mode t)
 '(indent-tabs-mode nil)
 '(require-final-newline t)
 '(show-trailing-whitespace t)
 '(tab-width 2)
 '(make-backup-files nil)
 '(auto-fill-mode nil)
 '(truncate-lines t)
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

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


;; buffer-move key-bindings
(global-set-key (kbd "C-c <C-up>")     'buf-move-up)
(global-set-key (kbd "C-c <C-down>")   'buf-move-down)
(global-set-key (kbd "C-c <C-left>")   'buf-move-left)
(global-set-key (kbd "C-c <C-right>")  'buf-move-right)



;; exec-path-from-shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))


(setq-default
 whitespace-line-column 80
 whitespace-style '(face lines-tail trailing tabs))
(add-hook 'prog-mode-hook 'whitespace-mode)



(defalias 'yes-or-no-p 'y-or-n-p)


;; enable interactive-do
(ido-mode t)
(ido-everywhere t)
(flx-ido-mode t)
(setq ido-enable-flex-matching t)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)
(ido-ubiquitous-mode t)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; ediff configuration
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-diff-options "-w")

;; projectile mode config
(projectile-global-mode)
(setq projectile-keymap-prefix (kbd "C-c C-p"))

;; highlight indentation mode config
(require 'highlight-indentation)
(add-hook 'prog-mode-hook 'highlight-indentation-mode)
(set-face-background 'highlight-indentation-face "#e3e3d3")
(set-face-background 'highlight-indentation-current-column-face "#c3b3b3")

;; ace-jump
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)


;; expand-region config
(global-set-key (kbd "C-=") 'er/expand-region)

;; Original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
        If no region is selected and current line is not blank and we are not at the end of the line,
        then comment current line.
        Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
(global-set-key "\M-;" 'comment-dwim-line)


(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (ruby . t)))


;; font config

(defconst my/english-fonts
  '("Source Code Pro" "Monaco" "Consolas" "DejaVu Sans Mono" "Droid Sans Mono" "PragmataPro"
    "Courier" "Courier New" "Liberation Mono" "Ubuntu Mono" "Droid Sans Mono Pro"
    "Inconsolata" "Lucida Console" "Envy Code R" "Andale Mono"
    "Lucida Sans Typewriter" "monoOne" "Lucida Typewriter" "Panic Sans" "Hack"
    "Bitstream Vera Sans Mono" "HyperFont" "PT Mono" "Ti92Pluspc" "Excalibur Monospace"
    "Menlof" "Cousine" "Fira Mono" "Lekton" "M+ 1mn" "BPmono" "Free Mono"
    "Anonymous Pro" "ProFont" "ProFontWindows" "Latin Modern Mono" "Code 2002"
    "ProggyCleanTT" "ProggyTinyTT"))
(defconst my/chinese-fonts
  '("微软雅黑" "Microsoft Yahei" "Microsoft_Yahei" "文泉驿等宽微米黑" "文泉驿等宽正黑"
    "黑体" "Hiragino Sans GB"  "文泉驿正黑" "文泉驿点阵正黑" "SimHei" "SimSun" "NSimSun"
    "FangSong" "KaiTi" "FangSong_GB2312" "KaiTi_GB2312" "LiSu" "YouYuan"
    "新宋体" "宋体" "楷体_GB2312" "仿宋_GB2312" "幼圆" "隶书" "STXihei" "STKaiti"
    "STSong" "STZhongsong" "STFangsong" "FZShuTi" "FZYaoti" "STCaiyun" "STHupo" "STLiti"
    "STXingkai" "STXinwei" "方正姚体" "方正舒体" "方正粗圆_GBK" "华文仿宋" "华文中宋"
    "华文彩云" "华文新魏" "华文细黑" "华文行楷"))


(defun my/font-exists-p (font)
  "Check if FONT exists."
  (if (null (x-list-fonts font))
      nil t))

(defun my/set-font (english-fonts english-size chinese-fonts chinese-size)
  (require 'cl)
  (let* ((selected-en (find-if #'my/font-exists-p english-fonts))
         (en (format "%s:pixelsize=%d" selected-en english-size))
         (selected-zh (find-if #'my/font-exists-p chinese-fonts))
         (zh (font-spec :family selected-zh :size chinese-size)))

    (message "Set English Font to %s" en)
    (set-face-attribute 'default nil :font en)

    (message "Set Chinese Font to %s" zh)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset
                        zh)))
  )

(if (display-graphic-p)
    (let ((height (display-pixel-height))
          (width (display-pixel-width)))
      (cond ((and (>= width 1920) (>= height 1080))
             (my/set-font my/english-fonts 15
                          my/chinese-fonts 16))
            ((and (>= width 1366) (>= height 768))
             (my/set-font my/english-fonts 14
                          my/chinese-fonts 14))
            ((and (>= width 1024) (>= height 600))
             (my/set-font my/english-fonts 13
                          my/chinese-fonts 13))
            (t
             (my/set-font my/english-fonts 12
                          my/chinese-fonts 12)))
      )
  )

(load-theme 'leuven t)


;;; 00-global.el ends here
