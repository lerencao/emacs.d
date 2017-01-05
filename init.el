;;; package --- Summary
;;; Commentary:
;;; init file
;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; maximize frame window
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . fullheight))

(use-package solarized-theme
  :ensure t
  :config (load-theme 'solarized-light t)
  )

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

(defun my/set-gui-fonts-frame-hook (frame)
  "Set gui fonts on FRAME."
  (with-selected-frame frame
    (run-hooks
     (if (display-graphic-p)
         (let ((height (display-pixel-height))
               (width (display-pixel-width)))
           (cond ((and (>= width 1920) (>= height 1080))
                  (my/set-font my/english-fonts 16
                               my/chinese-fonts 18))
                 ((and (>= width 1366) (>= height 768))
                  (my/set-font my/english-fonts 14
                               my/chinese-fonts 14))
                 ((and (>= width 1024) (>= height 600))
                  (my/set-font my/english-fonts 13
                               my/chinese-fonts 13))
                 (t
                  (my/set-font my/english-fonts 12
                               my/chinese-fonts 12))))))))

;; set font size when emacs is inited or a frame is created.
(add-hook 'after-make-frame-functions 'my/set-gui-fonts-frame-hook)
(add-hook 'after-init-hook (lambda ()
                             (when (selected-frame)
                               (my/set-gui-fonts-frame-hook (selected-frame)))))



;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)



(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(global-linum-mode)
(global-hl-line-mode t)

;; do not auto line break
(turn-off-auto-fill)
;; truncate lines
(setq-default truncate-lines t)

;; don't make backup files
(setq-default make-backup-files nil)

;; see http://stackoverflow.com/a/1819405/1275303
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq-default show-trailing-whitespace t)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default major-mode 'text-mode)
(use-package flyspell
  :diminish flyspell-mode
  :init
  (add-hook 'text-mode-hook 'turn-on-flyspell))


;; ediff configuration
;; (setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; (setq ediff-split-window-function 'split-window-horizontally)
;; (setq ediff-diff-options "-w")

(use-package files
  :config
  (setq require-final-newline t)
  (add-hook 'before-save-hook 'whitespace-cleanup)
  )

;; enable whittespace mode in all prog modes

(use-package whitespace
  :diminish whitespace-mode
  :config
  (setq-default whitespace-line-column 80
                whitespace-style '(face lines-tail trailing tabs))
  (add-hook 'prog-mode-hook 'whitespace-mode)
  )


;; move between different windows
(use-package windmove
  :bind
  (("C-c C-w j"    . windmove-up)
   ("C-c C-w k"  . windmove-down)
   ("C-c C-w h"  . windmove-left)
   ("C-c C-w l" . windmove-right))
  )

;; move buffer between windows
(use-package buffer-move
  :ensure t
  :bind (("C-c C-b j"    . buf-move-up)
         ("C-c C-b k"  . buf-move-down)
         ("C-c C-b h"  . buf-move-left)
         ("C-c C-b l" . buf-move-right))
  )



;; By default, C-x C-b runs the list-buffers command.
;; This command lists your buffers in another window.
;; Since I almost always want to do something in that window,
;; I prefer the buffer-menu command, which not only lists the buffers,
;; but moves point into that window.
(use-package buffer-menu
  :bind
  (("C-x C-b" . buffer-menu))
  )

;;
;; The next step is specific to OS X,
;; where an Emacs instance started from the GUI will have a different environment
;; than a shell in a terminal window,
;; because OS X does not run a shell during the login.
;; Obviously this will lead to unexpected results
;; when calling external utilities like make from Emacs.
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config (exec-path-from-shell-initialize)
  )


(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config (global-undo-tree-mode)
  :bind ("s-/" . undo-tree-visualize))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-mode t)
  (setq projectile-use-git-grep t)
  :bind (("s-f" . projectile-find-file)
     ("s-F" . projectile-grep))
  )

;; Magit mode
(use-package magit
  :ensure t
  :pin melpa-stable
  :bind
  (("C-x g" . magit-status))
  )


(use-package ido
  :config
  (ido-mode 1)
  (ido-everywhere t)
  (use-package flx-ido
    :pin melpa-stable
    :ensure t
    :config
    (flx-ido-mode 1)
    )
  ;; disable ido faces to see flx highlights
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil)

  ;; get ido-style completion for function
  (use-package ido-completing-read+
    :pin melpa-stable
    :ensure t
    )
  (use-package ido-ubiquitous
    :pin melpa-stable
    :ensure t
    :config
    (ido-ubiquitous-mode t)
    )
  )

;; Smex (Smart M-X) implements IDO functionality for the M-X window.
(use-package smex
  :ensure t
  :bind (("C-x C-m" . smex)
         ("C-x M-m" . smex-major-mode-commands)
         ("M-x" . smex-major-mode-commands))
  :init
  (unbind-key "<menu>")
  (smex-initialize)
  )

(use-package ace-jump-mode
  :ensure t
  :bind
  (("C-c SPC" . ace-jump-mode))
  )

(use-package highlight-indentation
  :ensure t
  :disabled t
  :config
  (add-hook 'prog-mode-hook 'highlight-indentation-mode)
  (set-face-background 'highlight-indentation-face "#e3e3d3")
  (set-face-background 'highlight-indentation-current-column-face "#c3b3b3")
  )

(use-package expand-region
  :ensure t
  :bind
  (("C-=" . er/expand-region))
  )

(use-package fic-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'fic-mode)
  )

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  ;; :bind
  ;; (("C-M-f" . sp-forward-sexp)
  ;;  ("C-M-b" . sp-backward-sexp)
  ;;  ("C-M-d" . sp-down-sexp)
  ;;  ("C-M-a" . sp-backward-down-sexp)
  ;;  ("C-S-a" . sp-beginning-of-sexp)
  ;;  ("C-S-d" . sp-end-of-sexp)
  ;;  ("C-M-e" . sp-up-sexp)
  ;;  ("C-M-u" . sp-backward-up-sexp)
  ;;  ("C-M-t" . sp-transpose-sexp)
  ;;  ("C-M-n" . sp-next-sexp)
  ;;  ("C-M-p" . sp-previous-sexp)
  ;;  ("C-M-k" . sp-kill-sexp)
  ;;  ("C-M-w" . sp-copy-sexp)
  ;;  ("M-<delete>" . sp-unwrap-sexp)
  ;;  ("M-S-<backspace>" . sp-backward-unwrap-sexp)
  ;;  ("C-<right>" . sp-forward-slurp-sexp)
  ;;  ("C-<left>" . sp-forward-barf-sexp)
  ;;  ("C-M-<left>" . sp-backward-slurp-sexp)
  ;;  ("C-M-<right>" . sp-backward-barf-sexp)
  ;;  ("M-D" . sp-splice-sexp)
  ;;  ("C-M-<delete>" . sp-splice-sexp-killing-forward)
  ;;  ("C-M-<backspace>" . sp-splice-sexp-killing-backward)
  ;;  ("C-M-S-<backspace>" . sp-splice-sexp-killing-around)
  ;;  ("C-]" . sp-select-next-thing-exchange)
  ;;  ("C-<left_bracket>" . sp-select-previous-thing)
  ;;  ("C-M-]" . sp-select-next-thing)
  ;;  ("M-F" . sp-forward-symbol)
  ;;  ("M-B" . sp-backward-symbol)
  ;;  ("H-t" . sp-prefix-tag-object)
  ;;  ("H-p" . sp-prefix-pair-object)
  ;;  ("H-s c" . sp-convolute-sexp)
  ;;  ("H-s a" . sp-absorb-sexp)
  ;;  ("H-s e" . sp-emit-sexp)
  ;;  ("H-s p" . sp-add-to-previous-sexp)
  ;;  ("H-s n" . sp-add-to-next-sexp)
  ;;  ("H-s j" . sp-join-sexp)
  ;;  ("H-s s" . sp-split-sexp)
  ;;  ("M-9" . sp-backward-sexp)
  ;;  ("M-0" . sp-forward-sexp))
  :init
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (use-package smartparens-config)
  ;; TODO: find out
  ;; (bind-key "s" 'smartparens-mode toggle-map)
  (when (eq system-type 'darwin)
    (bind-keys ("<s-right>" . sp-forward-slurp-sexp)
               ("<s-left>" . sp-forward-barf-sexp)))
  ;; (sp-with-modes '(markdown-mode gfm-mode)
  ;;   (sp-local-pair "*" "*"))
  (sp-with-modes '(org-mode)
    (sp-local-pair "=" "=")
    (sp-local-pair "*" "*")
    (sp-local-pair "/" "/")
    (sp-local-pair "_" "_")
    (sp-local-pair "+" "+")
    (sp-local-pair "<" ">")
    (sp-local-pair "[" "]"))
  (use-package rainbow-delimiters
    :ensure t
    :init
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

(use-package company
  :ensure t
  :pin melpa-stable
  :diminish company-mode
  :bind ("C-." . company-complete)
  :init
  (global-company-mode 1)
  :config
  (use-package company-quickhelp
    :ensure t)
  (add-hook 'company-mode 'company-quickhelp-mode)
  (bind-keys :map company-active-map
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)
             ("C-d" . company-show-doc-buffer)
             ("<tab>" . company-complete)
             ("M-h" . company-quickhelp-manual-begin)))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init
  (global-flycheck-mode)
  (setq flycheck-indication-mode 'right-fringe)
  (setq flycheck-check-syntax-automatically '(save new-line mode-enabled))
  :config
  (use-package flycheck-pos-tip
    :init
    (flycheck-pos-tip-mode)
    :ensure t
    )
  )

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init
  (yas-global-mode 1)
  )

;; comment-line do what i mean
;; Original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
(use-package newcomment
  :init
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
  :bind
  (("M-;" . comment-dwim-line))
  )
;; (use-package comment-dwim-2
;;   :ensure t
;;   :bind ("M-;" . comment-dwim-2))

;; markdown mode
(use-package markdown-mode
  :ensure t
  :mode (("\\.markdown$" . markdown-mode)
     ("\\.md$" . markdown-mode))
  )

;; C mode
;; TODO

;; Ruby Mode
(use-package ruby-mode
  :ensure t
  :mode (("\\.rb$" . ruby-mode)
         ("[vV]agrantfile$" . ruby-mode)
         ("[gG]emfile$" . ruby-mode)
         ("[pP]uppetfile$" . ruby-mode)
         ("\\.rake$" . ruby-mode)
         ("\\.rabl$" . ruby-mode)
         ("Procfile$" . enh-ruby-mode)
         ("[cC]apfile$" . ruby-mode)
         ("\\.gemspec$" . ruby-mode)
         ("\\.builder$" . ruby-mode))
  :config
  (setq ruby-align-to-stmt-keywords '(begin if while unless until case for def))
  (setq ruby-insert-encoding-magic-comment nil)
  (setq ruby-align-chained-calls nil)
  (setq ruby-deep-indent-paren nil)
  (setq ruby-deep-indent-paren-style nil)
  (setq ruby-use-smie t)
  (defadvice ruby-indent-line (after unindent-closing-paren activate)
    "Indent sole parenthesis in loca's way."
    (let ((column (current-column))
          indent offset)
      (save-excursion
        (back-to-indentation)
        (let ((state (syntax-ppss)))
          (setq offset (- column (current-column)))
          (when (and (eq (char-after) ?\))
                     (not (zerop (car state))))
            (goto-char (cadr state))
            (setq indent (current-indentation)))))
      (when indent
        (indent-line-to indent)
        (when (> offset 0) (forward-char offset)))))
  (use-package yard-mode
    :ensure t)
  (yard-turn-on)
  (use-package inf-ruby
    :ensure t
    )
  (use-package robe
    :ensure t
    :init
    (add-hook 'ruby-mode-hook 'robe-mode)
    )
  (use-package ruby-tools
    :ensure t
    :config
    (add-hook 'ruby-mode-hook 'ruby-tools-mode)
    )
  (use-package company
    :ensure t
    :config
    (push 'company-robe company-backends)
    )
  )

;; erlang config
(use-package erlang
  :ensure t
  :init
  (flycheck-define-checker erlang-otp
    "An Erlang syntax checker using the Erlang interpreter."
    :modes erlang-mode
    :command ("erlc" "-o" temporary-directory "-Wall"
              "-I" "../include" "-I" "../../include"
              "-I" "../../../include" source)
    :error-patterns
    ((warning line-start (file-name) ":" line ": Warning:" (message) line-end)
     (error line-start (file-name)   ":" line ": "         (message) line-end)))
  (add-hook 'erlang-mode-hook (lambda ()
                                (require 'erlang-start)
                                (flycheck-select-checker 'erlang-otp)
                                (flycheck-mode)))
  )

;; elixir config
(use-package elixir-mode
  :ensure t
  :init
  (add-hook 'elixir-mode-hook 'company-mode)
  :config
  (use-package alchemist
    :ensure t)
  (sp-with-modes '(elixir-mode)
    (sp-local-pair
     "fn" "end"
     :when '(("SPC" "RET"))
     :actions '(insert navigate))
    (sp-local-pair
     "do" "end"
     :when '(("SPC" "RET"))
     :actions '(insert navigate)))
  )

;; Rust Config
(use-package rust-mode
  :ensure t
  :mode (("\\.rs$" . rust-mode))
  :init
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  (add-hook 'rust-mode-hook 'racer-mode)
  :config
  (use-package cargo
    :ensure t
    )
  (use-package racer
    :ensure t
    :init
    (add-hook 'racer-mode-hook 'eldoc-mode)
    (add-hook 'racer-mode-hook 'company-mode)
    :config
    (setq racer-rust-src-path (expand-file-name "rust/src" user-emacs-directory))
    (racer-turn-on-eldoc)
    (setq company-tooltip-align-annotations t)
    )
  ;; (add-hook 'racer-mode-hook #'company-mode)
  )

;; Scala Config
(use-package scala-mode
  :interpreter ("scala" . scala-mode)
  )

(use-package ensime
  :ensure t
  :pin melpa-stable
  :config
  )

;; Jenkinsfile use groovy
(use-package groovy-mode
  :ensure t
  :mode (("Jenkinsfile" . groovy-mode))
  )

(use-package lua-mode
  :ensure t
  :mode (("\\.lua$" . lua-mode))
  :interpreter ("lua" . lua-mode)
  )

;; Dockerfile mode
(use-package dockerfile-mode
  :ensure t
  :pin melpa-stable
  )

;; fish shell script mode
(use-package fish-mode
  :ensure t
  )

;; yaml
(use-package yaml-mode
  :ensure t)

;; toml
(use-package toml-mode
  :ensure t)

(use-package autorevert
  :diminish auto-revert-mode)

;; just for fun
(use-package xkcd
  :ensure t)

(use-package graphviz-dot-mode
  :ensure t
  )

;; See http://emacs-fu.blogspot.nl/2013/03/editing-with-root-privileges-once-more.html
(defun find-file-as-root ()
  "Like `ido-find-file, but automatically edit the file with
root-privileges (using tramp/sudo), if the file is not writable by
user."
  (interactive)
  (let ((file (ido-read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))

(bind-key "C-x F" 'find-file-as-root)




;; Quotes
;; https://github.com/mwfogleman/config/blob/master/home/.emacs.d/michael.org

(provide 'init)
;;; init.el ends here
