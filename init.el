;;; package --- Summary
;;; Commentary:
;;; init file
;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; TODO:
;; treemacs

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-compute-statistics t)

(require 'bind-key)
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (setq auto-package-update-interval 30)
  (auto-package-update-maybe))

(use-package diminish
  :ensure t
  )

;; maximize frame window
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . fullheight))

(use-package solarized-theme
  :ensure t
  :config (load-theme 'solarized-dark t) (setq x-underline-at-descent-line t)
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
           (cond ((and (>= width 1366) (>= height 768))
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
;; (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; (load custom-file)
(setq-default scroll-preserve-screen-position 1)
(setq-default vc-follow-symlinks nil)


(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)
      (menu-bar-mode -1)))

(show-paren-mode 1)
(global-linum-mode)
(global-hl-line-mode t)

;; display line column along with line number in mini-menu
(column-number-mode t)

;; do not auto line break
;; (turn-off-auto-fill)
(turn-on-auto-fill)
;; truncate lines
(setq-default truncate-lines nil)

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
  (setq-default whitespace-line-column 120
                whitespace-style '(face lines-tail trailing space-after-tab))
  (add-hook 'prog-mode-hook 'whitespace-mode)
  )

(use-package js
  :mode "\\.json$"
  :custom
  (js-indent-level 2))


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
  :demand
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
  :init
  (ido-mode 1)
  (ido-everywhere t)

  :custom
  ;; disable ido faces to see flx highlights
  (ido-enable-flex-matching t)
  (ido-use-faces nil)

  :config
  ;; get ido-style completion for function
  (use-package ido-completing-read+
    :pin melpa-stable
    :ensure t
    )
  (use-package flx-ido
    :pin melpa-stable
    :ensure t
    :config
    (flx-ido-mode 1)
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
  ;; (sp-with-modes '(org-mode)
  ;;   (sp-local-pair "=" "=")
  ;;   (sp-local-pair "*" "*")
  ;;   (sp-local-pair "/" "/")
  ;;   (sp-local-pair "_" "_")
  ;;   (sp-local-pair "+" "+")
  ;;   (sp-local-pair "<" ">")
  ;;   (sp-local-pair "[" "]"))
  (use-package rainbow-delimiters
    :ensure t
    :init
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

(use-package company
  :ensure t
  :pin melpa-stable
  :diminish company-mode
  :hook (after-init . global-company-mode)

  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("C-d" . company-show-doc-buffer)
              ("<tab>" . company-complete)
              :map company-search-map
              ("C-p" . company-select-previous)
              ("C-n" . company-select-next))
  :custom
  (company-idle-delay 0)
  (company-echo-delay 0)
  (company-tooltip-align-annotations t)
  (company-minimum-prefix-length 1))
(use-package company-quickhelp
  :requires company
  :hook (company-mode . company-quickhelp-mode)
  :bind (:map company-active-map
              ("C-c h" . company-quickhelp-manual-begin))
  :custom (company-quickhelp-delay 0.8)
  :ensure t)

(use-package flycheck
  :ensure t
  :hook
  (after-init . global-flycheck-mode)
  (flycheck-mode . flycheck-pos-tip-mode)
  :custom
  (flycheck-indication-mode 'right-fringe)
  (flycheck-check-syntax-automatically '(save new-line mode-enabled))
  :config
  (use-package flycheck-pos-tip
    :ensure t
    :hook
    (flycheck-mode . flycheck-pos-tip-mode)
    )
  )


(use-package lsp-mode
  :ensure t
  :commands lsp
  :config
  (setq lsp-auto-guess-root t
        lsp-prefer-flymake nil)
 )
(use-package lsp-ui
  :ensure t
  :requires lsp-mode flycheck
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable nil
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t
        lsp-ui-peek-enable t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25)
  )
(use-package company-lsp
  :requires company
  :ensure t
  :config
  (push 'company-lsp company-backends)
  ;; Disable client-side cache because the LSP server does a better job.
  (setq company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil)
  )
(use-package yasnippet
  :ensure t
  :hook
  (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all)
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
(use-package cmake-mode
  :ensure t
  )

;; Ruby Mode
(use-package ruby-mode
  :ensure t
  :requires (company)
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
  (push 'company-robe company-backends)
  )

;; erlang config
(use-package erlang
  :ensure t
  :requires (flycheck)
  :mode ("\\.erl$")
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
  :bind (:map erlang-mode-map
              ("M-," . alchemist-goto-jump-back))
  :config
  (add-hook 'erlang-mode-hook (lambda ()
                                (require 'erlang-start)
                                (flycheck-select-checker 'erlang-otp)
                                (flycheck-mode)))
  (add-hook 'erlang-mode-hook (lambda ()
                                (setq-local indent-tabs-mode nil)
                                (setq-local c-basic-offset 4)))
  )

;; elixir config
(use-package elixir-mode
  :ensure t
  :requires company

  ;; TODO: this is flawed, check it later
  :init
  (defun set-elixir-format-arguments ()
    (let ((formatter-exs-dir (locate-dominating-file buffer-file-name ".formatter.exs")))
      (when formatter-exs-dir
        (setq-local elixir-format-arguments
                    (list "--dot-formatter" (concat formatter-exs-dir ".formatter.exs"))))
      ))
  (defun elixir-format-before-save ()
    (add-hook 'before-save-hook 'elixir-format)
    (add-hook 'elixir-format-hook 'set-elixir-format-arguments))

  :hook ((elixir-mode . company-mode)
         (elixir-mode . elixir-format-before-save))

  :config
  (use-package alchemist
    :ensure t
    :config
    (setq alchemist-goto-elixir-source-dir (expand-file-name "sources/elixir" user-emacs-directory))
    (setq alchemist-goto-erlang-source-dir (expand-file-name "sources/erlang-otp" user-emacs-directory))
    )
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


;; golang config
(use-package go-mode
  :ensure t
  :pin melpa-stable
  :commands go-mode
  :mode "\\.go$"
  :config
  (use-package go-eldoc
    :ensure t
    :pin melpa
    :commands go-eldoc-setup
    :config
    (add-hook 'go-mode-hook 'go-eldoc-setup)
    (add-hook 'go-mode-hook 'gofmt-before-save)
    )
  )


;; Rust Config
(use-package rust-mode
  :ensure t
  :mode (("\\.rs$" . rust-mode))
  :hook (rust-mode . lsp)
  :config
  (setq rust-format-on-save t))
(use-package cargo
  :ensure t
  :requires rust-mode
  :hook (rust-mode . cargo-minor-mode))


;; Scala Config
(use-package scala-mode
  :interpreter ("scala" . scala-mode)
  )

(use-package ensime
  :ensure t
  :pin melpa-stable
  :requires (scala-mode)
  :hook (scala-mode . ensime-mode)
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
  :mode (("Dockerfile\\(?:\\..*\\)?\\'" . dockerfile-mode))
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
  :mode ("\\.toml$" . toml-mode)
  :ensure t)

(use-package autorevert
  :diminish auto-revert-mode)

;; just for fun
(use-package xkcd
  :ensure t
  :commands (xkcd)
  :defer
  )

(use-package web-mode
  :ensure t
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html?\\'" . web-mode))
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

(defun find-init-file ()
  "Edit init file in another window."
  (interactive)
  (let ((init-file (expand-file-name "init.el" user-emacs-directory)))
    (find-file init-file)))
(bind-key "C-c I" 'find-init-file)

;; Quotes
;; https://github.com/mwfogleman/config/blob/master/home/.emacs.d/michael.org


;; csv mode to view and edit csv file
(use-package csv-mode
  :ensure t
  :mode (("\\.[Cc][Ss][Vv]\\'" . csv-mode))
  )

;; protobuf mode
(use-package protobuf-mode
  :ensure t
  :pin melpa
  :mode (("\\.proto$" . protobuf-mode) ("\\.proto3$" . protobuf-mode)))

(use-package makey
  :ensure t
  )

(use-package ox-ioslide
  :ensure t
  :requires makey
  :commands (org-ioslide-export-as-html org-ioslide-export-to-html)
  :config
  (require 'ox-ioslide-helper)
  )

(use-package ponylang-mode
  :ensure t
  :mode ("\\.pony$")
  :hook
  (ponylang-mode . (lambda ()
                     (setq-local indent-tabs-mode nil)
                     (setq-local tab-width 2)))
  :config
  (use-package flycheck-pony
    :ensure t)
  (use-package pony-snippets
    :requires (yasnippt)
    :ensure t)
  )

(use-package graphviz-dot-mode
  :ensure t
  :pin melpa
  :mode ("\\.dot$")
  )

(use-package solidity-mode
  :ensure t
  :pin melpa
  :mode "\\.sol$"
  :config
  (use-package company-solidity
    :ensure t
    :requires (company solidity-mode)
    :commands (company-solidity)
    )
  (use-package solidity-flycheck
    :ensure t
    :requires (flycheck solidity-mode)
    :custom
    (solidity-flycheck-solc-checker-active t)
    )
  (push 'company-solidity company-backends))

(provide 'init)
;;; init.el ends here
