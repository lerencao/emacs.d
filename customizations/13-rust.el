;;; pacakge --- summary
;;; Commentary:
;; rust mode config

;;; Code:

(add-hook 'rust-mode-hook 'racer-mode)
(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'racer-mode-hook 'eldoc-mode)

(setenv "CARGO_HOME"
        (expand-file-name "~/.cargo"))
(setq racer-rust-src-path
      (expand-file-name "rust/src"
                        (file-name-directory load-file-name)))

(add-hook 'racer-mode-hook '(lambda ()
                              (racer-turn-on-eldoc)
                              (company-mode)
                              (local-set-key (kbd "TAB")
                                             'company-indent-or-complete-common)
                              (setq company-tooltip-align-annotations t)))

;; TODO: git clone rust src when not exists
;; (when (not (file-directory-p (expand-file-name "./rust/src")))
;;   (shell-command-to-string "git clone https://github.com/rust-lang/rust.git"))


(provide '13-rust)
;;; 13-rust.el ends here
