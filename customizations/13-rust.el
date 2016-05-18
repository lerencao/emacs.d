;;; pacakge --- summary
;;; Commentary:
;; rust mode config

;;; Code:
(add-hook 'rust-mode-hook 'racer-mode)
(add-hook 'racer-mode-hook (lambda ()
                             (setq racer-cmd (concat (getenv "HOME") "/.cargo/bin/racer"))
                             (setq racer-rust-src-path (concat (getenv "HOME") "/.dev/rust/rust/src"))
                             ))

(add-hook 'racer-mode-hook 'eldoc-mode)
(add-hook 'racer-mode-hook '(lambda ()
                              (company-mode)
                              (local-set-key (kbd "TAB") 'company-indent-or-complete-common)
                              ))
;;; 12-rust.el ends here
