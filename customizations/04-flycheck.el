;;; package --- summary
;;; Commentary:
;;; configuration of flycheck mode
;;; Code:
(add-hook 'after-init-hook 'global-flycheck-mode)
(add-hook 'global-flycheck-mode-hook
          (lambda ()
            (custom-set-variables
             '(flycheck-display-error-delay 0.2)
             '(flycheck-check-syntax-automatically (quote (save new-line mode-enabled))))

            (flycheck-pos-tip-mode)))

(provide '04-flycheck)
;;; 04-flycheck ends here
