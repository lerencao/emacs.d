;;; package --- summary
;;; Commentary:
;;; configuration of flycheck mode
;;; Code:
(add-hook 'after-init-hook 'global-flycheck-mode)

(custom-set-variables
 '(flycheck-display-error-delay 0.2))

(provide '04-flycheck)
;;; 04-flycheck ends here
