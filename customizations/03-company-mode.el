;;; package --- summary
;;; Commentary:

;;; Code:

(add-hook 'prog-mode-hook 'company-mode)
(add-hook 'company-mode 'company-quickhelp-mode)

(eval-after-load 'company
  '(define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin))

;;; 03-company-mode.el ends here
