;;; pacakge --- summary
;;; Commentary:
;;; javascript editing config

;;; Code:


; add company mode hook
(add-hook 'javascript-mode-hook 'company-mode)
;; (eval-after-load 'company '(push 'company-tern company-backends))
(add-to-list 'company-backends 'company-tern)
;;; ends here
