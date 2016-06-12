;;; pacakge --- summary
;;; Commentary:
;;; elixr editing config

;;; Code:


(require 'elixir-mode)
(require 'alchemist)

; add company mode hook
(add-hook 'elixir-mode-hook 'company-mode)
(add-hook 'elixir-mode-hook (lambda ()
                              (sp-with-modes '(elixir-mode)
                                (sp-local-pair "fn" "end"
                                               :when '(("SPC" "RET"))
                                               :actions '(insert navigate))
                                (sp-local-pair "do" "end"
                                               :when  '(("SPC" "RET"))
                                               :actions '(insert navigate)))))

;;; 11-elixir.el ends here
