;;; pacakge --- summary
;;; Commentary:
;; ruby mode config

;;; Code:
;; (autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)

;; (add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
;; (add-to-list 'auto-mode-alist '("\\.rake$" . enh-ruby-mode))

;; (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

;;; enables outlining for ruby
;;; You may also want to bind hide-body, hide-subtree, show-substree
;;; show-all, show-children, ... to some keys easy folding and unfolding
(add-hook 'ruby-mode-hook
          '(lambda ()
             (outline-minor-mode)
             (setq outline-regexp
                   " *\\(def \\|class\\|module\\|describe \\|it \\)")
             (yard-mode)
             (eldoc-mode)
             (robe-mode)
             (company-mode)
             (push 'company-robe company-backends)))

;;; 10-ruby.el ends here
