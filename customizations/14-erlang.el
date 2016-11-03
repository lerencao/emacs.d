(require 'flycheck)
(require 'erlang-start)

;; (flycheck-define-checker erlang-otp
;;   "An Erlang syntax checker using the Erlang interpreter with OTP."
;;   :command ("erlc" "-o" temporary-directory "-Wall"
;;             "-I" "../include" "-I" "../../include"
;;             "-I" "../../../include" source)
;;   :error-patterns ((warning line-start
;;                             (file-name)
;;                             ":"
;;                             line
;;                             ": Warning:"
;;                             (message)
;;                             line-end)
;;                    (error line-start
;;                           (file-name)
;;                           ":"
;;                           line
;;                           ": "
;;                           (message)
;;                           line-end))
;;   :modes erlang-mode)


;; (setq load-path (cons "" load-path))
;; (setq erlang-root-dir "")

;; TODO: better way to set the dir?
(setq erlang-root-dir "~/.asdf/installs/erlang/19.0/lib/erlang")

(flycheck-define-checker erlang-otp
  "An Erlang syntax checker using the Erlang interpreter."
  :modes erlang-mode
  :command ("erlc" "-o" temporary-directory "-Wall"
            "-I" "../include" "-I" "../../include"
            "-I" "../../../include" source)
  :error-patterns
  ((warning line-start (file-name) ":" line ": Warning:" (message) line-end)
   (error line-start (file-name)   ":" line ": "         (message) line-end)))

(add-hook 'erlang-mode-hook
          (lambda ()
            (flycheck-select-checker 'erlang-otp)
            (flycheck-mode)))
;; (add-hook 'erlang-mode-hook
;;           (lambda ()
;;             (setq flycheck-erlang-include-path
;;                   (list "./include"
;;                         "../include"
;;                         "../../include"
;;                         "../../../include"))
;;             (setq flycheck-erlang-library-path
;;                   (list "./ebin"))))
