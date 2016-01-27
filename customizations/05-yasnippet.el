;;; 05-yasnippet.el --- config for yasnippet
;;; Commentary:
;;; TODO: why require needed?
;;; Code:
(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook 'yas-minor-mode)
;;; 05-yasnippet.el ends here
