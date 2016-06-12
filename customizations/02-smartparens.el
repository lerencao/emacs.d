(require 'smartparens-config)
(add-hook 'prog-mode-hook 'smartparens-mode)
(add-hook 'minibuffer-setup-hook 'turn-on-smartparens-strict-mode)
