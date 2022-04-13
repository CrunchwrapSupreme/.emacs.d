;; (add-to-list 'load-path "~/.emacs.d/rtags")
;; rtags config
;; (setq rtags-path "~/.emacs.d/rtags/build/bin")
;; (require 'company-rtags)
;; C++/C Config
;; (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
;; (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
;; (global-set-key (kbd "M-.") 'rtags-find-symbol-at-point)
;; (push 'company-rtags company-backends)
;; (setq rtags-completions-enabled t)
;; (define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))
;; (setq rtags-display-result-backend 'helm)
