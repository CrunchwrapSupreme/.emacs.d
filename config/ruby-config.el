(add-to-list 'load-path "~/.emacs.d/Enhanced-Ruby-Mode")
(add-to-list 'load-path "~/.emacs.d/rvm.el")
(add-to-list 'load-path "~/.emacs.d/yard-mode.el")
(add-to-list 'load-path "~/.emacs.d/robe")

;; Yard Mode
(use-package yard-mode
  :hook enh-ruby-mode)

;; Robe
(use-package robe
  :bind ("C-c r r" . inf-ruby)
  :hook enh-ruby-mode
  :hook (enh-ruby-mode . (lambda ()
              (setq tab-width 4)
              (robe-mode)
              (if (not is-robe-running)
                  (progn (robe-start t) (setq is-robe-running t)))))
  :hook (after-save . (lambda()
                        (when (eq major-mode 'enh-ruby-mode) (ruby-load-file))))
  :custom
  (is-robe-running nil)
  :config
  (eval-after-load 'company '(push 'company-robe company-backends))
  (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
    (rvm-activate-corresponding-ruby)))

;; Feature Mode
(use-package feature-mode
  :mode ("\.feature$" . feature-mode)
  :custom
  (feature-default-language "fi")
  (feature-step-search-path "features/**/*steps.rb")
  (feature-step-search-gems-path "gems/ruby/*/gems/*/**/*steps.rb")
  :config
  (add-hook 'feature-mode-hook
            (lambda ()
              (setq tab-width 2))))

;; RVM Mode
(use-package rvm
  :config
  (rvm-use-default))

;; Enh Ruby Mode
(use-package enh-ruby-mode
  :mode ("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode)
  :interpreter ("ruby" . enh-ruby-mode)
  :init
  (autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
  :config
  (remove-hook 'enh-ruby-mode-hook 'erm-define-faces))
