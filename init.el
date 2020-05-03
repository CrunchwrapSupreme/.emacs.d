(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay 0)
 '(feature-default-language "fi" t)
 '(feature-step-search-gems-path "gems/ruby/*/gems/*/**/*steps.rb" t)
 '(feature-step-search-path "features/**/*steps.rb" t)
 '(helm-M-x-fuzzy-match t t)
 '(helm-autoresize-max-height 20)
 '(helm-autoresize-min-height 15)
 '(helm-autoresize-mode t)
 '(package-selected-packages
   (quote
    (moody helm-company persp-projectile use-package solarized-theme magit enh-ruby-mode helm-projectile cargo rust-mode rtags projectile-rails projectile company ruby-end robe)))
 '(projectile-completion-system (quote helm)))

(eval-when-compile
  (require 'use-package))
;; (require 'use-package-ensure)
;; (setq use-package-always-ensure t)

(load "global")
(load "ruby-config")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
