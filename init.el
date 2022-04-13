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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay 0 nil nil "Customized with use-package company")
 '(helm-M-x-fuzzy-match t nil nil "Customized with use-package helm")
 '(helm-autoresize-max-height 20 nil nil "Customized with use-package helm")
 '(helm-autoresize-min-height 15 nil nil "Customized with use-package helm")
 '(helm-autoresize-mode t nil nil "Customized with use-package helm")
 '(helm-buffers-fuzzy-matching t nil nil "Customized with use-package helm")
 '(helm-imenu-fuzzy-match t t nil "Customized with use-package helm")
 '(helm-recentf-fuzzy-match t nil nil "Customized with use-package helm")
 '(helm-semantic-fuzzy-match t t nil "Customized with use-package helm")
 '(package-selected-packages
   '(elpy yasnippet rustic flycheck-rust helm-rg dockerfile-mode jenkinsfile-mode ranger ag tide feature-mode company-c-headers groovy-mode wgrep-helm helm-ag flycheck lsp-ui company-lsp prettier-js add-node-modules-path typescript-mode rjsx-mode lsp-mode rubocop yaml-mode exec-path-from-shell moody helm-company use-package solarized-theme magit enh-ruby-mode helm-projectile cargo rust-mode rtags projectile company ruby-end robe))
 '(projectile-completion-system 'helm))

(package-initialize)
(eval-when-compile
  (require 'use-package))
;; (require 'use-package-ensure)
;; (setq use-package-always-ensure t)

;; OSX Path Fix
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(setenv "VISUAL" (substitute-in-file-name "emacsclient -s $HOME/emacs.d/daemon/server"))
(setenv "EDITOR" (getenv "VISUAL"))

(add-to-list 'load-path "~/.emacs.d/config")
(load "global")
(load "ruby-config")
(load "javascript")
(load "rust-config")
(load "python-config")

;; (use-package server
;;   :config
;;   (setq server-use-tcp nil)
;;   (setq server-socket-dir (substitute-in-file-name "$HOME/.emacs.d/daemon"))
;;   (setq server-name "server")
;;   (unless (server-running-p)
;;     (server-start)
;;     (message "Emacs daemon via [%s]" (concat server-socket-dir "/" "server"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
