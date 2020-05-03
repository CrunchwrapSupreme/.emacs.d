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
 '(is-robe-running nil t)
 '(package-selected-packages
   (quote
    (moody helm-company persp-projectile use-package solarized-theme magit enh-ruby-mode helm-projectile cargo rust-mode rtags projectile-rails projectile company ruby-end robe)))
 '(projectile-completion-system (quote helm)))

(eval-when-compile
  (require 'use-package))

;; (require 'use-package-ensure)
;; (setq use-package-always-ensure t)

(add-to-list 'load-path "~/.emacs.d/emacs-async")
(add-to-list 'load-path "~/.emacs.d/popup-el")
(add-to-list 'load-path "~/.emacs.d/helm")
(add-to-list 'load-path "~/.emacs.d/config")
(add-to-list 'load-path "~/.emacs.d/moody")
(load "ruby-config")

;; Magit Config
(use-package magit
  :bind ("C-x g" . magit-status))

;; Company Mode Config
(use-package company
  :custom
  (company-idle-delay 0)
  :config
  (global-company-mode t))

;; Helm Config
(use-package helm
  :init
  (require 'helm-config)
  :custom
  (helm-autoresize-max-height 20)
  (helm-autoresize-min-height 15)
  (helm-autoresize-mode t)
  (helm-M-x-fuzzy-match t)
  :config
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (helm-mode 1))

;; Projectile + Helm Config
(use-package projectile)
(use-package helm-projectile
  :custom
  (projectile-completion-system 'helm)
  :config
  (projectile-global-mode)
  (helm-projectile-on))

;; Theme Config
(use-package solarized-theme
  :config
  (load-theme 'solarized-light t)
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9")))

(use-package moody
  :custom
  (x-underline-at-descent-line t)
  :config
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

;; (defvar moody-projectile-line
;;   ;;'(:eval (moody-ribbon (substring vc-mode 1) nil 'up))
;;   '(:eval (moody-tab (substring projectile-mode-line-function 1) nil 'up)))
;; (put 'moody-projectile-line 'risky-local-variable t)
;; (make-variable-buffer-local 'moody-projectile-line)

;; (defun moody-replace-projectile-line (&optional reverse)
;;   (interactive "P")
;;   (moody-replace-element '(projectile-mode-line-function projectile-mode-line-function)
;;                          '(projectile-mode-line-function moody-projectile-line)
;;                          reverse))
;; (moody-replace-projectile-line)

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

;; additional config
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(global-linum-mode t)
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))
(setq auto-save-file-name-transforms
      `((".*"  ,(concat user-emacs-directory "transforms"))))
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq version-control t)
(setq backup-by-copying-when-linked t)
(setq auto-save-default t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

(defun kill-other-buffers ()
  "Kill all inactive buffers"
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove-if-not 'buffer-file-name (buffer-list)))))

(defun hook-delete-whitespace ()
  "Delete whitespace at the end of a line"
  (when (or (derived-mode-p 'prog-mode) (derived-mode-p 'feature-mode))
    (delete-trailing-whitespace)))
(add-hook 'after-save-hook 'hook-delete-whitespace)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
