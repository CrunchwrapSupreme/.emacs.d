(add-to-list 'load-path "~/.emacs.d/emacs-async")
(add-to-list 'load-path "~/.emacs.d/popup-el")
; (add-to-list 'load-path "~/.emacs.d/helm")
(add-to-list 'load-path "~/.emacs.d/moody")

(use-package yaml
  :mode ("\.yml$" . yaml-mode))

;; Magit Config
(use-package magit
  :bind ("C-x g" . magit-status))

(use-package company-c-headers)

;; Company Mode Config
(use-package company
  :custom
  (company-idle-delay 0)
  :bind (:map company-active-map ("<tab>" . company-complete-selection))
  :config
  (add-to-list 'company-backends 'company-c-headers)
  (add-to-list 'company-c-headers-path-system "")
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
  (helm-buffers-fuzzy-matching t)
  (helm-recentf-fuzzy-match t)
  (helm-semantic-fuzzy-match t)
  (helm-imenu-fuzzy-match t)
  :config
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-c h o") 'helm-occur)
  (global-set-key (kbd "C-c h x") 'helm-register)
  (global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
  (helm-mode 1))

;; Projectile + Helm Config
(use-package projectile
  :config
  (setq projectile-enable-caching t)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package helm-projectile
  :config
  (setq projectile-completion-system 'helm)
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
(setq ring-bell-function 'ignore)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
;; (setq split-height-threshold 160)
;; (setq split-width-threshold 80)
(add-hook 'groovy-mode-hook
          (lambda ()
            (c-set-offset 'label 4)))

(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WORKED" "REVIEWED" "|" "DONE" "DELEGATED")))

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
