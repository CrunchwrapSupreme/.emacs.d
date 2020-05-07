(add-to-list 'load-path "~/.emacs.d/emacs-async")
(add-to-list 'load-path "~/.emacs.d/popup-el")
(add-to-list 'load-path "~/.emacs.d/helm")
(add-to-list 'load-path "~/.emacs.d/moody")

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
(setq-default tab-width 4)
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(setq split-height-threshold 160)
(setq split-width-threshold 80)

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

;; (defun magit-display-buffer-traditional (buffer)
;;   "Display BUFFER the way this has traditionally been done."
;;   (display-buffer
;;    buffer (if (and (derived-mode-p 'magit-mode)
;;                    (not (memq (with-current-buffer buffer major-mode)
;;                               '(magit-process-mode
;;                                 magit-revision-mode
;;                                 magit-diff-mode
;;                                 magit-stash-mode
;;                                 magit-status-mode))))
;;               '(display-buffer-same-window)
;;             nil))) ; display in another window
