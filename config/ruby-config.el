(add-to-list 'load-path "~/.emacs.d/Enhanced-Ruby-Mode")
(add-to-list 'load-path "~/.emacs.d/rvm.el")
(add-to-list 'load-path "~/.emacs.d/yard-mode.el")
(add-to-list 'load-path "~/.emacs.d/robe")

;; RVM Mode
(use-package rvm
  :config
  (rvm-use-default))

;; Yard Mode
(use-package yard-mode
  :disabled
  :hook enh-ruby-mode)

;; Rubocop Linting
(use-package rubocop)


;; Bundler
(use-package bundler
  :disabled
  :hook enh-ruby-mode)

;; Ruby-test
(use-package ruby-test
  :disabled
  :hook enh-ruby-mode)

(use-package rake
  :disabled
  :config
  (setq rake-completion-system 'helm)
  (define-key enh-ruby-mode-map (kbd "C-!") 'rake))

;; (defun projectile-after-switch-robe()
;;   (message "Attempting to start robe ruby server")
;;   (robe-mode)
;;   (unless robe-running
;;     (robe-start t)))

;; Robe
;;Starts robe on enh-ruby-mode hook
;;Reloads enh-ruby-mode files
(use-package robe
  :disabled
  :bind ("C-c r r" . inf-ruby)
  :hook (enh-ruby-mode . robe-mode)
  :hook (after-save . (lambda()
                        (when (eq major-mode 'enh-ruby-mode) (ruby-load-file buffer-file-name))))
  :config
  (advice-add 'inf-ruby-console-auto :before #'rvm-activate-corresponding-ruby)
  (eval-after-load 'company '(push 'company-robe company-backends)))

;; Feature Mode
(use-package feature-mode
  :mode ("\.feature$" . feature-mode)
  :custom
  (feature-default-language "fi")
  (feature-step-search-path "features/**/*steps.rb")
  (feature-step-search-gems-path "gems/ruby/*/gems/*/**/*steps.rb")
  (feature-use-docker-compose nil)
  (feature-default-language "en")
  (feature-use-rvm t)
  :config
  (add-hook 'feature-mode-hook
            (lambda ()
              (setq-default tab-width 2)
              (setq-default indent-tabs-mode nil))))

;; Enh Ruby Mode
(use-package enh-ruby-mode
  :mode ("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode)
  :interpreter ("ruby" . enh-ruby-mode)
  :init
  (autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
  :config
  ;; (projectile-register-project-type 'cucumber '("features")
  ;;                                   :compile "rubocop --auto-correct-safe"
  ;;                                   :test-suffix ".feature"
  ;;                                   :test "cucumber -p headless-staging"
  ;;                                   :test-dir "features")

  ;;(add-hook 'enh-ruby-mode-hook 'erm-define-faces)
  ;;(remove-hook 'enh-ruby-mode-hook 'erm-define-faces)
  (add-hook 'enh-ruby-mode-hook
            (lambda ()
              (setq tab-width 4))))

(defun stop-yardoc-server()
  "Stop the yard documentation server"
  (interactive)
  (if (get-buffer "*yard server*")
      (progn
        (message "Stopping yard server on *yard server*...")
        (delete-process "*yard server*")
        (kill-buffer "*yard server*"))))

(defun start-yardoc-server()
  "Start the yard server --gem documentation server"
  (interactive)
  (stop-yardoc-server)
  (if (executable-find "yard")
      (progn
        (start-process "yardserver" "*yard server*" "yard" "server" "--gems")
        (with-current-buffer (get-buffer "*yard server*")
          (read-only-mode t))
        (message "Starting yard server on *yard server*..."))
    (warn "Could not find yard in path")))

(defun stop-cuke-run()
  "Stop the current cucumber process if running and/or removes buffer if it exists"
  (interactive)
  (if (get-buffer "*cucumber run*")
      (progn
        (message "If started, stopping yard server on *yard server*...")
        (if (get-process "cuketest") (delete-process "cuketest"))
        (kill-buffer "*cucumber run*"))))

(defun buffer-lino-to-str()
  (interactive)
  (concat buffer-file-name ":" (number-to-string (line-number-at-pos))))

(defun cuke-run-to-log ()
  (interactive)
  (with-current-buffer (get-buffer-create "*Cucumber Log*")`
    (flush-lines "Process .* finished")
    (if (get-buffer "*cucumber run*")
        (read-only-mode nil)
        (insert-buffer-substring "*cucumber run*"))))

(require 'ansi-color)
(defun run-cuke-f()
  "Run feature at line"
  (interactive)
  (when (and (derived-mode-p 'feature-mode) (not (get-process "cuketest")))
    ;; (cuke-run-to-log)
    (let ((default-directory (projectile-project-root)))
      (start-process "cuketest" "*cucumber run*" "cucumber" "-p" "headless-qa" (buffer-lino-to-str)))
    (display-buffer-below-selected (get-buffer "*cucumber run*") '((window-height . 30)))
    (with-current-buffer (get-buffer "*cucumber run*")
      (read-only-mode nil)
      (erase-buffer)
      (read-only-mode t)
      (ansi-color-for-comint-mode-on)
      (comint-mode)
      (set-process-filter (get-process "cuketest") 'comint-output-filter))))
