(use-package flycheck
  :defer t
  :diminish flycheck-mode
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (setq-default flycheck-check-syntax-automatically '(mode-enabled save))
  ;; disable documentation related emacs lisp checker
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc clojure-cider-typed))
  (setq flycheck-mode-line-prefix "✔"))

(use-package rjsx-mode
  :mode ("\\.js\\'"
         "\\.jsx\\'")
  :config
  (setq js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        js2-basic-offset 2
        js-indent-level 2)
  (setq-local flycheck-disabled-checkers (cl-union flycheck-disabled-checkers
                                                   '(javascript-jshint))) ; jshint doesn't work for JSX
  (electric-pair-mode 1))

(use-package typescript-mode
  :mode ("\\.ts\\'")
  :config
  (setq typescript-indent-level 2))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save))
  ;; :config
  ;; (setq flycheck-check-syntax-automatically '(save mode-enabled))
  )

(use-package add-node-modules-path
  :defer t
  :hook (((rjsx-mode typescript-mode) . add-node-modules-path)))

(use-package prettier-js
  :defer t
  :diminish prettier-js-mode
  :hook (((rjsx-mode typescript-mode) . prettier-js-mode))
  :init)

(use-package lsp-mode
  :defer t
  :diminish lsp-mode
  :hook (((rjsx-mode) . lsp))
  :commands lsp
  :config
  (setq lsp-auto-configure t
        lsp-auto-guess-root t
        ;; don't set flymake or lsp-ui so the default linter doesn't get trampled
        lsp-diagnostics-package :none))  ; (j)ump (b)ack to marker

(use-package company-lsp
  :defer t
  :config
  (setq company-lsp-cache-candidates 'auto
        company-lsp-async t
        company-lsp-enable-snippet nil
        company-lsp-enable-recompletion t))

(use-package lsp-ui
  :defer t
  :config
  (setq lsp-ui-sideline-enable t
        ;; disable flycheck setup so default linter isn't trampled
        lsp-ui-flycheck-enable nil
        lsp-ui-sideline-show-symbol nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-peek-enable nil
        lsp-ui-imenu-enable nil
        lsp-ui-doc-enable nil))
