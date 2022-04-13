;; (use-package rust-mode
;;   :mode "\\.rs\\'"
;;   :config
;;   (setq tab-width 2)
;;   (projectile-register-project-type 'rust-lang '("Cargo.toml")
;;                                     :compile "cargo build"
;;                                     :test "cargo test"
;;                                     :run "cargo run"
;;                                     :test-dir "tests/"))

(use-package rustic
  :config
  (setq tab-width 2)
  (projectile-register-project-type 'rust-lang '("Cargo.toml")
                                    :compile "cargo build"
                                    :test "cargo test"
                                    :run "cargo run"
                                    :test-dir "tests/"))
;; (with-eval-after-load 'rust-mode
;;   (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
