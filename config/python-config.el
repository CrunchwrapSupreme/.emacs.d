(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  (setq elpy-rpc-python-command "python3")
  (setq python-shell-completion-native-enable nil)
  (setq python-shell-interpreter "python3"
        python-shell-interpreter-args "-i"))
