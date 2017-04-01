;;; personal-rust.el --- Personal: Chooses a suitable rust for new frames

;;; Commentary:

;;; Code:

(require 'prelude-programming)
(prelude-require-packages '(cargo flycheck-rust racer))

(setq racer-cmd "~/.cargo/bin/racer")
(setq racer-rust-src-path "~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'rust-mode-hook #'cargo-minor-mode)
(add-hook 'racer-mode-hook #'company-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(provide 'personal-rust)
;;; personal-rust.el ends here
