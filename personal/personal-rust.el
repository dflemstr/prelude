;;; personal-rust.el --- Personal: Chooses a suitable rust for new frames

;;; Commentary:

;;; Code:

(require 'prelude-programming)
(prelude-require-packages '(flycheck-rust racer))

(eval-after-load 'rust-mode
  '(add-hook 'rust-mode-hook #'racer-mode))

(eval-after-load 'racer-mode
  '(add-hook 'racer-mode-hook #'company-mode))

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(provide 'personal-rust)
;;; personal-rust.el ends here
