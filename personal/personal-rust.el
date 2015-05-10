;;; personal-rust.el --- Personal: Chooses a suitable rust for new frames

;;; Commentary:

;;; Code:

(require 'prelude-programming)
(prelude-require-packages '(flycheck-rust))

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(provide 'personal-rust)
;;; personal-rust.el ends here
