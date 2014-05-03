;;; personal-haskell.el --- Personal: Configures haskell-mode

;;; Commentary:

;; Adds nice features for dealing with Haskell files

;;; Code:

(require 'prelude-programming)
(prelude-require-packages '(flycheck-haskell))

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(provide 'personal-haskell)
;;; personal-haskell.el ends here
