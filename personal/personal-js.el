;;; personal-js.el --- Personal: Configures js-mode

;;; Commentary:

;; Miscellaneous changes to the js-mode defaults.

;;; Code:

(defun personal-js2-mode-hook ()
  (setq js2-basic-offset 2))

(eval-after-load 'js2-mode
  '(add-hook 'js2-mode-hook #'personal-js2-mode-hook))

(provide 'personal-js)
;;; personal-js.el ends here
