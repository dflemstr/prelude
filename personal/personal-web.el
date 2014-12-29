;;; personal-web.el --- Personal: some web-mode defaults

;;; Commentary:

;; Mostly just changes indentation and stuff like that.

;;; Code:

(defun personal-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-style-padding 2)
  (setq web-mode-script-padding 2))

(eval-after-load 'web-mode
  '(add-hook 'web-mode-hook  #'personal-web-mode-hook))

(provide 'personal-web)
;;; personal-web.el ends here
