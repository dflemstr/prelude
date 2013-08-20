;;; personal-terminal.el --- Personal: Sets up terminal navigation.

;;; Commentary:

;; Some xterms have horrible compatibility issues.  Here we try to fix
;; that.

;;; Code:

(defun personal-terminal-configure (&optional frame)
  "Manage function `xterm-mouse-mode' for current terminal (FRAME is ignored)."
  (if (not frame)
      (xterm-mouse-mode 1)
    (if xterm-mouse-mode
        (xterm-mouse-mode 1))))

(personal-terminal-configure)
(add-hook 'after-make-frame-functions 'personal-terminal-configure)

(provide 'personal-terminal)
;;; personal-terminal.el ends here
