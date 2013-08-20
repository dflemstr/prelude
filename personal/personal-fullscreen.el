;;; personal-fullscreen.el --- Personal: Adds fullscreen toggling support

;;; Commentary:

;; Makes it possible to toggle full-screen with F11

;;; Code:

(defvar personal-fullscreen-old nil)

(defun personal-fullscreen-toggle ()
  "Toggle fullscreen for the active frame."
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter
     nil 'fullscreen
     (if (equal 'fullboth current-value)
         personal-fullscreen-old
       (progn (setq personal-fullscreen-old current-value)
              'fullboth)))))

(global-set-key [f11] 'personal-fullscreen-toggle)

(provide 'personal-fullscreen)
;;; personal-fullscreen.el ends here
