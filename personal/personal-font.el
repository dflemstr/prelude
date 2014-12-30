;;; personal-font.el --- Personal: Chooses a suitable font for new frames

;;; Commentary:

;; Some fonts are simply better than others.  This file finds the best
;; font that is actually available on the system for a specific frame.

;;; Code:

(require 'cl)

(defun personal-font-candidate (fonts &optional frame)
  "Return the first available font in FONTS that can be shown in FRAME."
  (find-if (lambda (f) (find-font (font-spec :name f) frame)) fonts))

(defvar personal-font-preferences
  '("Fantasque Sans Mono"
    "PragmataPro"
    "Inconsolata"
    "DejaVu Sans Mono"
    "Bitstream Vera Sans Mono"
    "Anonymous Pro"
    "Menlo"
    "Consolas"))

(defun personal-font-set-preferred (&optional frame)
  "Set the default font in FRAME to the preferred font."
  (let ((preferred-font
         (personal-font-candidate personal-font-preferences frame)))
    (if preferred-font
        (set-frame-font preferred-font nil (if frame (list frame))))))

(add-hook 'after-make-frame-functions 'personal-font-set-preferred)

(provide 'personal-font)
;;; personal-font.el ends here
