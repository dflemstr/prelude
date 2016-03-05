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

(defun personal-font-set-preferred (&rest frame)
  "Set the default font in FRAME to the preferred font."
  (if window-system
      (let* ((f (if (car frame)
                   (car frame)
                 (selected-frame)))
             (preferred-font
              (personal-font-candidate personal-font-preferences f)))
        (if preferred-font
            (progn
              (message "Using font %s" preferred-font)
              (set-frame-font preferred-font nil (if f (list f))))))))

(add-hook 'after-make-frame-functions #'personal-font-set-preferred t)
(personal-font-set-preferred)

(provide 'personal-font)
;;; personal-font.el ends here
