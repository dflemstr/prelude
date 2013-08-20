;;; personal-org.el --- Personal: Configure org-mode

;;; Commentary:

;; These settings basically set up org-mode for HTML exports and basic
;; tangling.

;;; Code:

(require 'org)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((css . t)
   (emacs-lisp . t)
   (haskell . t)
   (js . t)
   (org . t)
   (sh . t)
   (sql . t)))

(provide 'personal-org)
;;; personal-org.el ends here
