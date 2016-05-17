;;; personal-whitespace.el --- Personal: Configures whitespace-mode

;;; Commentary:

;; Sets up nicer-looking characters and better semantics for
;; whitespace-mode

;;; Code:

(require 'whitespace)

(setq whitespace-display-mappings
      '((space-mark 32
                    [183]
                    [46])
        (space-mark 160
                    [9085]
                    [164]
                    [95])
        (space-mark 2208
                    [65533]
                    [2212])
        (space-mark 2336
                    [2340])
        (space-mark 3616
                    [3620])
        (space-mark 3872
                    [3876])
        (newline-mark 10
                      [8629 10]
                      [182 10]
                      [36 10])
        (tab-mark 9
                  [8677 9]
                  [9225 9]
                  [187 9]
                  [92 9])))

(setq whitespace-style
      '(face trailing tabs spaces lines-tail newline empty
             indentation space-after-tab space-before-tab
             space-mark tab-mark newline-mark))

;; Many styles set ugly backgrounds for 'whitespace-space
(set-face-background 'whitespace-space nil)

;; This make it so that whitespace-line-column can be customized as a
;; local variable
(add-hook
 'hack-local-variables-hook
 (lambda ()
   (if (or whitespace-mode global-whitespace-mode)
       (progn
         (whitespace-mode 0)
         (whitespace-mode 1)))))

(setq whitespace-line-column nil)
(put 'whitespace-line-column 'safe-local-variable #'integerp)

(provide 'personal-whitespace)
;;; personal-whitespace.el ends here
