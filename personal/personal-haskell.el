;;; personal-haskell.el --- Personal: Configures haskell-mode

;;; Commentary:

;; Adds nice features for dealing with Haskell files

;;; Code:

(defun personal-haskell-joindirs (root &rest segments)
  "Starting from the ROOT directory, navigate to each path segment in SEGMENTS \
in order and return the resulting path."
  (if segments
      (apply 'personal-haskell-joindirs
             (expand-file-name (car segments) root)
             (cdr segments))
    root))

(defun personal-haskell-find-cabal-file (file)
  "Find the Cabal project file for the specified Haskell source FILE."
  (let* ((parent (file-name-directory file))
         (cabal-files (directory-files parent nil "..*\\.cabal" t)))
    (cond (cabal-files (expand-file-name (car cabal-files) parent))
          ((equal file parent) nil)
          (t (personal-haskell-find-cabal-file (directory-file-name parent))))))

(defun personal-haskell-packages-from-macros-file (file)
  "Extract the list of packages declared in a Cabal macros FILE."
  (when (file-exists-p file)
    (with-temp-buffer
      (goto-char 1)
      (insert-file-contents file nil nil nil t)
      (let (start end result)
        (while (setq start (search-forward "/* package " nil t))
          (when (search-forward " */" nil t)
            (setq end (- (point) 3))
            (push (buffer-substring-no-properties start end) result)))
        (reverse result)))))

(defun personal-haskell-ghc-args-from-cabal-file (file)
  "Return a list of options for GHC from a Cabal FILE."
  (when (file-exists-p file)
    (with-temp-buffer
      (goto-char 1)
      (insert-file-contents file nil nil nil t)
      (let (start end result
            (case-fold-search t)
            (project-dir (file-name-directory file)))
        (while
            (setq start
                  (search-forward-regexp "hs-source-dirs[\t\n ]*:" nil t))
          (setq end
                (if (search-forward-regexp "[a-zA-Z0-9-]*[\t\n ]*:" nil t)
                    (- (point) (length (match-string 0)))
                  (point-max)))
          (let ((dirs (split-string (buffer-substring-no-properties start end)
                                    "[\t\n ]*,[\t\n ]*")))
            (while dirs
              (let* ((unstripped-dir (pop dirs))
                     (dir (replace-regexp-in-string
                           "\\`[ \t\n]*" ""
                           (replace-regexp-in-string
                            "[ \t\n]*\\'" "" unstripped-dir)))
                     (include-option (concat
                                      "-i"
                                      (expand-file-name dir project-dir))))
                (unless (member include-option result)
                  (push include-option result))))))
        result))))

(defun personal-haskell-ghc-args (file)
  "Return a list of options for GHC to compile the specified Haskell FILE."
  (let* ((project-file (personal-haskell-find-cabal-file file))
         (project-dir (file-name-directory project-file))
         (macros-file
          (personal-haskell-joindirs
           project-dir "dist" "build" "autogen" "cabal_macros.h"))
         (packages (personal-haskell-packages-from-macros-file macros-file))
         (result (personal-haskell-ghc-args-from-cabal-file project-file)))
    (if packages
        (progn (push "-hide-all-packages" result)
               (while packages
                 (let ((package (pop packages)))
                   (push "-package" result)
                   (push package result)))))
    (reverse result)))

(require 'flycheck)

(flycheck-define-checker haskell-ghc-cabal
  "A Haskell syntax and type checker using GHC that is kinda aware of Cabal.

See URL `http://www.haskell.org/ghc/'."

  :command
  ("ghc" "-Wall" "-fno-code"
   (eval (personal-haskell-ghc-args (buffer-file-name)))
   source-inplace)
  :predicate
  (lambda ()
    (when (buffer-file-name)
      (personal-haskell-find-cabal-file (buffer-file-name))))
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ":"
            (or " " "\n    ") "Warning:" (optional "\n")
            (one-or-more " ")
            (message (one-or-more not-newline)
                     (zero-or-more "\n"
                                   (one-or-more " ")
                                   (one-or-more not-newline)))
            line-end)
   (error line-start (file-name) ":" line ":" column ":"
          (or (message (one-or-more not-newline))
              (and "\n" (one-or-more " ")
                   (message (one-or-more not-newline)
                            (zero-or-more "\n"
                                          (one-or-more " ")
                                          (one-or-more not-newline)))))
          line-end))
  :modes haskell-mode
  :next-checkers ((warnings-only . haskell-hlint)))

(add-to-list 'flycheck-checkers 'haskell-ghc-cabal)

(provide 'personal-haskell)
;;; personal-haskell.el ends here
