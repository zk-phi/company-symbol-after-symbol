(require 'company)
(require 'company-symbol-after-symbol-core)

(defcustom company-symbol-after-symbol-continue-commands
  '(company-complete-common
    company-complete-common-or-cycle
    company-indent-or-complete-common
    company-dwim)
  "List of commands after which completion should be continued."
  :group 'company-symbol-after-symbol
  :type '(list symbol))

(defvar company-symbol-after-symbol--candidates nil)

(defun company-symbol-after-symbol (command &optional _ &rest __)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-symbol-after-symbol))
    (prefix
     (and (or
           ;; if completion is already started, continue completion as long as
           ;; the current command is listed in "continue-commands"
           (and (memq this-command company-symbol-after-symbol-continue-commands)
                company-symbol-after-symbol--candidates)
           ;; otherwise, start completion iff the point is NOT immediately after a symbol
           ;; (at least one non-symbol character is required to start completion)
           (not (looking-back "\\(\\sw\\|\\s_\\)" (point-at-bol))))
          (or
           ;; capture two (one if at BOL) symbols and search 3-gram candidates
           (and (looking-back
                 ;; (?:BOL    |  (prefix1          )  )  (prefix2          )  (current-symb )
                 "\\(?:^\\W*\\|\\(\\_<.+?\\_>\\W*\\)\\)\\(\\_<.+?\\_>\\W*\\)\\(\\sw\\|\\s_\\)*"
                 (point-at-bol))
                (or company-symbol-after-symbol--candidates
                    (save-match-data
                      (setq company-symbol-after-symbol--candidates
                            (company-symbol-after-symbol-all-completions
                             (match-string 1) (match-string 2)))))
                (concat (match-string 1) (match-string 2) (match-string 3)))
           (and
            ;; capture one symbol and search 2-gram candidates
            company-symbol-after-symbol-fallback-to-2gram
            (looking-back
             ;; (prefix1          )
             "\\(\\_<.+?\\_>\\W*\\)\\(?:\\sw\\|\\s_\\)*"
             (point-at-bol))
            (or company-symbol-after-symbol--candidates
                (save-match-data
                  (setq company-symbol-after-symbol--candidates
                        (company-symbol-after-symbol-all-completions (match-string 1) nil))))
            (match-string 0)))))
    (duplicates t)
    (candidates company-symbol-after-symbol--candidates)))

(defun company-symbol-after-symbol-finished (&optional _)
  (setq company-symbol-after-symbol--candidates nil))

(add-hook 'company-after-completion-hook 'company-symbol-after-symbol-finished)

(provide 'company-symbol-after-symbol)
