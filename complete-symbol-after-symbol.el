(require 'company-symbol-after-symbol-core)

(defun complete-symbol-after-symbol ()
  ;; otherwise, start completion iff the point is NOT immediately after a symbol
  ;; (at least one non-symbol character is required to start completion)
  (unless (looking-back "\\(\\sw\\|\\s_\\)" (point-at-bol))
    (or
     ;; capture two (one if at BOL) symbols and search 3-gram candidates
     (and (looking-back
           ;; (?:BOL    |  (prefix1          )  )  (prefix2          )
           "\\(?:^\\W*\\|\\(\\_<.+?\\_>\\W*\\)\\)\\(\\_<.+?\\_>\\W*\\)"
           (point-at-bol))
          (list (or (match-beginning 1) (match-beginning 2))
                (point)
                (company-symbol-after-symbol-all-completions
                 (match-string 1) (match-string 2))
                :exclusive 'no
                :annotation-function (lambda (_) " 3-gram")))
     (and
      ;; capture one symbol and search 2-gram candidates
      company-symbol-after-symbol-fallback-to-2gram
      (looking-back
       ;; (prefix1          )
       "\\(\\_<.+?\\_>\\W*\\)"
       (point-at-bol))
      (list (match-beginning 0)
            (point)
            (company-symbol-after-symbol-all-completions (match-string 1) nil)
            :exclusive 'no
            :annotation-function (lambda (_) " 2-gram"))))))

(provide 'complete-symbol-after-symbol)
