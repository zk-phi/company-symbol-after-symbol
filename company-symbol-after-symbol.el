;; (defvar-local company-symbol-after-symbol-table nil)
;;
;; (defun company-symbol-after-symbol-add-symbol-to-table (before after)
;;   (let ((lst (radix-tree-lookup company-symbol-after-symbol-table before)))
;;     (if lst
;;         (push after (cdr lst))
;;       (setq company-symbol-after-symbol-table
;;             (radix-tree-insert company-symbol-after-symbol-table before (list t after))))))
;;
;; (defun company-symbol-after-symbol-get-candidates (before)
;;   (cdr (radix-tree-lookup company-symbol-after-symbol-table before)))
;;
;; (defun company-symbol-after-symbol-update-cache ()
;;   (save-excursion
;;     (goto-char (point-min))
;;     (let (lst)
;;       (while (search-forward-regexp "\\(\\_<.+?\\_>.*?\\)\\(\\_<.+?\\_>\\)" nil t)
;;         (company-symbol-after-symbol-add-symbol-to-table (match-string 1) (match-string 2))
;;         (goto-char (match-end 1)))
;;       lst)))

(require 'cl-lib)

(defun company-symbol-after-symbol-search-candidates (prefix &optional cursor)
  (let ((regex (concat (regexp-quote prefix) "\\(\\_<.+?\\_>\\)"))
        lst)
    (when cursor
      (save-excursion
        (goto-char cursor)
        (while (search-backward-regexp regex nil t)
          (push (match-string-no-properties 0) lst))))
    (save-excursion
      (goto-char (or cursor (point-min)))
      (while (search-forward-regexp regex nil t)
        (push (match-string-no-properties 0) lst)))
    lst))

(defun company-symbol-after-symbol (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-symbol-after-symbol))
    (prefix (and (derived-mode-p 'prog-mode)
                 (not (looking-back "\\_>" (1- (point))))
                 (looking-back "\\_<.+?\\_>.+?" (point-at-bol))
                 (match-string 0)))
    (duplicates t)
    (candidates (company-symbol-after-symbol-search-candidates company-prefix (point)))))

(provide 'company-symbol-after-symbol)
