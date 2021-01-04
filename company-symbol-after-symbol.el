;; TODO: cache candidates across sessions

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

(require 'company)
(require 'cl-lib)

(defgroup company-symbol-after-symbol nil
  "Simple-minded omni completion engine for company."
  :group 'company-symbol-after-symbol)

(defcustom company-symbol-after-symbol-complete-after-space t
  "When non-nil, complete spacce-delimited symbols. Otherwise, at
least one non-space character is required to start completon."
  :group 'company-symbol-after-symbol
  :type 'boolean)

(defcustom company-symbol-after-symbol-same-buffer-occurrence-threshold 2
  "Number of minimum ocurrences when filtering candidates in the
current buffer."
  :group 'company-symbol-after-symbol
  :type 'integer)

(defun company-symbol-after-symbol-search-candidates (prefix &optional cursor)
  (let ((regex (concat (regexp-quote prefix) "[\s\t]*\\(\\_<.+?\\_>\\)"))
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

(defun company-symbol-after-symbol-filter-by-ocurrences (sorted-list threshold)
  (when sorted-list
    (let ((current-count 1) candidates)
      (while sorted-list
        (cond ((and (cadr sorted-list) (string= (car sorted-list) (cadr sorted-list)))
               (pop sorted-list)
               (cl-incf current-count))
              (t
               (if (>= current-count threshold)
                   (push (pop sorted-list) candidates)
                 (pop sorted-list))
               (setq current-count 1))))
      candidates)))

(defvar company-symbol-after-symbol--candidates nil)

(defun company-symbol-after-symbol (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-symbol-after-symbol))
    (prefix
     (and (derived-mode-p 'prog-mode)
          (or company-symbol-after-symbol--candidates
              (not (looking-back
                    (if company-symbol-after-symbol-complete-after-space
                        "\\(\\sw\\|\\s_\\)"
                      "\\(\\sw\\|\\s_\\)[\s\t]*")
                    (point-at-bol))))
          (looking-back "\\_<.+?\\_>[^\n]+?" (point-at-bol))
          (match-string 0)))
    (duplicates t)
    (candidates
     (or company-symbol-after-symbol--candidates
         (let ((candidates (company-symbol-after-symbol-search-candidates company-prefix (point))))
          (setq company-symbol-after-symbol--candidates
                (company-symbol-after-symbol-filter-by-ocurrences
                 (sort candidates 'string<)
                 company-symbol-after-symbol-same-buffer-occurrence-threshold)))))))

(defun company-symbol-after-symbol-finished (&optional _)
  (setq company-symbol-after-symbol--candidates nil))

(add-hook 'company-after-completion-hook 'company-symbol-after-symbol-finished)

(provide 'company-symbol-after-symbol)
