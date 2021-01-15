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

(defcustom company-symbol-after-symbol-complete-after-space nil
  "When non-nil, complete spacce-delimited symbols. Otherwise, at
least one non-space character is required to start completion."
  :group 'company-symbol-after-symbol
  :type 'boolean)

(defcustom company-symbol-after-symbol-same-buffer-threshold 0.1
  "Threshold to filter search results in the current buffer. When
 0.05 for example, which is the defualt value, completion
 candidates which occupy less than 5% among the results are
 dropped."
  :group 'company-symbol-after-symbol
  :type 'number)

(defcustom company-symbol-after-symbol-continue-commands
  '(company-complete-common
    company-complete-common-or-cycle
    company-indent-or-complete-common
    company-dwim)
  "List of commands after which completion should be continued."
  :group 'company-symbol-after-symbol
  :type '(list symbol))

;; ---- search through buffer

(defun company-symbol-after-symbol-search-regex (regex &optional subexp cursor)
  "Search all occurrences of REGEX through the buffer. If CURSOR
is specified, search before/after the point separately."
  (let (lst)
    (when cursor
      (save-excursion
        (goto-char cursor)
        (while (search-backward-regexp regex nil t)
          (push (match-string-no-properties (or subexp 0)) lst))))
    (save-excursion
      (goto-char (or cursor (point-min)))
      (while (search-forward-regexp regex nil t)
        (push (match-string-no-properties (or subexp 0)) lst)))
    lst))

;; ---- filter candidates

(defun company-symbol-after-symbol-filter-by-occurrences (sorted-list threshold)
  (when sorted-list
    (setq threshold (* threshold (length sorted-list)))
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

;; ---- interface

(defvar company-symbol-after-symbol--candidates nil)
(defvar company-symbol-after-symbol--bolp nil)

(defun company-symbol-after-symbol (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-symbol-after-symbol))
    (prefix
     (when (or
            ;; if completion is already started, continue completion as long as
            ;; the current command is listed in "continue-commands"
            (and (memq this-command company-symbol-after-symbol-continue-commands)
                 company-symbol-after-symbol--candidates)
            ;; otherwise, start completion iff the point is NOT immediately after a symbol
            ;; (at least one non-symbol character is required to start completion)
            (not (looking-back
                  (if company-symbol-after-symbol-complete-after-space
                      "\\(\\sw\\|\\s_\\)"
                    "\\(\\sw\\|\\s_\\)[\s\t]*")
                  (point-at-bol))))
       ;; capture at most two symbols before the cursor
       ;; (except for a currently-completing symbol)
       (cond ((looking-back "\\_<.+?\\_>[^\n]+?\\_<.+?\\_>[^\n]+?\\(\\sw\\|\\s_\\)*" (point-at-bol))
              (setq company-symbol-after-symbol--bolp nil)
              (match-string 0))
             ((looking-back "\\_<.+?\\_>[^\n]+?\\(\\sw\\|\\s_\\)*" (point-at-bol))
              (setq company-symbol-after-symbol--bolp t)
              (match-string 0)))))
    (duplicates t)
    (candidates
     (or company-symbol-after-symbol--candidates
         (let ((candidates
                (company-symbol-after-symbol-search-regex
                 (concat (and company-symbol-after-symbol--bolp "^\\W*")
                         "\\(" (regexp-quote company-prefix) "[\s\t]*\\_<.+?\\_>\\)")
                 1
                 (point))))
           (setq company-symbol-after-symbol--candidates
                 (company-symbol-after-symbol-filter-by-occurrences
                  (sort candidates 'string<) company-symbol-after-symbol-same-buffer-threshold)))))))

(defun company-symbol-after-symbol-finished (&optional _)
  (setq company-symbol-after-symbol--candidates nil))

(add-hook 'company-after-completion-hook 'company-symbol-after-symbol-finished)

(provide 'company-symbol-after-symbol)
