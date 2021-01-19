;;; -*- lexical-binding: t -*-
;; TODO: cache candidates across sessions

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

(defcustom company-symbol-after-symbol-fallback-to-2gram t
  "When non-nil, try to find 2-gram candidates, if no 3-gram
  candidates found."
  :group 'company-symbol-after-symbol
  :type 'boolean)

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

;; ---- completion-tree

;; a completion-tree is:
;; - cons[ocurrences, radix-tree[symbol, completion-tree]]

(defun company-symbol-after-symbol-tree-empty ()
  "Allocate an empty completion-tree."
  (cons 0 nil))

(defun company-symbol-after-symbol-tree-insert (tree keys)
  "Insert item to a completion-tree destructively."
  (cl-incf (car tree))
  (when keys
    (let ((child (radix-tree-lookup (cdr tree) (car keys))))
      (unless child
        (setq child (company-symbol-after-symbol-tree-empty))
        (setcdr tree (radix-tree-insert (cdr tree) (car keys) child)))
      (company-symbol-after-symbol-tree-insert child (cdr keys)))))

(defun company-symbol-after-symbol-tree-search (tree keys &optional threshold)
  "Search through a completion-tree with PREFIX. If THRESHOLD is
specified, filter as like
`company-symbol-after-symbol-filter-by-occurrences'."
  (when tree
   (if keys
       (company-symbol-after-symbol-tree-search
        (radix-tree-lookup (cdr tree) (car keys)) (cdr keys) threshold)
     (setq threshold (* (or threshold 0) (car tree)))
     (let (candidates)
       (radix-tree-iter-mappings
        (cdr tree)
        (lambda (k v) (when (>= (car v) threshold) (push k candidates))))
       candidates))))

;; ---- cache

;; hash[mode -> completion-tree]
(defvar company-symbol-after-symbol-cache (make-hash-table :test 'eq))
(defvar-local company-symbol-after-symbol-cache-is-dirty t)

(defun company-symbol-after-symbol-get-all-current-buffer-candidates ()
  "Get list of all possible (SYMBOL1 SYMBOL2 SYMBOL3)s in the
buffer. Note that each symbols may combined with suffix
character, like \"foo (\" for example."
  (let ((lines (mapcar
                (lambda (line) (split-string line "\\_<"))
                (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n")))
        candidates)
    (dolist (line lines)
      ;; replace the first element (which is a non-symbol string at
      ;; the BOL) with an empty string
      (setcar lines "")
      (while (cddr line)
        (push (list (car line) (cadr line) (caddr line)) candidates)
        (setq line (cdr line))))
    candidates))

(defun company-symbol-after-symbol-update-cache (&optional buffer)
  "Put all symbols in the buffer into
`company-symbol-after-symbol-cache'."
  (with-current-buffer (or buffer (current-buffer))
    (when (and company-symbol-after-symbol-cache-is-dirty
               (or (derived-mode-p 'prog-mode)
                   (derived-mode-p 'text-mode)))
      (let ((tree (gethash major-mode company-symbol-after-symbol-cache nil))
            (items (company-symbol-after-symbol-get-all-current-buffer-candidates)))
        (unless tree
          (setq tree (company-symbol-after-symbol-tree-empty))
          (puthash major-mode tree company-symbol-after-symbol-cache))
        (dolist (item items)
          (company-symbol-after-symbol-tree-insert tree item))
        (setq company-symbol-after-symbol-cache-is-dirty nil)))))

(defun company-symbol-after-symbol-invalidate-cache (&rest _)
  (setq company-symbol-after-symbol-cache-is-dirty t))

(defun company-symbol-after-symbol-update-cache-other-buffers ()
  "Update cache for all buffers except for the current buffer."
  (dolist (buf (buffer-list))
    (unless (eq buf (current-buffer))
      (company-symbol-after-symbol-update-cache buf))))

;; ---- interface

(defun company-symbol-after-symbol-search-current-buffer-candidates (prefix1 &optional prefix2)
  (let ((candidates
         (company-symbol-after-symbol-search-regex
          (concat
           (if prefix1 "" "^\\W*")
           "\\(" (regexp-quote (or prefix1 "")) (regexp-quote (or prefix2 "")) "\\_<.+?\\_>\\)")
          1
          (point))))
    (company-symbol-after-symbol-filter-by-occurrences
     (sort candidates 'string<)
     company-symbol-after-symbol-same-buffer-threshold)))

(defun company-symbol-after-symbol-search-other-buffer-candidates (prefix1 &optional prefix2)
  (company-symbol-after-symbol-update-cache-other-buffers)
  (mapcar (lambda (s)
            (string-match "^.+\\_>" s)  ; drop suffix
            (concat (or prefix1 "") (or prefix2 "") (match-string 0 s))) ; concat prefix
          (company-symbol-after-symbol-tree-search
           (gethash major-mode company-symbol-after-symbol-cache nil)
           (cons (or prefix1 "") (if prefix2 (list prefix2) nil))
           company-symbol-after-symbol-same-buffer-threshold)))

(defun company-symbol-after-symbol-all-completions (prefix1 &optional prefix2)
  "Get all completions for given prefixes. If only PREFIX1 is
given, try to find 2-gram candidates. Otherwise try to find
3-gram candidates. PREFIX1 can be nil for 3-gram completion,
which implies the BOL."
  (nconc (company-symbol-after-symbol-search-current-buffer-candidates prefix1 prefix2)
         (company-symbol-after-symbol-search-other-buffer-candidates prefix1 prefix2)))

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
           (not (looking-back
                 (if company-symbol-after-symbol-complete-after-space
                     "\\(\\sw\\|\\s_\\)"
                   "\\(\\sw\\|\\s_\\)[\s\t]*")
                 (point-at-bol))))
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
                        (company-symbol-after-symbol-all-completions (match-string 1)))))
            (match-string 0)))))
    (duplicates t)
    (candidates company-symbol-after-symbol--candidates)))

(defun company-symbol-after-symbol-finished (&optional _)
  (setq company-symbol-after-symbol--candidates nil))

(add-hook 'company-after-completion-hook 'company-symbol-after-symbol-finished)

(provide 'company-symbol-after-symbol)
