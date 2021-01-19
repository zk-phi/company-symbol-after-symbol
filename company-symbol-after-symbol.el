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

(defun company-symbol-after-symbol-candidates-tree-empty ()
  "Allocate an empty candidates-tree."
  ;; cons[ocurrences, tree[symb, cons[ocurrences, nil]]]
  (cons 0 nil))

(defun company-symbol-after-symbol-candidates-tree-insert (tree symbol)
  "Insert item to a candidates-tree destructively."
  (let ((cell (radix-tree-lookup (cdr tree) symbol)))
    (if cell
        (cl-incf (car cell))
      (setcdr tree (radix-tree-insert (cdr tree) symbol (cons 1 nil)))))
  (cl-incf (car tree)))

(defun company-symbol-after-symbol-completion-tree-empty ()
  "Allocate an empty completion-tree."
  ;; cons[tree[prefix, candidates-tree], nil]
  (cons nil nil))

(defun company-symbol-after-symbol-completion-tree-insert (tree prefix symbol)
  "Insert item to a completion-tree destructively."
  (let ((candidates-tree (radix-tree-lookup (car tree) prefix)))
    (unless candidates-tree
      (setq candidates-tree (company-symbol-after-symbol-candidates-tree-empty))
      (setcar tree (radix-tree-insert (car tree) prefix candidates-tree)))
    (company-symbol-after-symbol-candidates-tree-insert candidates-tree symbol)))

(defun company-symbol-after-symbol-completion-tree-search (tree prefix &optional threshold)
  "Search through a completion-tree with PREFIX. If THRESHOLD is
specified, filter as like
`company-symbol-after-symbol-filter-by-occurrences'."
  (let ((tree (or (radix-tree-lookup (car tree) prefix) (cons 0 nil)))
        candidates)
    (setq threshold (* (or threshold 0) (car tree)))
    (radix-tree-iter-mappings
     (cdr tree)
     (lambda (k v) (when (>= (car v) threshold) (push (concat prefix k) candidates))))
    candidates))

;; ---- cache

;; hash[mode -> completion-tree]
(defvar company-symbol-after-symbol-cache (make-hash-table :test 'eq))
(defvar-local company-symbol-after-symbol-cache-is-dirty t)

(defun company-symbol-after-symbol-get-all-current-buffer-candidates ()
  "Get list of all possible (PREFIX . COMPLETION) pairs in the
buffer."
  (let ((lines (mapcar
                (lambda (line) (cdr (split-string line "\\_<\\|\\_>")))
                (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n")))
        candidates)
    (dolist (line lines)
      ;; add an empty symbol, so that a symbol after the first symbol
      ;; can be completed with 2-gram
      (setq line (cons "" (cons "" line)))
      (while (cddddr line)
        (cl-destructuring-bind (symbol1 delimiter1 symbol2 delimiter2 suffix . _) line
          (push (cons (concat symbol1 delimiter1 symbol2 delimiter2) suffix) candidates))
        (setq line (cddr line))))
    candidates))

(defun company-symbol-after-symbol-update-cache (&optional buffer)
  "Put all symbols in the buffer into
`company-symbol-after-symbol-cache'."
  (with-current-buffer (or buffer (current-buffer))
    (when (and company-symbol-after-symbol-cache-is-dirty
               (or (derived-mode-p 'prog-mode)
                   (derived-mode-p 'text-mode)))
      (let ((tree (gethash major-mode company-symbol-after-symbol-cache nil))
            (pairs (company-symbol-after-symbol-get-all-current-buffer-candidates)))
        (unless tree
          (setq tree (company-symbol-after-symbol-completion-tree-empty))
          (puthash major-mode tree company-symbol-after-symbol-cache))
        (dolist (pair pairs)
          (company-symbol-after-symbol-completion-tree-insert tree (car pair) (cdr pair)))
        (setq company-symbol-after-symbol-cache-is-dirty nil)))))

(defun company-symbol-after-symbol-invalidate-cache (&rest _)
  (setq company-symbol-after-symbol-cache-is-dirty t))

(defun company-symbol-after-symbol-update-cache-other-buffers ()
  "Update cache for all buffers except for the current buffer."
  (dolist (buf (buffer-list))
    (unless (eq buf (current-buffer))
      (company-symbol-after-symbol-update-cache buf))))

;; ---- interface

(defvar company-symbol-after-symbol--candidates nil)
(defvar company-symbol-after-symbol--bolp nil)

(defun company-symbol-after-symbol-search-current-buffer-candidates (prefix)
  (let ((candidates
         (company-symbol-after-symbol-search-regex
          (concat (and company-symbol-after-symbol--bolp "^\\W*")
                  "\\(" (regexp-quote prefix) "\\_<.+?\\_>\\)")
          1
          (point))))
    (setq company-symbol-after-symbol--candidates
          (company-symbol-after-symbol-filter-by-occurrences
           (sort candidates 'string<)
           company-symbol-after-symbol-same-buffer-threshold))))

(defun company-symbol-after-symbol-search-other-buffer-candidates (prefix)
  (company-symbol-after-symbol-update-cache-other-buffers)
  (company-symbol-after-symbol-completion-tree-search
   (gethash major-mode company-symbol-after-symbol-cache nil)
   prefix
   company-symbol-after-symbol-same-buffer-threshold))

(defun company-symbol-after-symbol-all-completions (prefix)
  (nconc (company-symbol-after-symbol-search-current-buffer-candidates prefix)
         (company-symbol-after-symbol-search-other-buffer-candidates prefix)))

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
       ;; capture at most two (except for the currently-completing
       ;; symbol) symbols before the cursor
       (cond ((looking-back "\\_<.+?\\_>[^\n]+?\\_<.+?\\_>[^\n]+?\\(\\sw\\|\\s_\\)*" (point-at-bol))
              (setq company-symbol-after-symbol--bolp nil)
              (match-string 0))
             ((looking-back "\\_<.+?\\_>[^\n]+?\\(\\sw\\|\\s_\\)*" (point-at-bol))
              (setq company-symbol-after-symbol--bolp t)
              (match-string 0)))))
    (duplicates t)
    (candidates
     (or company-symbol-after-symbol--candidates
         (company-symbol-after-symbol-all-completions company-prefix)))))

(defun company-symbol-after-symbol-finished (&optional _)
  (setq company-symbol-after-symbol--candidates nil))

(add-hook 'company-after-completion-hook 'company-symbol-after-symbol-finished)

(provide 'company-symbol-after-symbol)
