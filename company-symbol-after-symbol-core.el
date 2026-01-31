(require 'cl-lib)
(require 'radix-tree)

(defgroup company-symbol-after-symbol nil
  "Simple-minded omni completion engine for company."
  :group 'company-symbol-after-symbol)

(defcustom company-symbol-after-symbol-fallback-to-2gram t
  "When non-nil, try to find 2-gram candidates, if no 3-gram
  candidates found."
  :group 'company-symbol-after-symbol
  :type 'boolean)

(defcustom company-symbol-after-symbol-history-file nil
  "When non-nil, save history to a file in order to share
  completion candidates across sessions."
  :group 'company-symbol-after-symbol
  :type 'string)

(defcustom company-symbol-after-symbol-minimum-word-length 1
  "Minimum length of the words to be saved."
  :group 'company-symbol-after-symbol
  :type 'integer)

(defcustom company-symbol-after-symbol-maximum-word-length 80
  "Maximum length of words to save. This may be useful to avoid
  long meaningless words (like base64 string) to be saved."
  :group 'company-symbol-after-symbol
  :type 'integer)

(defcustom company-symbol-after-symbol-history-store-capacity 1000
  "How many 3-grams should be stored in the history file for each
 major modes."
  :group 'company-symbol-after-symbol
  :type 'number)

(make-obsolete-variable
 'company-symbol-after-symbol-history-store-limit
 "company-symbol-after-symbol-history-store-capacity"
 "2026-01-31")

;; ---- utils

(defun complete-symbol-after-symbol--maphash (fn table)
  (when table
    (let (res)
      (maphash (lambda (k v) (push (funcall fn k v) res)) table)
      res)))

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

;; ---- completion-tree

;; a completion-tree is a:
;; - radix-tree[segment -> cons[t, completion-tree]]
;; where the first-level SEGMENT may be an empty string that indicates a BOL.

(defun company-symbol-after-symbol-tree-insert (tree keys)
  "Insert an item to a completion-tree destructively. KEYS can be a
list of 3 segments (where 1st element can be an empty string that
indicates a BOL)."
  (when keys
    (let ((child (radix-tree-lookup tree (car keys))))
      (unless child
        (setq child (cons t nil))
        (setq tree (radix-tree-insert tree (car keys) child)))
      (setcdr child (company-symbol-after-symbol-tree-insert (cdr child) (cdr keys)))))
  tree)

(defun company-symbol-after-symbol-tree-search (tree keys)
  "Search through a completion-tree with KEYS. KEYS can be a list of
one or two strings."
  (if keys
      (company-symbol-after-symbol-tree-search
       (cdr (radix-tree-lookup tree (car keys)))
       (cdr keys))
    (let (candidates)
      (radix-tree-iter-mappings tree (lambda (k v) (push k candidates)))
      candidates)))

(defun company-symbol-after-symbol-tree-to-list (tree)
  "Transform completion-tree to list of 3-grams."
  (let (lst)
    (radix-tree-iter-mappings
     tree
     (lambda (k v)
       (cond ((cdr v)                   ; node
              (setq lst
                    (nconc lst
                           (mapcar (lambda (child) (cons k child))
                                   (company-symbol-after-symbol-tree-to-list (cdr v))))))
             (t                         ; leaf
              (push (list k) lst)))))
    lst))

;; ---- cache

;; hash[mode -> hash[file -> (modified-p . completion-tree)]]
(defvar company-symbol-after-symbol--cache (make-hash-table :test 'eq))
(defvar-local company-symbol-after-symbol-cache-is-dirty t)
(defvar-local company-symbol-after-symbol--buffer-modified nil)

(defun company-symbol-after-symbol--cache-get-tree (mode file)
  (let ((tbl (or (gethash mode company-symbol-after-symbol--cache)
                 (puthash mode (make-hash-table :test 'equal) company-symbol-after-symbol--cache))))
    (cdr (gethash file tbl))))

(defun company-symbol-after-symbol--cache-update-tree (mode file modified-p tree)
  (let ((tbl (or (gethash mode company-symbol-after-symbol--cache)
                 (puthash mode (make-hash-table :test 'equal) company-symbol-after-symbol--cache))))
    (puthash file (cons modified-p tree) tbl)))

(defun company-symbol-after-symbol-get-all-current-buffer-3grams ()
  "Get list of all possible 3-grams of the form (SYMBOL1 SYMBOL2
SYMBOL3) in the buffer. Note that each symbols may combined with
suffix punctuation characters, like \"foo (\" for an example."
  (let* ((lines (mapcar
                 (lambda (line) (split-string line "\\_<"))
                 (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n")))
         (symb (concat "\\(?:\\sw\\|\\s_\\)\\{"
                       (number-to-string company-symbol-after-symbol-minimum-word-length)
                       ","
                       (number-to-string company-symbol-after-symbol-maximum-word-length)
                       "\\}\\_>"))
         (regex-segment (concat "^\\(?:" symb "\\|$\\)"))
         (regex-segment-nobol (concat "^" symb))
         candidates)
    (dolist (line lines)
      ;; replace the first element (which is a non-symbol string at the BOL) with an empty string
      (setcar line "")
      (while (cddr line)
        (when (and (string-match regex-segment (car line))
                   (string-match regex-segment-nobol (cadr line))
                   (string-match regex-segment-nobol (caddr line)))
          (push (list (car line) (cadr line) (caddr line)) candidates))
        (setq line (cdr line))))
    candidates))

(defun company-symbol-after-symbol-update-cache (&optional buffer)
  "Put all symbols in the buffer into
`company-symbol-after-symbol--cache'."
  (when (and buffer-file-name
             company-symbol-after-symbol-cache-is-dirty
             (or (derived-mode-p 'prog-mode) (derived-mode-p 'text-mode)))
    (with-current-buffer (or buffer (current-buffer))
      (let ((tree (company-symbol-after-symbol--cache-get-tree major-mode buffer-file-name))
            (items (company-symbol-after-symbol-get-all-current-buffer-3grams)))
        (dolist (item items)
          (setq tree (company-symbol-after-symbol-tree-insert tree item)))
        (company-symbol-after-symbol--cache-update-tree
         major-mode buffer-file-name
         company-symbol-after-symbol--buffer-modified tree)
        (setq company-symbol-after-symbol-cache-is-dirty nil)))))

(defun company-symbol-after-symbol-invalidate-cache (&rest _)
  (setq company-symbol-after-symbol-cache-is-dirty t
        company-symbol-after-symbol--buffer-modified t))

(defun company-symbol-after-symbol-update-cache-other-buffers ()
  "Update cache for all buffers except for the current buffer."
  (dolist (buf (buffer-list))
    (unless (eq buf (current-buffer))
      (company-symbol-after-symbol-update-cache buf))))

;; ---- interface

(defun company-symbol-after-symbol-search-current-buffer-candidates (prefix1 prefix2)
  "Get 3-gram candidates from the current-buffer with regex
sesarch. If PREFIX2 is nil, fallback to 2-gram
candidates. PREFIX1 can be nil for 3-gram completion, which
implies the BOL."
  (let ((query
         (concat
          (if prefix1 "" "^\\W*")
          "\\(" (regexp-quote (or prefix1 "")) (regexp-quote (or prefix2 "")) "\\_<.+?\\_>\\)")))
    (company-symbol-after-symbol-search-regex query 1 (point))))

(defun company-symbol-after-symbol-search-other-buffer-candidates (prefix1 prefix2)
  "Get 3-gram candidates from the completion-table. If PREFIX2 is
nil, fallback to 2-gram candidates. PREFIX1 can be nil for 3-gram
completion, which implies the BOL."
  (company-symbol-after-symbol-update-cache-other-buffers)
  (let ((candidates
         (apply 'nconc
                (complete-symbol-after-symbol--maphash
                 (lambda (file entry)         ; entry is a (modified-p . completion-tree)
                   (company-symbol-after-symbol-tree-search
                    (cdr entry) (cons (or prefix1 "") (if prefix2 (list prefix2) nil))))
                 (gethash major-mode company-symbol-after-symbol--cache)))))
    (mapcar (lambda (s)
              (string-match "^.+\\_>" s) ; drop suffix
              (concat (or prefix1 "") (or prefix2 "") (match-string 0 s))) ; concat prefix
            candidates)))

(defun company-symbol-after-symbol-all-completions (prefix1 prefix2)
  "Get all completions for given prefixes. If only PREFIX1 is
given, try to find 2-gram candidates. Otherwise try to find
3-gram candidates. PREFIX1 can be nil for 3-gram completion,
which implies the BOL."
  (let ((all-candidates
         (nconc
          (company-symbol-after-symbol-search-current-buffer-candidates prefix1 prefix2)
          (company-symbol-after-symbol-search-other-buffer-candidates prefix1 prefix2))))
    (delete-dups (sort all-candidates 'string<))))

;; ---- save and load

(defun company-symbol-after-symbol--make-save-data-v3 (previous-data)
  ;; alist[mode -> list[keys]]
  (let ((table (make-hash-table :test 'eq))) ; table[mode -> table[keys -> (count . write-flag)]]
    (maphash (lambda (mode file-table)
               (let ((3grams (or (gethash major-mode table)
                                 (puthash major-mode (make-hash-table :test 'equal) table))))
                 (maphash
                  (lambda (path entry)
                    (mapcar
                     (lambda (3gram)
                       (let* ((oldvalue (or (gethash 3gram 3grams) '(0 . nil)))
                              (count (1+ (car oldvalue)))
                              (write-flag (or (cdr oldvalue) (car entry))))
                         (puthash 3gram (cons count write-flag) 3grams)))
                     (company-symbol-after-symbol-tree-to-list (cdr entry))))
                  file-table)))
             company-symbol-after-symbol--cache)
    (let ((new-data
           (complete-symbol-after-symbol--maphash
            (lambda (mode 3grams)
              (let* ((3gram-list (complete-symbol-after-symbol--maphash
                                  (lambda (3gram value)
                                    (and (>= (car value) 2)
                                         (cdr value)
                                         3gram))
                                  3grams))
                     (filtered (delq nil 3gram-list)))
                (cons mode filtered)))
            table)))
      ;; merge with previous-data
      (dolist (mode previous-data)
        (let ((concatenated (nconc (alist-get (car mode) new-data) (cdr mode))))
          (delete-dups concatenated)
          (ignore-errors
            (setf (nthcdr company-symbol-after-symbol-history-store-capacity concatenated) nil))
          (setf (alist-get (car mode) new-data) concatenated)))
      new-data)))

(defun company-symbol-after-symbol--load-saved-data-v3 (data)
  ;; alist[mode -> list[keys]]
  (dolist (mode-data data)
    (let ((tree (company-symbol-after-symbol--cache-get-tree (car mode-data) nil)))
      (dolist (item (cdr mode-data))
        (setq tree (company-symbol-after-symbol-tree-insert tree item)))
      (company-symbol-after-symbol--cache-update-tree (car mode-data) nil nil tree))))

(defun complete-symbol-after-symbol--upgrade-history-v2-to-v3 (data)
  ;; v2: alist[mode -> alist[time -> list[keys]]]
  ;; v3: alist[mode -> sorted-unique-list[keys]]
  (mapcar (lambda (mode-data)
            (cons (car mode-data) (apply 'nconc (mapcar 'cdr (cdr mode-data)))))
          data))

(defun company-symbol-after-symbol--maybe-read-history-file ()
  (when (and company-symbol-after-symbol-history-file
             (file-exists-p company-symbol-after-symbol-history-file))
    (with-temp-buffer
      (insert-file-contents company-symbol-after-symbol-history-file)
      (read (current-buffer)))))

(defun company-symbol-after-symbol--parse-history-file-v3 ()
  (let ((data (company-symbol-after-symbol--maybe-read-history-file)))
    (when data
      (cl-case (car data)
        (3 (cdr data))
        (2 (complete-symbol-after-symbol--upgrade-history-v2-to-v3 (cdr data)))
        (t (error "unsupported history file version"))))))

(defun company-symbol-after-symbol-history-save ()
  (when company-symbol-after-symbol-history-file
    (company-symbol-after-symbol-update-cache-other-buffers)
    (company-symbol-after-symbol-update-cache (current-buffer))
    (let ((data (cons 3
                      (company-symbol-after-symbol--make-save-data-v3
                       (company-symbol-after-symbol--parse-history-file-v3))))
          (enable-local-variables nil))
      (with-temp-buffer
        (prin1 data (current-buffer))
        (write-file company-symbol-after-symbol-history-file)))))

(defun company-symbol-after-symbol-history-load ()
  (company-symbol-after-symbol--load-saved-data-v3
   (company-symbol-after-symbol--parse-history-file-v3)))

(defun company-symbol-after-symbol-initialize ()
  (add-hook 'after-change-functions 'company-symbol-after-symbol-invalidate-cache)
  (add-hook 'kill-buffer-hook 'company-symbol-after-symbol-update-cache)
  (add-hook 'kill-emacs-hook 'company-symbol-after-symbol-history-save)
  (company-symbol-after-symbol-history-load)
  nil)

(provide 'company-symbol-after-symbol-core)
