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

(defcustom company-symbol-after-symbol-maximum-word-length 80
  "Maximum length of words to save. This may be useful to avoid
  long meaningless words (like base64 string) to be saved."
  :group 'company-symbol-after-symbol
  :type 'integer)

(defcustom company-symbol-after-symbol-minimum-current-buffer-occurrences 1
  "How many times (at least) candidates from the current-buffer
must appear. When 2 for an example, \"baz\" is suggested after
\"foo bar \", if the sequence \"foo bar baz\" appears more than
twice in the same buffer."
  :group 'company-symbol-after-symbol
  :type 'integer)

(defcustom company-symbol-after-symbol-minimum-other-buffers-occurrences 2
  "How many times (at least) candidates from other-buffers must
  appear in the same-mode buffers. When 2 for an example, \"baz\"
  is suggested after \"foo bar \", if the sequence \"foo bar
  baz\" appears more than twice in the same-mode buffers. Note
  that the count is NOT carried over across
  sessions. i.e. candidates must appear more than twice in a
  single session (not once in a session and once in another
  session). Once the condition is met, candidates are saved in
  `company-symbol-after-symbol-history-file' for next sessions."
  :group 'company-symbol-after-symbol
  :type 'integer)

(defcustom company-symbol-after-symbol-history-store-limit (* 7 24 60 60)
  "How long (in seconds) candidates should be stored in the
  history file after last seen. When 7 days for an example,
  candidates are deleted if they are unseen for more than 7
  days."
  :group 'company-symbol-after-symbol
  :type 'number)

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

(defun company-symbol-after-symbol-filter-by-occurrences (sorted-list threshold)
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

;; ---- completion-tree

;; a completion-tree is a:
;; - cons[occurrences, radix-tree[symbol, completion-tree]] (level 1 or 2)
;; - cons[occurrences, timestamp] (level 3)

(defconst company-symbol-after-symbol-session-start-time (float-time))

(defun company-symbol-after-symbol-tree-empty ()
  "Allocate an empty completion-tree."
  (cons 0 nil))

(defun company-symbol-after-symbol-tree-insert (tree keys &optional n timestamp)
  "Insert an item to a completion-tree destructively. When N is
specified, repeat N times. Current time is recorded unless
TIMESTAMP is specified."
  (cl-incf (car tree) (or n 1))
  (cond (keys
         (let ((child (radix-tree-lookup (cdr tree) (car keys))))
           (unless child
             (setq child (company-symbol-after-symbol-tree-empty))
             (setcdr tree (radix-tree-insert (cdr tree) (car keys) child)))
           (company-symbol-after-symbol-tree-insert child (cdr keys) n timestamp)))
        (t
         (unless (and timestamp (cdr tree) (> (cdr tree) timestamp))
           (setcdr tree (or timestamp company-symbol-after-symbol-session-start-time))))))

(defun company-symbol-after-symbol-tree-search (tree keys &optional threshold)
  "Search through a completion-tree with KEYS."
  (when (consp tree)
    (if keys
        (company-symbol-after-symbol-tree-search
         (radix-tree-lookup (cdr tree) (car keys)) (cdr keys) threshold)
      (let (candidates)
        (radix-tree-iter-mappings
         (cdr tree)
         (lambda (k v) (when (<= (or threshold 0) (car v)) (push k candidates))))
        candidates))))

(defun company-symbol-after-symbol-tree-to-alist (tree)
  "Transform tree to an alist of the form ((TIMESTAMP OCCURRENCES . KEYS)
  ...)."
  (let (lst)
    (radix-tree-iter-mappings
     (cdr tree)
     (lambda (k v)
       (cond ((numberp (cdr v))         ; leaf
              (push (cons (cdr v) (cons (car v) (list k))) lst))
             (t                         ; node
              (setq lst
                    (nconc lst
                           (mapcar (lambda (item) (push k (cddr item)) item)
                                   (company-symbol-after-symbol-tree-to-alist v))))))))
    lst))

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
      (setcar line "")
      (while (cddr line)
        (when (and (<= (length (car line)) company-symbol-after-symbol-maximum-word-length)
                   (<= (length (cadr line)) company-symbol-after-symbol-maximum-word-length)
                   (<= (length (caddr line)) company-symbol-after-symbol-maximum-word-length))
         (push (list (car line) (cadr line) (caddr line)) candidates))
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

;; ---- save / load cache

(defun company-symbol-after-symbol-cache-to-history-v2 (cache)
  ;; alist[mode -> alist[time -> list[keys]]]
  (let ((limit (- (float-time) company-symbol-after-symbol-history-store-limit))
        res)
    (dolist (mode (hash-table-keys cache))
      (let ((items (company-symbol-after-symbol-tree-to-alist (gethash mode cache)))
            (hash-by-time (make-hash-table :test 'eql)))
        (dolist (item items)
          (cl-destructuring-bind (timestamp occurrences . keys) item
            (when (and (<= limit timestamp)
                       (<= company-symbol-after-symbol-minimum-other-buffers-occurrences occurrences)
                       ;; drop candidates saved before the option
                       ;; `company-symbol-after-symbol-maximum-word-length` is
                       ;; introduced.
                       (<= (length (car keys)) company-symbol-after-symbol-maximum-word-length)
                       (<= (length (cadr keys)) company-symbol-after-symbol-maximum-word-length)
                       (<= (length (caddr keys)) company-symbol-after-symbol-maximum-word-length))
              (push (cddr item) (gethash (car item) hash-by-time)))))
        (let (time-list)
          (maphash (lambda (time items) (push (cons time items) time-list)) hash-by-time)
          (when time-list
            (push (cons mode time-list) res)))))
    res))

(defun company-symbol-after-symbol-cache-from-history-v1 (_)
  (make-hash-table :test 'eq))

(defun company-symbol-after-symbol-cache-from-history-v2 (data)
  (let ((cache (make-hash-table :test 'eq)))
    (dolist (mode-data data)
      (let ((tree (company-symbol-after-symbol-tree-empty)))
        (dolist (time-data (cdr mode-data))
          (dolist (item (cdr time-data))
            (company-symbol-after-symbol-tree-insert
             tree item
             company-symbol-after-symbol-minimum-other-buffers-occurrences (car time-data))))
        (puthash (car mode-data) tree cache)))
    cache))

(defun company-symbol-after-symbol-history-save ()
  (when company-symbol-after-symbol-history-file
    (company-symbol-after-symbol-update-cache-other-buffers)
    (company-symbol-after-symbol-update-cache (current-buffer))
    (let ((data (company-symbol-after-symbol-cache-to-history-v2
                 company-symbol-after-symbol-cache))
          (enable-local-variables nil))
      (with-temp-buffer
        (prin1 (cons 2 data) (current-buffer))
        (write-file company-symbol-after-symbol-history-file)))))

(defun company-symbol-after-symbol-history-load ()
  (when (and company-symbol-after-symbol-history-file
             (file-exists-p company-symbol-after-symbol-history-file))
    (let ((data (with-temp-buffer
                  (insert-file-contents company-symbol-after-symbol-history-file)
                  (read (current-buffer)))))
      (cl-case (car data)
        (1 (setq company-symbol-after-symbol-cache
                 (company-symbol-after-symbol-cache-from-history-v1 (cdr data))))
        (2 (setq company-symbol-after-symbol-cache
                 (company-symbol-after-symbol-cache-from-history-v2 (cdr data))))
        (t (error "unknown history file version"))))))

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
     company-symbol-after-symbol-minimum-current-buffer-occurrences)))

(defun company-symbol-after-symbol-search-other-buffer-candidates (prefix1 &optional prefix2)
  (company-symbol-after-symbol-update-cache-other-buffers)
  (let ((tree (gethash major-mode company-symbol-after-symbol-cache nil)))
    (when tree
      (mapcar (lambda (s)
                (string-match "^.+\\_>" s) ; drop suffix
                (concat (or prefix1 "") (or prefix2 "") (match-string 0 s))) ; concat prefix
              (company-symbol-after-symbol-tree-search
               tree (cons (or prefix1 "") (if prefix2 (list prefix2) nil))
               company-symbol-after-symbol-minimum-other-buffers-occurrences)))))

(defun company-symbol-after-symbol-all-completions (prefix1 &optional prefix2)
  "Get all completions for given prefixes. If only PREFIX1 is
given, try to find 2-gram candidates. Otherwise try to find
3-gram candidates. PREFIX1 can be nil for 3-gram completion,
which implies the BOL."
  (nconc (company-symbol-after-symbol-search-current-buffer-candidates prefix1 prefix2)
         (company-symbol-after-symbol-search-other-buffer-candidates prefix1 prefix2)))

(defun company-symbol-after-symbol-initialize ()
  (add-hook 'after-change-functions 'company-symbol-after-symbol-invalidate-cache)
  (add-hook 'kill-buffer-hook 'company-symbol-after-symbol-update-cache)
  (add-hook 'kill-emacs-hook 'company-symbol-after-symbol-history-save)
  (company-symbol-after-symbol-history-load)
  nil)

(provide 'company-symbol-after-symbol-core)
