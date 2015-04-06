;; jai-mode.el - very basic jai mode

(require 'cl)
(require 'rx)

(defcustom jai-indent-level 4 "Number of spaces per indent.")

(defconst jai-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    ;; additional symbols
    (modify-syntax-entry ?_ "w" table)

    (modify-syntax-entry ?' "." table)
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?+  "." table)
    (modify-syntax-entry ?-  "." table)
    (modify-syntax-entry ?%  "." table)
    (modify-syntax-entry ?&  "." table)
    (modify-syntax-entry ?|  "." table)
    (modify-syntax-entry ?^  "." table)
    (modify-syntax-entry ?!  "." table)
    (modify-syntax-entry ?$  "/" table)
    (modify-syntax-entry ?=  "." table)
    (modify-syntax-entry ?<  "." table)
    (modify-syntax-entry ?>  "." table)
    (modify-syntax-entry ??  "." table)

    ;; Modify some syntax entries to allow nested block comments
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23n" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\^m "> b" table)

    table))

(defconst jai-builtins
  '("cast" "it"))

(defconst jai-keywords
  '("if" "else" "then" "while" "for" "switch" "case" "struct" "enum"
    "return" "new" "remove" "continue" "break" "defer" "inline" "no_inline"
    "using" "SOA"))

(defconst jai-constants
  '("null" "true" "false"))

(defconst jai-typenames
  '("int" "u64" "u32" "u16" "u8"
    "s64" "s32" "s16" "s8" "float"
    "float32" "float64" "string"
    "bool"))

(defun jai-wrap-word-rx (s)
  (concat "\\<" s "\\>"))

(defun jai-keywords-rx (keywords)
  "build keyword regexp"
  (jai-wrap-word-rx (regexp-opt keywords t)))

(defconst jai-hat-type-rx (rx (group (and "^" (1+ word)))))
(defconst jai-dollar-type-rx (rx (group "$" (or (1+ word) (opt "$")))))
(defconst jai-number-rx (rx (group (and (opt "0x") (1+ num) (opt (and "." (0+ num))) (opt (in "fgFG"))))))

(defconst jai-procedure-rx (rx (and "("
                                    (0+ anything)
                                    ")"
                                    (0+ space)
                                    (opt (and "->"
                                              (0+ space)
                                              (opt "(")
                                              (0+ anything)
                                              (opt ")")))
                                    (0+ space)
                                    "{")))


(defconst jai-font-lock-defaults
  `(
    ;; Keywords
    (,(jai-kwrx jai-keywords) 1 font-lock-keyword-face)

    ;; single quote characters
    ("\\('[[:word:]]\\)\\>" 1 font-lock-constant-face)

    ;; Variables
    (,(jai-keywords-rx jai-builtins) 1 font-lock-variable-name-face)

    ;; Constants
    (,(jai-keywords-rx jai-constants) 1 font-lock-constant-face)

    ;; Hash directives
    ("#\\w+" . font-lock-preprocessor-face)

    ;; At directives
    ("@\\w+" . font-lock-preprocessor-face)

    ;; Strings
    ("\\\".*\\\"" . font-lock-string-face)

    ;; Numbers
    (,(jai-wrap-word-rx jai-number-rx) . font-lock-constant-face)

    ;; Types
    (,(jai-keywords-rx jai-typenames) 1 font-lock-type-face)
    (,jai-hat-type-rx 1 font-lock-type-face)
    (,jai-dollar-type-rx 1 font-lock-type-face)

    ("---" . font-lock-constant-face)
    ))

(defmacro jai-paren-level ()
  `(car (syntax-ppss)))

(defmacro jai-in-string-or-comment-p ()
  `(nth 8 (syntax-ppss)))

(defmacro jai-in-string-p ()
  `(nth 3 (syntax-ppss)))

(defmacro jai-in-comment-p ()
  `(nth 4 (syntax-ppss)))

(defmacro jai-goto-beginning-of-string-or-comment ()
  `(goto-char (nth 8 (syntax-ppss))))


(defun jai--backward-irrelevant (&optional stop-at-string)
  "Skips backwards over any characters that are irrelevant for
indentation and related tasks.

It skips over whitespace, comments, cases and labels and, if
STOP-AT-STRING is not true, over strings."

  (let (pos (start-pos (point)))
    (skip-chars-backward "\n\s\t")
    (if (and (save-excursion (beginning-of-line) (jai-in-string-p)) (looking-back "`") (not stop-at-string))
        (backward-char))
    (if (and (jai-in-string-p) (not stop-at-string))
        (jai-goto-beginning-of-string-or-comment))
    (if (looking-back "\\*/")
        (backward-char))
    (if (jai-in-comment-p)
        (jai-goto-beginning-of-string-or-comment))
    (setq pos (point))
    (beginning-of-line)
    (goto-char pos)
    (if (/= start-pos (point))
        (jai--backward-irrelevant stop-at-string))
    (/= start-pos (point))))

(defun jai-beginning-of-defun (&optional count)
  "Try to find the beginning of the enclosing procedure"
  (unless (bolp)
    (end-of-line))
  (setq count (or count 1))
  (let (first failure)
    (dotimes (i (abs count))
      (setq first t)
      (while (and (not failure)
                  (or first (jai-in-string-or-comment-p)))
        (if (>= count 0)
            (progn
              (jai--backward-irrelevant)
              (if (not (re-search-backward jai-procedure-rx nil t))
                  (setq failure t)))
          (if (looking-at jai-procedure-rx)
              (forward-char))
          (if (not (re-search-forward jai-procedure-rx nil t))
              (setq failure t)))
        (setq first nil)))
    (if (< count 0)
        (beginning-of-line))
    (not failure)))

(defun jai-end-of-defun ()
  "Try to find the end of the enclosing procedure"
  (interactive)
  (let ((orig-level (jai-paren-level)))
    (if (<= orig-level 0)
      (skip-chars-forward "^}")
      (while (>= (jai-paren-level) orig-level)
        (skip-chars-forward "^}")
        (forward-char)))))

(defun jai-indent-line ()
   "Indent current line of jai code."
   (interactive)
   (let ((savep (> (current-column) (current-indentation)))
         (indent (condition-case nil (* jai-indent-level (max (jai-calculate-indentation) 0))
                   (error 0))))
     (if savep
	 (save-excursion (indent-line-to indent))
       (indent-line-to indent))))

(defun jai-calculate-indentation ()
  "Return the column to which the current line should be indented."
    (let ((indent-level 0))
      (save-excursion
        (beginning-of-line)
        (while (not (bobp))
          ;; TODO search backwards
          (backward-char)
          (cond ((looking-at "{") (setq indent-level (+ 1 indent-level)))
                ((looking-at "}") (setq indent-level (- indent-level 1))))))
      (save-excursion
        (beginning-of-line)
        (let ((p0 (point)))
          (end-of-line)
          (while (and (>= (point) p0)
                      (not (looking-at "{")))
            (when (looking-at "}")
              (setq indent-level (- indent-level 1)))
            (backward-char))))
      indent-level))

;;;###autoload
(define-derived-mode jai-mode prog-mode "Jai Mode"
  :syntax-table jai-mode-syntax-table
  (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  (setq-local comment-start "/*")
  (setq-local comment-end "*/")
  (setq-local indent-line-function 'jai-indent-line)
  (setq-local font-lock-defaults '(jai-font-lock-defaults))
  (setq-local beginning-of-defun-function 'jai-beginning-of-defun)
  (setq-local end-of-defun-function 'jai-end-of-defun)

  (font-lock-fontify-buffer))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.jai\\'" . jai-mode))

(provide 'jai-mode)
