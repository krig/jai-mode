;; jai-mode.el - very basic jai mode

(require 'cl)

(defcustom jai-indent-level 4 "Number of spaces per indent.")

(defconst jai-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; ' is a string delimiter
    (modify-syntax-entry ?' "\"" table)
    ;; " is a string delimiter too
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    ;; additional symbols
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?: "_" table)

    ;; Modify some syntax entries to allow nested block comments
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23n" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\^m "> b" table)

    table))

(defun jai-kwrx (&rest keywords)
  "build keyword regexp"
  (concat "\\<" (regexp-opt keywords t) "\\>"))

(defconst jai-keywords
  `(
    ;; Keywords
    (,(jai-kwrx "if" "else" "while" "for" "switch" "case" "struct" "return")
     1 font-lock-keyword-face)

    ;; Variables
    (,(jai-kwrx "it" "cast") 1 font-lock-variable-name-face)

    ;; Hash directives
    ("\\(#\\w+\\)" 1 font-lock-constant-face)

    ;; At directives
    ("\\(@\\w+\\)" 1 font-lock-constant-face)

    ;; Strings
    ("\\(\\\".*\\\"\\)" 1 font-lock-string-face)

    ;; Types
    (,(jai-kwrx "int" "u64" "u32" "u16" "u8" "s64" "s32" "s16" "s8" "float" "float32" "float64" "double" "string") 1 font-lock-type-face)
    ("\\(\\^\\w+\\)" 1 font-lock-type-face)
    ("\\<\\($\\w+\\)\\>" 1 font-lock-type-face)
    ("\\<\\(${2}\\)\\>" 1 font-lock-type-face)
    ))


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
  (setq-local font-lock-defaults '(jai-keywords))
  (font-lock-fontify-buffer))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.jai\\'" . jai-mode))

(provide 'jai-mode)
