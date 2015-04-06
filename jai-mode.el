;; jai-mode.el - very basic jai mode

(require 'cl)
(require 'rx)
(require 'js)

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
  '("cast" "it" "type_info" "size_of"))

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
(defconst jai-number-rx
  (rx (and
       symbol-start
       (or (and (+ digit) (opt (and (any "eE") (opt (any "-+")) (+ digit))))
           (and "0" (any "xX") (+ hex-digit)))
       (opt (and (any "_" "A-Z" "a-z") (* (any "_" "A-Z" "a-z" "0-9"))))
       symbol-end)))

(defconst jai-font-lock-defaults
  `(
    ;; Keywords
    (,(jai-keywords-rx jai-keywords) 1 font-lock-keyword-face)

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

;; add setq-local for older emacs versions
(unless (fboundp 'setq-local)
  (defmacro setq-local (var val)
    `(set (make-local-variable ',var) ,val)))

(defconst jai--defun-rx "\(.*\).*\{")

(defmacro jai-paren-level ()
  `(car (syntax-ppss)))

(defun jai-line-is-defun ()
  "return t if current line begins a procedure"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let (found)
      (while (and (not (eolp)) (not found))
        (if (looking-at jai--defun-rx)
            (setq found t)
          (forward-char 1)))
      found)))

(defun jai-beginning-of-defun (&optional count)
  "Go to line on which current function starts."
  (interactive)
  (let ((orig-level (jai-paren-level)))
    (while (and
            (not (jai--line-is-defun))
            (not (bobp))
            (> orig-level 0))
      (setq orig-level (jai-paren-level))
      (while (>= (jai-paren-level) orig-level)
        (skip-chars-backward "^{")
        (backward-char))))
  (if (jai--line-is-defun)
      (beginning-of-line)))

(defun jai-end-of-defun ()
  "Go to line on which current function ends."
  (interactive)
  (let ((orig-level (jai-paren-level)))
    (when (> orig-level 0)
      (jai-beginning-of-defun)
      (end-of-line)
      (setq orig-level (jai-paren-level))
      (skip-chars-forward "^}")
      (while (>= (jai-paren-level) orig-level)
        (skip-chars-forward "^}")
        (forward-char)))))

;;;###autoload
(define-derived-mode jai-mode prog-mode "Jai Mode"
  :syntax-table jai-mode-syntax-table
  :group 'jai
  (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  (setq-local comment-start "/*")
  (setq-local comment-end "*/")
  (setq-local indent-line-function 'js-indent-line)
  (setq-local font-lock-defaults '(jai-font-lock-defaults))
  (setq-local beginning-of-defun-function 'jai-beginning-of-defun)
  (setq-local end-of-defun-function 'jai-end-of-defun)

  (font-lock-fontify-buffer))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.jai\\'" . jai-mode))

(provide 'jai-mode)
