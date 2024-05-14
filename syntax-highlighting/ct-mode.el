;;; ct-mode.el --- major mode for ç-lang

(setq debug-on-error t)

(defconst ct-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" table) ; Double quote -> string
    (modify-syntax-entry ?\n ">" table)  ; Newline finishes comment
    table
    ))

(define-derived-mode ct-mode
   prog-mode "Ç-lang"
   "Major mode for Ç-lang"
   :syntax-table ct-mode-syntax-table
   (set (make-local-variable 'font-lock-defaults) '(ct-font-lock t nil nil)))

(defconst ct-keywords (concat "\\<" (regexp-opt '("deffun" "deftype" "deftypeclass" "if" "match" "where" "impls" "impl" "let")) "\\>"))

(defconst ct-font-lock
  `(
    ("<--.*" . font-lock-comment-face)
    ("{-.*-}" . font-lock-comment-face)
    ("\".*\"" . font-lock-string-face)
    ("[?[A-Z][a-zA-Z0-9_']*" . font-lock-type-face)
    (,ct-keywords . font-lock-keyword-face) 
    ))

; After running eval-buffer and ct-mode, you should be able to see the changes here (because a semicolon isn't a comment in it!)
;(deftype (List a)         <-- List with typed admitted by the type constructor
;  '((Empty)               <-- possible variants of sum type
;    (Cons a (List a))))
;
;(deffun foldl
;  (Fn[a a a] a (List a))
;  (f acc xs)
;  (if (empty? xs)
;    acc
;    (foldl 'f (f acc (head xs)) (tail xs))))
;
;(deffun empty?
;  ((List a) Bool)
;  (xs)
;  (match xs 
;    ((Empty) true)
;    ((Cons x t) false)))
;
;(deffun append
;  (a (List a) (List a))
;  (x xs)
;  (Cons x xs))
;   
;
;(deffun flatten
;  ((List (List a)) (List a))
;  (xss)
;  (match xss
;   ((Empty) (Empty)
;    (Cons xs rest) (append xs (flatten xss)))))
;
;(deffun stringtest
;  (Bool)
;  (b)
;  (if b "It's true :D" "It's false D:"))
