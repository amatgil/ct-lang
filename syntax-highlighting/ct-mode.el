;;; ct-mode.el --- major mode for ç-lang

;(define-generic-mode ct-mode
;   (list "<--" "--")                                           ; Comments
;   (list "deffun" "deftype" "deftypeclass" "loop" "match")     ; Keyword
;  '(("true" . 'font-lock-constant-face)                        ; font lock
;    ("false" . 'font-lock-constant-face))
;   (list "*.ct")                                               ; automode lock
;   nil
;   "Major mode for editing ç-lang files")

(defconst ct-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?' "\"" table)  ; Single quote -> string
    (modify-syntax-entry ?\" "\"" table) ; Double quote -> string
    (modify-syntax-entry ?\n ">" table)  ; Newline finishes comment
    ))

(define-derived-mode ct-mode
  prog-mode "Ç-lang"
  "Major mode for Ç-lang"
  :syntax-table ct-mode-syntax-table
  (font-lock-fontify-buffer))


; After running eval-buffer and ct-mode, you should be able to see the changes here (because a semicolon isn't a comment in it!)
;(deftype (List a)       <-- List with typed admitted by the type constructor
;  '(Empty               <-- possible variants of sum type
;   (Cons a (List a))))
;
;(deffun foldl
;  (Fn[a a a] a (List a))
;  (f acc xs)
;  (if (empty? xs)
;    acc
;    (foldl 'f (f acc (head xs)) (tail xs))))
;
;(deffun empty?
;  ((List a) bool)
;  (xs)
;  (match xs 
;    ((Empty) true)
;    ((Cons x t) false)))
;
;(deffun append
;  ((List a))
;  (xss)
;  (match xss
;   ((Empty) (Empty)
;    (Cons xs rest) (append xs (flatten xss)))))
;
;(deffun flatten
;  ((List (List a)) (List a))
;  (xss)
;  (match xss
;   ((Empty) (Empty)
;    (Cons xs rest) (append xs (flatten xss)))))
