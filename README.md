# Ç-lang
Ç-lang, also known as `ct` (for 'ce trencada'), is a strongly typed lisp[^1] with type inference that aims to address some of the minutia that annoyed me, personally, about Common Lisp (such as essentially no having any types).

Examples can be found under `examples`.

## Core ideals
The language contains:
- No mutability, no side-effects. This is _mostly_ a functional language, though it contains nice-to-haves like basic loops
- Strict typing
- Typeclasses (à la Haskell)
- Generics with easy-to-set class bounds
- Functions as First Order Types
- `Maybe` and `Either` as core parts of the language (ideally they'd be just another sum type, but I'm not sure how to integrate that into lisp type)


## Other elements
- Looping will not be an entire DSL like in Common Lisp, it will be incredibly basic. Printing will not resemble `(format)` either.
- All predicates must end in a question mark, as in: `(if (empty? xs) ... ...)`
- Basic types are called: `String`, `Int` & `Uint`, `f32` & `f64`, `char` (unicode minimum unit)

## Roadmap
- [ ] Prototype spec
- [ ] Final spec
- [ ] Compiler
  - [ ] Parse 
  - [ ] Proper errors in parsing
  - [ ] Generate AST
  - [ ] Type checking
  - [ ] Optimization passes (on its own AST)
  - [ ] Evaluate final AST
  - [ ] Emit native bytecode

## To Resolve


Debate on whether to allow (set): do we want mutability?
Macros seem too hard '>.>

Will have:





- file extension would be nice to be ct, if not taken
- comments are marked with <--

# Doubts:
- How to write down generics? Also with an attribute 
- How do I handle stuff like unwrapping in an aesthetically reasonable way for e.g. (head)?
- How do I create record types and access their fields without losing my sanity?
- How do I pattern match effectively? How should e.g. (match) work? Especially the types!


(deffun fib
  (Int (List Int))                 <-- types, curried
  (x)                                   <-- names except
  (if (or? (=? x 0) (=? x 1))         <-- body
     1
     (+ (fib (- x 1)) (fib (- x 2)))

In module std/list
(deffun foldl
  (Fn[a a a] a (List a))
  (f acc xs)
  (if (empty? xs)
    acc
    (foldl 'f (f acc (head xs)) (tail xs))))

(deftype (List a) <- Q(?)list with generics 
  '(Empty            <- possible varis of enum 
  '(Cons a (List a)))

(deftype (Tree a) 
  '('(Leaf a)
   '('(Left a) '(Right a))))

(deffun empty?
  ((List a) bool)
  (xs)
  (match xs 
    ((Empty) true)
    ((Cons x t) false)))

(deffun fizzbuzz
  (Int Nil)
  (max)
  (loop from 0 to max (








[^1]: Get it? It's a _lisp_ called '_Ç_-lang', hehe.
