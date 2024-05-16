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
- `Maybe` and `Either` as core parts of the language (ideally they'd be just another sum type, but I'm not sure how to integrate that into lisp style)
- Polymorphism as a core principal in function writing: a function on lists must accept a `(List a`), and even the type constructor may be generic: `(impls (m '(Monad)) (m a))`

## Other elements
- Looping will not be an entire DSL like in Common Lisp, it will be incredibly basic. Printing will not resemble `(format)` either, etc.
- All predicates must end in a question mark, as in: `(if (empty? xs) ... ...)`
- Basic types are called: `String`, `Int` & `Uint`, `f32` & `f64`, `char` (unicode minimum unit)
- Standard formatting is Lisp style, with two spaces
- Comments are prefixes by either `<--` or `--`. Doccoments are prefixes with `<-->`
- We will not have 52 ways to check for equality. There will only be one, dependant on the `Eq` typeclass
- Booleans are written `#true` and `#false`

## Roadmap
- [x] Prototype spec
- [-] Emacs major mode (syntax highlighting)
- [ ] Formalize the grammar
- [ ] Prototype lexer
  - [ ] Proper errors in lexing
- [ ] Finalize lexer
- [ ] Prototype parser (AST)
  - [ ] Syntax checker
  - [ ] Type checking
  - [ ] Proper errors in parsing
- [ ] Finalize parser (AST)
- [ ] Final spec
- [ ] Compilation
    - [ ] Start emitting for LLVM [Helpful link](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl01.html)
    - [ ] LLVM output

## To Resolve
- How do I handle unwrapping/pattern matching in an aesthetically pleasing and reasonable way for e.g. `(head)`?
- How do I create record types and access their fields without losing my sanity?
- How do I pattern match _effectively_? How should e.g. (match) work? Especially the types!
- Will function accept variadic arguments? How would that interact with the type checker?
  - Possible solution: do not, make 'variadics' mean 'a function that takes in a list'


## Basic syntax
It's a lisp, so functions are called like `(f)` or `(f 1 2 3)`.

### Function definition
Functions are defined as follows:
```
(deffun functionname
  optional-doc-comment
  list-of-arguments+return-types
  list-of-argument-names
  body-expr)
```

For example:

```
(deffun factorial
  <--> Return the factorial of the argument
  (Uint Uint)      <-- Takes in an unsigned integer, returns another one
  (n)              <-- input Int is bound to `n`
  (if (= n 0)      <-- Standard factorial definition
    1
    (* n (fact (dec n)))))
```


### Conditionals
#### `(if)`
```
(if cond expr-if-true expr-if-false)
```

#### `(case)`
```
(case expr
  (pred-1 expr-1
   pred-2 expr-2
   pred-3 expr-3
   ...
   pred-n expr-n))
```

### Pattern matching

`(todo)`

### Typeclasses
Note that the `list-of-methods-with-a-default-impl` default implementation can't go inside the typeclass definition, and should instead be placed elsewhere
```
(deftypeclass (name types)
  class-bounds
  list-of-methods-to-implement
  list-of-methods-with-a-default-impl)
```

Example:
```
(deftypeclass (Eq a)
  ()
  (('= Fn[a a Bool]))     <-- Note that = is a normal valid function name
  ())
```

```
(deftypeclass (Monad m)
  ((Functor m) (Applicative m))
  ((>>= Fn[(m a) Fn[a (m b)] (m b)])            <-- Note that >>= is a normal valid function name
   (return Fn[a (m a)])) 
  ((>> whatever-this-is-called-here)))
```

### Generics (and restricting them)
IMPORTANT: all single-letter, lower case types are interpreted as generics. Also note that type constructors must be uppercase (e.g. `Int` or `(List a)`)

Accepting generics is easy. For example, you can have a list of any `a`:
```
(deftype (List a)
  '((Empty)
    (Cons a (List a))))
```

Here's a function that checks for the emptiness of _any_ `(List a)`
```
(deffun empty?
  ((List a) bool)
  (xs)
  (match xs
   ((Empty) true
    (Cons _ _) false)))
  
```

More interesting is what happens when you want to restrict `a` to being part of a `typeclass`: for example, say we want to check if all elements of the list are equal:


```
(deffun isHomogenous?
  ((where (a '(Eq)) (List a))
   bool)
  (xs)
  (match xs
   (((Empty) true)
    ((Cons y (Empty)) true)
    ((Cons y ys) (and
                    (= y (head ys))
                    (isAllEqual (tail ys)))))))
  
```

So, a bounded type can be written in:

```
(impls type list-of-typeclasses) <-- For if the entire type must implement the typeclass
or
(where list-of-bound-types-and-classes type) <-- For if only a bound type must implement it
```

`where` is to be used, for example, when the entire type doesn't need to be bounded: e.g. if `Tree`s don't need to be comparable but `a`s must: `(where (a '(Eq) (Tree a)))`

Which both returns the `type`, but bounded. 


## Helpful links
- Making an interpreter: https://vishpat.github.io/lisp-rs/overview.html
- Outputting to LLVM: https://llvm.org/docs/tutorial/


[^1]: Get it? It's a _lisp_ called '_Ç_-lang', hehe.
