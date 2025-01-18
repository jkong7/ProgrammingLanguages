#lang plai

; Jonathan Kong
; CS 321
; Winter 2025
; Homework 2

(define eight-principles
  (list
   "Know your rights."
   "Acknowledge your sources."
   "Protect your work."
   "Avoid suspicion."
   "Do your own work."
   "Never falsify a record or permit another person to do so."
   "Never fabricate data, citations, or experimental results."
   "Always tell the truth when discussing your work with your instructor."))

(print-only-errors)

(define-type fnWAE
  [num (n number?)]
  [add (lhs fnWAE?)
       (rhs fnWAE?)]
  [sub (lhs fnWAE?)
       (rhs fnWAE?)]
  [with (name symbol?)
        (bound-expr fnWAE?)
        (body-expr fnWAE?)]
  [id (name symbol?)]
  [app (fun-name symbol?)
       (arg-exprs (listof fnWAE?))])

(define-type FunDef
  [fundef (fun-name symbol?)
          (param-names (listof symbol?))
          (body fnWAE?)])

(define-type DefSub
  [mtSub]
  [aSub (name symbol?)
        (value number?)
        (rest DefSub?)])

;; ----------------------------------------------------------------------

#|
<FunDef> ::= {deffun {<id> <id>*} <fnWAE>}
<fnWAE> ::= <num>
         | {+ <fnWAE> <fnWAE>}
         | {- <fnWAE> <fnWAE>}
         | {with {<id> <fnWAE>} fnWAE>}
         | <id>
         | {<id> <fnWAE>*}
|#

;; parse : s-expression -> fnWAE?

(define (parse s-expr)
  (cond [(number? s-expr) (num s-expr)]
        [(symbol? s-expr) (id s-expr)]
        [(list? s-expr)
         (when (empty? s-expr)
           (error 'parse "the empty list is not a valid fnWAE"))
         (case (first s-expr)
           [(+)
            (check-pieces s-expr 3 "addition")
            (add (parse (second s-expr))
                 (parse (third s-expr)))]
           [(-)
            (check-pieces s-expr 3 "subtraction")
            (sub (parse (second s-expr))
                 (parse (third s-expr)))]
           [(with)
            (check-pieces s-expr 3 "with")
            (check-pieces (second s-expr) 2 "with binding")
            (unless (symbol? (first (second s-expr)))
              (error 'parse "expected a variable name, got: ~a" (first (second s-expr))))
            (with (first (second s-expr))
                  (parse (second (second s-expr)))
                  (parse (third s-expr)))]
           [else
            (unless (symbol? (first s-expr))
              (error 'parse "expected a function name, got: ~a" (first s-expr)))
            (app (first s-expr)
                 (map parse (rest s-expr)))]
            )]
        [else
         (error 'parse "expected an fnWAE, got: ~a" s-expr)]
        ))


;; parse-defn : s-expression -> fNWAE?

(define (parse-defn s-expr)
  (case (first s-expr)
    [(deffun)
     (unless (symbol? (first (second s-expr)))
       (error 'parse-defn "expected a function name, got: ~a" (first (second s-expr))))
     (when (list-repeat? (rest (second s-expr)))
       (error 'parse-defn "bad syntax, two or more arguments are the same"))
     (fundef (first (second s-expr))
             (rest (second s-expr))
             (parse (third s-expr)))]
    [else
     (error 'parse-defn "expected a deffun expression, got: ~a" s-expr)]))

;; list-repeat? : (listof symbol) -> bool

(define (list-repeat? lst)
  (cond [(empty? lst) #f]
        [(member (first lst) (rest lst)) #t]
        [else (list-repeat? (rest lst))]))

;; check-pieces : s-expression number string -> void?

(define (check-pieces s-exp n-pieces expected)
  (unless (and (list? s-exp)
               (= (length s-exp) n-pieces))
    (error 'parse "expected ~a, got: ~a" expected s-exp)))


;; parse tests

(test (parse `1) (num 1))

(test (parse `y) (id 'y))

(test (parse `{+ 1 2}) (add (num 1) (num 2)))

(test (parse `{- 1 2}) (sub (num 1) (num 2)))

(test (parse `{with {x 3} {+ x 2}}) (with 'x (num 3) (add (id 'x) (num 2))))

(test (parse `{f 10})
      (app 'f (list (num 10))))

(test/exn (parse `{+ 1 2 3}) "expected addition")

(test/exn (parse '{- 2}) "expected subtraction")

(test/exn (parse `{1 2 10000})
          "expected a function name")

(test/exn (parse `{with {'x 2} {+ 3 4}})
          "expected a variable name")

(test/exn (parse '"")
          "expected an fnWAE")


(test (parse `{f})
      (app 'f (list)))

(test (parse `{f 1 {g 2 3}})
      (app 'f (list (num 1) 
                    (app 'g (list (num 2) (num 3))))))

(test (parse '{+ {f 1 2} {g 3}})
      (add (app 'f (list (num 1) (num 2)))
           (app 'g (list (num 3)))))

(test (parse '{with {x 10} {f x 20}})
      (with 'x (num 10) (app 'f (list (id 'x) (num 20)))))

(test (parse '{f {a 5 10} {b {+ 4 5}} {with {c 5} {+ c 2}} {- 4 3}})
      (app 'f (list
               (app 'a (list (num 5) (num 10)))
               (app 'b (list (add (num 4) (num 5))))
               (with 'c (num 5) (add (id 'c) (num 2)))
               (sub (num 4) (num 3)))))

;; parse-defn tests

(test (parse-defn '{deffun {f x y} {+ x y}})
      (fundef 'f (list 'x 'y)
              (add (id 'x) (id 'y))))

(test (parse-defn '{deffun {j} {with {z 10} {+ z z}}})
      (fundef 'j (list)
              (with 'z (num 10) (add (id 'z) (id 'z)))))

(test (parse-defn '{deffun {k a b c d} {with {e 42} {+ a e}}})
      (fundef 'k (list 'a 'b 'c 'd)
              (with 'e (num 42) (add (id 'a) (id 'e)))))

(test/exn (parse-defn '{deffun {"not" x y} {+ 1 2}})
          "expected a function name")

(test/exn (parse-defn '{deffun {dup x x y} {+ 1 2}})
          "bad syntax, two or more arguments are the same")

(test/exn (parse-defn '{not-deffun {f x y z} {+ x y z}})
          "expected a deffun expression")



;; ----------------------------------------------------------------------

;; interp : fnWAE? (listof FunDef?) DefSub? -> number?
(define (interp an-fnwae fundefs ds)
  (type-case fnWAE an-fnwae
    [num (n) n]
    [add (l r) (+ (interp l fundefs ds) (interp r fundefs ds))]
    [sub (l r) (- (interp l fundefs ds) (interp r fundefs ds))]
    [id (name) (lookup name ds)]
    [with (name named-expr body)
          (interp body
                  fundefs
                  (aSub name
                        (interp named-expr fundefs ds)
                        ds))]
    [app (fun-name arg-exprs)
         (define found-function (lookup-fundef fun-name fundefs))
         (define found-function-param-names (fundef-param-names found-function))
         (define body (fundef-body found-function))
         (unless (equal? (length arg-exprs) (length found-function-param-names))
           (error 'interp "wrong arity"))
         (define interpreted-arg-exprs
           (map (lambda (arg-expr) (interp arg-expr fundefs ds)) arg-exprs))
         (define paired-symbol-to-name
           (map cons found-function-param-names interpreted-arg-exprs))
         (define new-ds
           (foldl (lambda (pair acc) (aSub (car pair)
                                           (cdr pair)
                                           acc))
                  (mtSub)
                  paired-symbol-to-name))
         (interp body fundefs new-ds)]))

;; lookup : symbol? DefSub? -> number?

(define (lookup name ds)
  (type-case DefSub ds
    [mtSub () (error 'interp "free identifier")]
    [aSub (name2 value rest)
          (if (equal? name name2)
              value
              (lookup name rest))]))

;; lookup-fundef : symbol? (listof FunDef?) -> FunDef?

(define (lookup-fundef name fundefs)
  (cond [(empty? fundefs)
         (error 'interp "undefined function: ~a" name)]
        [(equal? name (fundef-fun-name (first fundefs)))
         (first fundefs)]
        [else
         (lookup-fundef name (rest fundefs))]))



;; parse, parse-defn, interp tests, functions with multiple arguments:

;; utilizing deferred substitution: each interp takes in an initially empty ds variable binding list:
(define initial-def-sub (mtSub))

(test (interp (parse `{f 10})
              (list (fundef 'f '(x) 
                            (parse `{- 20 {twice {twice x}}}))
                    (fundef 'twice '(y) 
                            (parse `{+ y y}))) initial-def-sub)
      -20)

(test (interp (parse '{+ {f 10 20 30} {g 50 20}})
              (list (fundef 'f '(x y z) 
                            (parse `{+ x {+ y z}}))
                    (fundef 'g '(a b) 
                            (parse `{- a b}))) 
              initial-def-sub)
      90)

(test (interp (parse '{with {x 10} {f {+ x 5} 20}})
              (list (fundef 'f '(a b) 
                            (parse `{+ a b}))) 
              initial-def-sub)
      35)

(test (interp (parse '{with {x 5} {f x 10}})
              (list (fundef 'f '(a b) 
                            (parse `{+ a b}))) 
              initial-def-sub)
      15)

(test (interp (parse '{with {y 2} {f y}})
              (list (parse-defn '{deffun {f x} {+ x 1}}))
              initial-def-sub)
      3)

(test (interp (parse '{+ {f 10} {g 20 10}})
              (list (parse-defn '{deffun {f x} {+ x 1}})
                    (parse-defn '{deffun {g a b} {- a b}}))
              initial-def-sub)
      21)

(test (interp (parse '{+ {f 1 2 3} {g 10 2 1}})
              (list (parse-defn '{deffun {f x y z} {+ x {+ y z}}})
                    (parse-defn '{deffun {g a b c} {- a {+ b c}}}))
              initial-def-sub)
      13)


;; errors: undefined function, bad syntax, wrong arrity, free identifier 

(test/exn (interp (parse '{with {a {+ 5 6}} {+ a b}})
                  (list)
                  initial-def-sub)
          "free identifier")

(test/exn (interp (parse '{+ {with {x {+ 2 3}} x} {with {y 10} {+ y z}}})
                  (list)
                  initial-def-sub)
          "free identifier")

(test/exn (interp (parse '{with {x y} (+ 1 23)})
                  (list)
                  initial-def-sub)
          "free identifier")

(test/exn (interp (parse '{f 10 20})
                  (list (parse-defn '{deffun {f x y} {+ x z}}))
                  initial-def-sub)
          "free identifier")

(test/exn (interp (parse '{h 3 4})
                  (list (parse-defn '{deffun {h a a} {+ a a}}))
                  initial-def-sub)
          "bad syntax")

(test/exn (interp (parse '{g 5 10})
                  (list (parse-defn '{deffun {g x y x} {+ x y}}))
                  initial-def-sub)
          "bad syntax")


(test/exn (interp (parse '{with {z {+ 5 5}} {f z}})
                  (list (parse-defn '{deffun {g x y} {+ x y}}))
                  initial-def-sub)
          "undefined function")

(test/exn (interp (parse '{h 10})
                  (list (parse-defn '{deffun {f x} {+ x x}}))
                  initial-def-sub)
          "undefined function")

(test/exn (interp (parse '{f 20})
                  (list (parse-defn '{deffun {f a b} {+ a b}}))
                  initial-def-sub)
          "wrong arity")


(test/exn (interp (parse '{with {a {+ 3 3}} {f a}})
                  (list (parse-defn '{deffun {f x y z} {+ x y}}))
                  initial-def-sub)
          "wrong arity")



;; provided tests

;; ----------------------------------------------------------------------
;; tests from last time, updated

;; {deffun {f x} {+ y x}}
;; {with {y 2} {f 10}}
(test/exn (interp (parse `{with {y 2} {f 10}})
                  (list (fundef 'f '(x) 
                                (parse `{+ y x})))
                  initial-def-sub)
          "free identifier")

;; {deffun {f x} {+ 1 x}}
;; {with {y 2} {f y}} ; -> 3
(test (interp (parse `{with {y 2} {f y}})
              (list (fundef 'f '(x) (parse `{+ 1 x})))
              initial-def-sub)
      3)

;; 5 -> 5
(test (interp (parse `5) '() initial-def-sub)
      5)

;; {+ 1 2} -> 3
(test (interp (parse `{+ 1 2}) '() initial-def-sub)
      3)

;; {- 3 4} -> -1
(test (interp (parse `{- 3 4}) '() initial-def-sub)
      -1)

;; {+ {+ 1 2} {- 3 4}} -> 2
(test (interp (parse `{+ {+ 1 2} {- 3 4}}) '() initial-def-sub)
      2)

#|
{with {x {+ 1 2}}
      {+ x x}}
|#
(test (interp (parse `{with {x {+ 1 2}}
                            {+ x x}})
              '()
              initial-def-sub)
      6)

#|
x
|#
(test/exn (interp (parse `x) '() initial-def-sub)
          "free identifier")

#|
{+ {with {x {+ 1 2}}
         {+ x x}}
   {with {x {- 4 3}}
         {+ x x}}}
|#
(test (interp (parse `{+ {with {x {+ 1 2}}
                               {+ x x}}
                         {with {x {- 4 3}}
                               {+ x x}}})
              '()
              initial-def-sub)
      8)

#|
{+ {with {x {+ 1 2}}
         {+ x x}}
   {with {y {- 4 3}}
         {+ y y}}}
|#
(test (interp (parse `{+ {with {x {+ 1 2}}
                               {+ x x}}
                         {with {y {- 4 3}}
                               {+ y y}}})
              '()
              initial-def-sub)
      8)

#|
{with {x {+ 1 2}}
      {with {x {- 4 3}}
            {+ x x}}}
|#
(test (interp (parse `{with {x {+ 1 2}}
                            {with {x {- 4 3}}
                                  {+ x x}}})
              '()
              initial-def-sub)
      2)

#|
{with {x {+ 1 2}}
      {with {y {- 4 3}}
            {+ x x}}}
|#
(test (interp (parse `{with {x {+ 1 2}}
                            {with {y {- 4 3}}
                                  {+ x x}}})
              '()
              initial-def-sub)
      6)

;; Function application tests
(test/exn (interp (parse `{f 10})
                  (list)
                  initial-def-sub)
          "undefined function")

(test (interp (parse `{f 10})
              (list (fundef 'f '(x) 
                            (parse `{- 20 {twice {twice x}}}))
                    (fundef 'twice '(y) 
                            (parse `{+ y y})))
              initial-def-sub)
      -20)

(test (interp (parse `{f 10})
              (list (fundef 'f '(x)  
                            (parse `{- 10 {twice {twice x}}}))
                    (fundef 'twice '(y) 
                            (parse `{+ y y})))
              initial-def-sub)
      -30)
