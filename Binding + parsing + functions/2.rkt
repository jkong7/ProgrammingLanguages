#lang plai

; Jonathan Kong
; CS 321
; Winter 2025
; Homework 1

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

(test (parse `1)
      (num 1))

(test (parse `x)
      (id 'x))

(test (parse `{+ 1 2})
      (add (num 1) (num 2)))

(test (parse `{- 1 2})
      (sub (num 1) (num 2)))

(test (parse `{+ 1 {+ 2 3}})
      (add (num 1) (add (num 2) (num 3))))

(test (parse `{with {x 3} {+ x 2}})
      (with 'x (num 3) (add (id 'x) (num 2))))

(test (parse `{f 10})
      (app 'f (list (num 10))))

(test (parse '{f 10 {+ 1 3}})
      (app 'f (list (num 10) (add (num 1) (num 3)))))

(test (parse '{f x {f 10} {- 5 3}})
      (app 'f (list (id 'x)
                    (app 'f (list (num 10)))
                    (sub (num 5) (num 3)))))

(test (parse '{f})
      (app 'f (list)))


;; parse error cases

(test/exn (parse `{1 2})
          "expected a function name")
(test/exn (parse `{+ 1 2 3})
          "expected addition")
(test/exn (parse '{with {2 2} {+ 3 2}})
          "expected a variable name")


;; parse-defn tests

(test (parse-defn '{deffun {f x y} {+ 1 2}})
      (fundef 'f (list 'x 'y)
              (add (num 1) (num 2))))

(test (parse-defn '{deffun {f x y} {+ x y}})
      (fundef 'f (list 'x 'y)
              (add (id 'x) (id 'y))))

(test (parse-defn '{deffun {f} 5})
      (fundef 'f (list)
              (num 5)))

(test (parse-defn '{deffun {f x y} x})
      (fundef 'f (list 'x 'y) (id 'x)))

(test (parse-defn '{deffun {f x y} {+ x y}})
      (fundef 'f (list 'x 'y) (add (id 'x) (id 'y))))


(test/exn (parse-defn '{deffun {123 x y} {+ 1 2}})
          "expected a function name")

(test/exn (parse-defn '{not-a-deffun {f x y} {+ 1 2}})
          "expected a deffun expression")

(test/exn (parse-defn '{deffun {f x x} {+ 1 2}})
          "bad syntax")



;; ----------------------------------------------------------------------

;; interp : fnWAE? (listof FunDef?) -> number?
(define (interp an-fnWAE fundefs)
  (type-case fnWAE an-fnWAE
    [num (n) n]
    [add (lhs rhs)
         (+ (interp lhs fundefs)
            (interp rhs fundefs))]
    [sub (lhs rhs)
         (- (interp lhs fundefs)
            (interp rhs fundefs))]
    [with (name named-expr body)
          (interp (subst body
                         name
                         (interp named-expr fundefs))
                  fundefs)]
    [id (name)
        (error 'interp "free identifier: ~a" name)]
    [app (fun-name arg-exprs)
         (define found-function (lookup-fundef fun-name fundefs))
         (define found-function-param-names (fundef-param-names found-function))
         (define body (fundef-body found-function))
         (unless (equal? (length arg-exprs) (length found-function-param-names))
           (error 'interp "wrong arity"))
         (define interpreted-arg-exprs
           (map (lambda (arg-expr) (interp arg-expr fundefs)) arg-exprs))
         (define paired-symbol-to-name
           (map cons found-function-param-names interpreted-arg-exprs))
         (define substituted-body
           (foldl (lambda (elem acc) (subst acc
                                            (car elem)
                                            (cdr elem)))
                  body
                  paired-symbol-to-name))
         (interp substituted-body fundefs)]))

;; lookup-fundef : symbol? (listof FunDef?) -> FunDef?

(define (lookup-fundef name fundefs)
  (cond [(empty? fundefs)
         (error 'interp "undefined function: ~a" name)]
        [(equal? name (fundef-fun-name (first fundefs)))
         (first fundefs)]
        [else
         (lookup-fundef name (rest fundefs))]))

;; subst : fnWAE? symbol? number? -> fnWAE?
(define (subst a-f1wae name value)
  (type-case fnWAE a-f1wae
    [num (n)
         a-f1wae]
    [add (l r)
         (add (subst l name value)
              (subst r name value))]
    [sub (l r)
         (sub (subst l name value)
              (subst r name value))]
    [with (name2 named-expr body)
          (with name2 (subst named-expr name value)
                (if (equal? name name2)
                    body
                    (subst body name value)))]
    [id (name2)
        (if (equal? name name2)
            (num value)
            a-f1wae)]
    [app (f-name arg-exprs)
         (app f-name
         (map (lambda (x) (subst x name value)) arg-exprs))]))


;; parse, parse-defn, interp tests, functions with multiple arguments: 
(test (interp (parse `{f 10})
              (list (fundef 'f '(x) ; 
                            (parse `{- 20 {twice {twice x}}}))
                    (fundef 'twice '(y) 
                            (parse `{+ y y})))) -20)

(test (interp (parse '{f 20 {+ 1 3}})
              (list (parse-defn '{deffun {f x y} {+ x y}})))
      24)

(test (interp (parse '{+ {f 10 20 30} {g 50 20}})
              (list (parse-defn '{deffun {f x y z} {+ x {+ y z}}})
                    (parse-defn '{deffun {g a b} {- a b}})))
      90)

(test (interp (parse '{+ {f 10 20} {g 30 {h 40 50}}})
              (list (parse-defn '{deffun {f x y} {+ x y}})
                    (parse-defn '{deffun {g a b} {- a b}})
                    (parse-defn '{deffun {h p q} {+ p q}})))
      -30)

;; errors: undefined function, bad syntax, wrong arrity, free identifier 

(test/exn (interp (parse '{with {x y} 1})
                  (list))
          "free identifier")

(test/exn (interp (parse '{f 1 2})
                  (list (parse-defn '{deffun {f x x} {+ x x}})))
          "bad syntax")

(test/exn (interp (parse '{f x})
                  (list (parse-defn '{deffun {g a b c} c})))
          "undefined function")

(test/exn (interp (parse '{f 1})
                  (list (parse-defn '{deffun {f x y} {+ x y}})))
          "wrong arity")

(test/exn (interp (parse '{g 3 4})
                  (list (parse-defn '{deffun {g a b a} {+ a b}})))
          "bad syntax")

(test/exn (interp (parse '{with {x 3} {+ x y}}) (list))
          "free identifier")

(test/exn (interp (parse '{with {x {+ 1 2}} {+ x z}}) (list))
          "free identifier")

(test/exn (interp (parse '{f 10 20}) (list (parse-defn '{deffun {f x z} {+ x y}})))
          "free identifier")

(test/exn (interp (parse '{f 10}) (list))
          "undefined function")

(test/exn (interp (parse '{h 5 10})
                  (list (parse-defn '{deffun {f x} {+ x 1}})))
          "undefined function")

(test/exn (interp (parse '{f 1 2})
                  (list (parse-defn '{deffun {f x y z} {+ x y}})))
          "wrong arity")

(test/exn (interp (parse '{f})
                  (list (parse-defn '{deffun {f x y} {+ x y}})))
          "wrong arity")


;; provided test cases: interp, parse, with, subst 

;; 5 -> 5
(test (interp (parse `5) '())
      5)
;; {+ 1 2} -> 3
(test (interp (parse `{+ 1 2}) '())
      3)
;; {- 3 4} -> -1
(test (interp (parse `{- 3 4}) '())
      -1)
;; {+ {+ 1 2} {- 3 4}} -> 2
(test (interp (parse `{+ {+ 1 2} {- 3 4}}) '())
      2)

#|
{with {x {+ 1 2}}
      {+ x x}}
|#
(test (interp (parse `{with {x {+ 1 2}}
                            {+ x x}})
              '())
      6)
#|
x
|#
(test/exn (interp (parse `x) '())
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
              '())
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
              '())
      8)
#|
{with {x {+ 1 2}}
      {with {x {- 4 3}}
            {+ x x}}}
|#
(test (interp (parse `{with {x {+ 1 2}}
                            {with {x {- 4 3}}
                                  {+ x x}}})
              '())
      2)
#|
{with {x {+ 1 2}}
      {with {y {- 4 3}}
            {+ x x}}}
|#
(test (interp (parse `{with {x {+ 1 2}}
                            {with {y {- 4 3}}
                                  {+ x x}}})
              '())
      6)


#|
substitute 10 for x in {+ 1 x}
|#
(test (subst (add (num 1) (id 'x))
             'x
             10)
      (add (num 1) (num 10)))
#|
substitute 10 for x in y
|#
(test (subst (id 'y) 'x 10)
      (id 'y))
#|
substitute 10 for x in {- 1 x}
|#
(test (subst (sub (num 1) (id 'x))
             'x
             10)
      (sub (num 1) (num 10)))
#|
substitute 10 for x in {with {y 17} x}
|#
(test (subst (with 'y (num 17) (id 'x))
             'x
             10)
      (with 'y (num 17) (num 10)))
#|
substitute 10 for x in {with {y x} y}
|#
(test (subst (with 'y (id 'x) (id 'y))
             'x
             10)
      (with 'y (num 10) (id 'y)))
#|
substitute 10 for x in {with {x y} x}
|#
(test (subst (with 'x (id 'y) (id 'x))
             'x
             10)
      (with 'x (id 'y) (id 'x)))

