#lang plai
(print-only-errors)

(define-type RCFAE
  [num (n number?)]
  [add (lhs RCFAE?)
       (rhs RCFAE?)]
  [sub (lhs RCFAE?)
       (rhs RCFAE?)]
  [id (name symbol?)]
  [fun (param-name symbol?)
       (body RCFAE?)]
  [app (fun-expr RCFAE?)
       (arg-expr RCFAE?)]
  [if0 (test-expr RCFAE?)
       (then-expr RCFAE?)
       (else-expr RCFAE?)]
  [rec (name symbol?)
       (named-expr RCFAE?)
       (body RCFAE?)])

(define-type RCFAE-Value
  [numV (n number?)]
  [closureV (param-name symbol?)
            (body RCFAE?)
            (ds DefSub?)])

(define-type DefSub
  [mtSub]
  [aSub  (name symbol?)
         (value RCFAE-Value?)
         (rest DefSub?)]
  [aRecSub  (name symbol?)
            (value-box (box/c RCFAE-Value?))
            (rest DefSub?)])

;; ----------------------------------------------------------------------

;; parse : s-expression -> RCFAE?
(define (parse s-exp)
  (cond [(number? s-exp)
         (num s-exp)]
        [(symbol? s-exp)
         (id s-exp)]
        [(list? s-exp)
         (when (empty? s-exp)
           (error 'parse "the empty list is not a valid RCFAE"))
         (case (first s-exp)
           [(+)
            (check-pieces s-exp "add" 3)
            (add (parse (second s-exp))
                 (parse (third s-exp)))]
           [(-)
            (check-pieces s-exp "sub" 3)
            (sub (parse (second s-exp))
                 (parse (third s-exp)))]
           [(fun)
            (check-pieces s-exp "fun" 3)
            (check-pieces (second s-exp) "parameter list" 1)
            (fun (first (second s-exp))
                 (parse (third s-exp)))]
           [(if0)
            (check-pieces s-exp "if0" 4)
            (if0 (parse (second s-exp))
                 (parse (third  s-exp))
                 (parse (fourth s-exp)))]
           [(rec)
            (check-pieces s-exp "rec" 3)
            (check-pieces (second s-exp) "binding pair" 2)
            (rec (first (second s-exp))
                 (parse (second (second s-exp)))
                 (parse (third s-exp)))]
           [else
            (check-pieces s-exp "app" 2)
            (app (parse (first s-exp))
                 (parse (second s-exp)))])]
        [else
         (error 'parse "not an RCFAE")]))

(define (check-pieces s-exp expected n-pieces)
  (unless (and (list? s-exp)
               (= n-pieces (length s-exp)))
    (error 'parse "expected ~a got ~a" expected s-exp)))

;; ----------------------------------------------------------------------

;; interp : RCFAE? DefSub? -> RCFAE-Value?
(define (interp an-rcfae ds)
  (type-case RCFAE an-rcfae
    [num (n)   (numV n)]
    [add (l r) (numop + l r ds)]
    [sub (l r) (numop - l r ds)]
    [id (name) (lookup name ds)]
    [fun (param-name body) (closureV param-name body ds)]
    [app (fun-expr arg-expr)
         (define fun-val (interp fun-expr ds))
         (define arg-val (interp arg-expr ds))
         (type-case RCFAE-Value fun-val
           [closureV (param-name body closed-ds)
                     (interp body
                             (aSub param-name
                                   arg-val
                                   closed-ds))]
           [else (error 'interp "expected function")])]
    [if0 (tst thn els)
         (define test-val (interp tst ds))
         (if (and (numV? test-val)
                  (= 0 (numV-n test-val)))
             (interp thn ds)
             (interp els ds))]
    [rec (name named-expr body)
         (define value-holder (box (numV 42)))
         (define new-ds
           (aRecSub name value-holder ds))
         (define val (interp named-expr new-ds))
         (set-box! value-holder val)
         (interp body new-ds)]))

;; numop : (number? number? -> number?) BFAE? BFAE? DefSub? -> BFAE-Value?
(define (numop op l r ds)
  (define l-val (interp l ds))
  (unless (numV? l-val)
    (error 'interp "expected number, got ~a" l-val))
  (define r-val (interp r ds))
  (unless (numV? r-val)
    (error 'interp "expected number, got ~a" r-val))
  (numV (op (numV-n l-val) (numV-n r-val))))

;; lookup : symbol? DefSub? -> RCFAE-Value?
(define (lookup name ds)
  (type-case DefSub ds
    [mtSub () (error 'interp "free identifier")]
    [aSub (name2 val rest)
          (if (equal? name name2)
              val
              (lookup name rest))]
    [aRecSub (name2 val-box rest)
             (if (equal? name name2)
              (unbox val-box)
              (lookup name rest))]))

(define initial-def-sub (mtSub))

(test (interp
       (parse `{rec {fac
                     {fun {n}
                          {if0 n
                               0
                               {+ n {fac {- n 1}}}}}}
                 {fac 5}})
              initial-def-sub)
      (numV 15))

#;
(test (interp
       (parse `{rec {loop
                     {fun {n} {loop n}}}
                 {loop 0}})
       initial-def-sub)
      (numV 42))

#;
(test (interp
       (parse `{{fun {x} {x x}} {fun {x} {x x}}})
       initial-def-sub)
      (numV 42))

(test (interp (parse `{rec {x x} x})
              initial-def-sub)
      (numV 42))

;; ----------------------------------------------------------------------------
;; old tests, with `with` -> `rec`

(test (interp (parse `{fun {x} {+ x 1}})
              initial-def-sub)
      (closureV 'x (add (id 'x) (num 1))
                (mtSub)))
(test (interp (parse `{rec {y 3} {fun {x} {+ x y}}})
              initial-def-sub)
      (closureV 'x (add (id 'x) (id 'y))
                (aRecSub 'y (box (numV 3)) (mtSub))))
(test (interp (parse `{{rec {y 3} {fun {x} {+ x y}}}
                       5})
              initial-def-sub)
      (numV 8))
(test (interp (parse `{rec {y 100}
                           {{rec {y 3} {fun {x} {+ x y}}}
                            5}})
              initial-def-sub)
      (numV 8))


(test/exn (interp (parse `{rec {z {fun {x} {+ x y}}}
                                {rec {y 10}
                                     {z 3}}})
                  initial-def-sub)
          "free identifier")
;; A: 13 -- wrong
;; B: free identifier -- right

;; ----------

;; 5 -> 5
(test (interp (parse `5)
              initial-def-sub)
      (numV 5))
;; {+ 1 2} -> 3
(test (interp (parse `{+ 1 2})
              initial-def-sub)
      (numV 3))
;; {- 3 4} -> -1
(test (interp (parse `{- 3 4})
              initial-def-sub)
      (numV -1))
;; {+ {+ 1 2} {- 3 4}} -> 2
(test (interp (parse `{+ {+ 1 2} {- 3 4}})
              initial-def-sub)
      (numV 2))

#|
{with {x {+ 1 2}}
      {+ x x}}
|#
(test (interp (parse `{rec {x {+ 1 2}}
                           {+ x x}})
              initial-def-sub)
      (numV 6))
#|
x
|#
(test/exn (interp (parse `x)
                  initial-def-sub)
          "free identifier")
#|
{+ {with {x {+ 1 2}}
         {+ x x}}
   {with {x {- 4 3}}
         {+ x x}}}
|#
(test (interp (parse `{+ {rec {x {+ 1 2}}
                              {+ x x}}
                         {rec {x {- 4 3}}
                              {+ x x}}})
              initial-def-sub)
      (numV 8))
#|
{+ {with {x {+ 1 2}}
         {+ x x}}
   {with {y {- 4 3}}
         {+ y y}}}
|#
(test (interp (parse `{+ {rec {x {+ 1 2}}
                              {+ x x}}
                         {rec {y {- 4 3}}
                              {+ y y}}})
              initial-def-sub)
      (numV 8))
#|
{with {x {+ 1 2}}
      {with {x {- 4 3}}
            {+ x x}}}
|#
(test (interp (parse `{rec {x {+ 1 2}}
                           {rec {x {- 4 3}}
                                {+ x x}}})
              initial-def-sub)
      (numV 2))
#|
{with {x {+ 1 2}}
      {with {y {- 4 3}}
            {+ x x}}}
|#
(test (interp (parse `{rec {x {+ 1 2}}
                           {rec {y {- 4 3}}
                                {+ x x}}})
              initial-def-sub)
      (numV 6))

;; ----------

(test (interp (parse `{rec {f {fun {x} {+ x 1}}}
                           {f 3}})
              initial-def-sub)
      (numV 4))
(test (interp (parse `{{fun {x} {+ x 1}} 3})
              initial-def-sub)
      (numV 4))
(test (interp (parse `{fun {x} {+ x 1}})
              initial-def-sub)
      (closureV 'x (parse `{+ x 1}) (mtSub)))
(test/exn (interp (parse `{1 2})
                  initial-def-sub)
          "expected function")
(test/exn (interp (parse `{+ 1 {fun {x} x}})
                  initial-def-sub)
          "expected number")
(test (interp (parse `{rec {f {rec {x 3}
                                   {fun {y} {+ x y}}}}
                           {f 2}})
              initial-def-sub)
      (numV 5))