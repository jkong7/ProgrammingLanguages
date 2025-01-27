#lang plai

(print-only-errors)

(define-type FWAE
  [num  (n number?)]
  [add  (lhs FWAE?)
        (rhs FWAE?)]
  [sub  (lhs FWAE?)
        (rhs FWAE?)]
  [with (name symbol?)
        (named-expr FWAE?)
        (body FWAE?)]
  [id   (name symbol?)]
  [fun  (param-name symbol?)
        (body FWAE?)]
  [app  (fun-expr FWAE?)
        (arg-expr FWAE?)])

(define-type FWAE-Value
  [numV (n number?)]
  [closureV (param-name symbol?)
            (body FWAE?)
            (ds DefSub?)])

(define-type DefSub
  [mtSub]
  [aSub (name  symbol?)
        (value FWAE-Value?)
        (rest  DefSub?)])

;; parse : s-expr -> FWAE?
(define (parse s-expr)
  (cond [(number? s-expr)
         (num s-expr)]
        [(symbol? s-expr)
         (id s-expr)]
        [(list? s-expr)
         (when (empty? s-expr)
           (error 'parse "the empty list is not a valid FWAE"))
         (case (first s-expr)
           [(+)
            (check-pieces s-expr 3 "+")
            (add (parse (second s-expr))
                 (parse (third s-expr)))]
           [(-)
            (check-pieces s-expr 3 "-")
            (sub (parse (second s-expr))
                 (parse (third s-expr)))]
           [(with)
            (check-pieces s-expr 3 "with")
            (check-pieces (second s-expr) 2 "with binding pair")
            (unless (symbol? (first (second s-expr)))
              (error 'parse "expected variable name, got ~a" (first (second s-expr))))
            (with (first (second s-expr))
                  (parse (second (second s-expr)))
                  (parse (third s-expr)))]
           [(fun)
            (check-pieces s-expr 3 "fun")
            (check-pieces (second s-expr) 1 "parameter list")
            (fun (first (second s-expr))
                 (parse (third s-expr)))]
           [else
            (check-pieces s-expr 2 "app")
            #;(unless (symbol? (first s-expr))
              (error 'parse "expected identifier as function name"))
            (app
             (parse (first s-expr)) #;(first s-expr)
             (parse (second s-expr)))])]
        [else
         (error 'parse "expected FWAE, got ~a" s-expr)]))

(define (check-pieces s-expr n who)
  (unless (and (list? s-expr) (= (length s-expr) n))
    (error 'parse "expected ~a, got ~a" who s-expr)))

;; ----------------------------------------------------------------------------

;; interp : FWAE? DefSub? -> FWAE-Value?
(define (interp an-fwae ds)
  (type-case FWAE an-fwae
    [num (n) (numV n)]
    [add (l r)
         (num-op + l r ds)]
    [sub (l r)
         (num-op - l r ds)]
    [with (name named-expr body)
          (define new-ds (aSub name (interp named-expr ds) ds))
          (interp body new-ds)]
    [id (name) (lookup name ds)]
    [fun (param-name body) (closureV param-name body ds)]      
    [app  (fun-expr arg-expr)
          (define fun-val (interp fun-expr ds))
          (unless (closureV? fun-val)
            (error 'interp "expected a function, got ~a:" fun-val))
          (define body (closureV-body fun-val))
          (define param-name (closureV-param-name fun-val))
          (define body-ds (closureV-ds fun-val))
          (define new-ds (aSub param-name (interp arg-expr ds) body-ds))
          (interp body new-ds)]))
    

;; numop: (number? number? -> number?)
;;        FWAE?
;;        FWAE?
;;        DefSub?
;;        ->
;;        FWAE-Value)
(define (num-op op l r ds)
  (define l-val (interp l ds))
  (unless (numV? l-val)
    (error 'interp "expected a number, got ~a" l-val))
  (define r-val (interp r ds))
  (unless (numV? r-val)
    (error 'interp "expected a number, got ~a" r-val))
  (define result-as-number (op (numV-n l-val) (numV-n r-val)))
  (numV result-as-number))

;; lookup : symbol? DefSub? -> FWAE-Value?
(define (lookup name ds)
  (type-case DefSub ds
    [mtSub () (error 'interp "free identifier")]
    [aSub (name2 val rest)
          (if (equal? name name2)
              val
              (lookup name rest))]))

;; ----------------------------------------------------------------------------

(define initial-def-sub (mtSub))

(test (interp (parse `{with {f {fun {x} {+ x 1}}}
                            {f 3}})
              initial-def-sub)
      (numV 4))

(test (interp (parse `{{fun {x} {+ x 1}} 3})
              initial-def-sub)
      (numV 4))

(test (interp (parse `{fun {x} {+ x 1}})
              initial-def-sub)
      (closureV 'x (add (id 'x) (num 1)) (mtSub)))

(test/exn (interp (parse `{1 2})
              initial-def-sub)
      "expected a function")

(test/exn (interp (parse `{+ 1 {fun {x} 10}})
              initial-def-sub)
      "expected a number")

(test (interp (parse `{with {f {with {x 3}
                                     {fun {y} {+ x y}}}}
                            {f 5}})
              initial-def-sub)
      (numV 8))

(test/exn (interp (parse `{with {z {fun {x} {+ x y}}}
                            {with {y 10} {z y}}})
              initial-def-sub)
     "free identifier")




(test (interp (parse `{{fun {x}
                           {fun {y}
                                {+ x y}}}
                       10})
              initial-def-sub)
      (closureV 'y (add (id 'x) (id 'y))
                (aSub 'x (numV 10) (mtSub))))


(test (interp (parse `{{{fun {x} 
                              {fun {y} 
                                   {+ x y}}} 
                          10} 
                      5})
              initial-def-sub)
      (numV 15))


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
(test (interp (parse `{with {x {+ 1 2}}
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
(test (interp (parse `{+ {with {x {+ 1 2}}
                               {+ x x}}
                         {with {x {- 4 3}}
                               {+ x x}}})
              initial-def-sub)
      (numV 8))
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
              initial-def-sub)
      (numV 8))
#|
{with {x {+ 1 2}}
      {with {x {- 4 3}}
            {+ x x}}}
|#
(test (interp (parse `{with {x {+ 1 2}}
                            {with {x {- 4 3}}
                                  {+ x x}}})
              initial-def-sub)
      (numV 2))
#|
{with {x {+ 1 2}}
      {with {y {- 4 3}}
            {+ x x}}}
|#
(test (interp (parse `{with {x {+ 1 2}}
                            {with {y {- 4 3}}
                                  {+ x x}}})
              initial-def-sub)
      (numV 6))


;; {with {x FWAE_1} FWAE_2} is the same as {{fun {x} FWAE_2} FWAE_1}
(test (interp (parse `{with {x 41} {+ x 1}})
              initial-def-sub)
      (interp (parse `{{fun {x} {+ x 1}} 41})
              initial-def-sub))

