#lang plai
(print-only-errors)

(define-type KFAE
  [num (n number?)]
  [add (lhs KFAE?)
       (rhs KFAE?)]
  [sub (lhs KFAE?)
       (rhs KFAE?)]
  [id (name symbol?)]
  [fun (param-name symbol?)
       (body KFAE?)]
  [app (fun-expr KFAE?)
       (arg-expr KFAE?)]
  [ret-0]
  [ret (ret-expr KFAE?)])

(define-type KFAE-Value
  [numV (n number?)]
  [closureV (param-name symbol?)
            (body KFAE?)
            (ds DefSub?)])

(define-type DefSub
  [mtSub]
  [aSub (name symbol?)
        (value KFAE-Value?)
        (rest DefSub?)])

(define-type Cont
  [numop-do-right  (rhs KFAE?)
                   (ds  DefSub?)
                   (op (-> number? number? number?))
                   (rest-k Cont?)]
  [numop-do-op     (l-val KFAE-Value?)
                   (op (-> number? number? number?))
                   (rest-k Cont?)]
  [app-do-arg      (arg-expr KFAE?)
                   (ds  DefSub?)
                   (rest-k Cont?)]
  [app-do-body     (fun-val KFAE-Value?)
                   (rest-k Cont?)]
  [app-do-return   (rest-k Cont?)]
  [do-early-return (rest-k Cont?)]
  [done])

;; ----------------------------------------------------------------------

;; parse : s-expression -> KFAE?
(define (parse s-exp)
  (cond [(number? s-exp)
         (num s-exp)]
        [(symbol? s-exp)
         (id s-exp)]
        [(list? s-exp)
         (when (empty? s-exp)
           (error 'parse "the empty list is not a valid KFAE"))
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
           [(with) ; in lieu of a compiler
            (check-pieces s-exp "with" 3)
            (check-pieces (second s-exp) "with binding pair" 2)
            (unless (symbol? (first (second s-exp)))
              (error 'parse "expected variable name, got ~a" (first (second s-exp))))
            (app (fun (first (second s-exp)) (parse (third s-exp)))
                 (parse (second (second s-exp))))]
           [(ret-0)
            (check-pieces s-exp "ret-0" 1)
            (ret-0)]
           [(ret)
            (check-pieces s-exp "ret" 2)
            (ret (parse (second s-exp)))]
           [else
            (check-pieces s-exp "app" 2)
            (app (parse (first s-exp))
                 (parse (second s-exp)))])]
        [else
         (error 'parse "wat")]))

(define (check-pieces s-exp expected n-pieces)
  (unless (and (list? s-exp)
               (= n-pieces (length s-exp)))
    (error 'parse "expected ~a got ~a" expected s-exp)))

;; ----------------------------------------------------------------------

;; interp-test : s-expression -> KFAE-Value?
(define (interp-test s-exp)
  (interp (parse s-exp) (mtSub) (done)))

(require racket/pretty)
(define (log-interp a-kfae k)
  (printf "interp:\na-kfae: ~a\nk: ~a\n\n"
          (pretty-format a-kfae)
          (pretty-format k)))

;; interp : KFAE? DefSub? Cont? -> KFAE-Value?
(define (interp a-kfae ds k)
  (type-case KFAE a-kfae
    [num (n) (interp-cont (numV n) k)]
    [id (name) (interp-cont (lookup name ds) k)]
    [add (l r) (numop + l r ds k)]
    [sub (l r) (numop - l r ds k)]
    [fun (param-name body) (interp-cont (closureV param-name body ds) k)]
    [app (fun-expr arg-expr)
         (interp fun-expr ds
                 (app-do-arg arg-expr ds k))]
    [ret-0 ()
           (return (numV 0) k)]
    [ret (ret-expr)
         (interp ret-expr ds
                 (do-early-return k))]))

(define (return ret-val k)
  (type-case Cont k
    [done () (error 'interp "not inside a function")]
    [numop-do-right (r op ds rest-k)
                    (return ret-val rest-k)]
    [numop-do-op (l-val op rest-k)
                 (return ret-val rest-k)]
    [app-do-arg (arg-expr ds rest-k)
                (return ret-val rest-k)]
    [app-do-body (fun-val rest-k)
                 (return ret-val rest-k)]
    [app-do-return (rest-k)
                   (interp-cont ret-val rest-k)]
    [do-early-return (rest-k)
                     (return ret-val rest-k)]))

;; numop : (number? number? -> number?) KFAE? KFAE? DefSub? Cont? -> KFAE-Value?
(define (numop op l r ds k)
  (interp l ds
          (numop-do-right r ds op k)))

(define (log-interp-cont v k)
  (printf "interp-cont:\nv: ~a\nk: ~a\n\n"
          (pretty-format v)
          (pretty-format k)))

;; interp-cont : KFAE-Value? Cont? -> KFAE-Value?
(define (interp-cont v k)
  ;;(log-interp-cont v k)
  (type-case Cont k
    [done () v]
    [numop-do-right (r ds op rest-k)
                    (define l-val v)
                    (interp r ds
                            (numop-do-op l-val op rest-k))]
    [numop-do-op (l-val op rest-k)
                 (define r-val v)
                 (unless (and (numV? l-val)
                              (numV? r-val))
                   (error 'interp "expected number"))
                 (interp-cont (numV (op (numV-n l-val) (numV-n r-val)))
                              rest-k)]
    [app-do-arg (arg-expr ds rest-k)
                (define fun-val v)
                (interp arg-expr ds
                        (app-do-body fun-val rest-k))]
    [app-do-body (fun-val rest-k)
                 (define arg-val v)
                 (type-case KFAE-Value fun-val
                   [closureV (param-name body ds)
                             (interp body
                                     (aSub param-name
                                           arg-val
                                           ds)
                                     (app-do-return rest-k))]
                   [else (error 'interp "expected function")])]
    [app-do-return (rest-k)
                   (define ret-val v)
                   (interp-cont ret-val rest-k)]
    [do-early-return (rest-k)
                     (define ret-val v)
                     (return ret-val rest-k)]))

;; lookup : symbol? DefSub? -> KFAE-Value?
(define (lookup name ds)
  (type-case DefSub ds
    [mtSub () (error "free identifier")]
    [aSub (name2 value rest)
          (if (equal? name name2)
              value
              (lookup name rest))]))

;; ----------------------------------------------------------------------

(test (interp-test `{{fun {x} {+ x {ret-0}}}
                     5})
      (numV 0))

(test (interp-test `{+ {{fun {x} {+ x {ret-0}}}
                        5}
                       3})
      (numV 3))

(test/exn (interp-test `{ret-0})
          "not inside a function")

(test (interp-test `{with {f {fun {x} {+ 3 {ret-0}}}}
                          {with {g {fun {y} {+ 10 {f y}}}}
                                {g 8}}})
      (numV 10))

(test (interp-test `{{fun {x} {+ x {ret 3}}}
                     5})
      (numV 3))

(test (interp-test `{+ {{fun {x} {+ x {ret {- 18 10}}}}
                        5}
                       10})
      (numV 18))

(test/exn (interp-test `{ret 5})
          "not inside a function")

(test (interp-test `{with {f {fun {x} {+ 3 {ret x}}}}
                          {with {g {fun {y} {+ 10 {ret {f y}}}}}
                                {g 8}}})
      (numV 8))

(test (interp-test `{{fun {x} {+ x {ret {ret 2}}}}
                     5})
      (numV 2))

;; ----------------------------------------------------------------------

(test (interp-test `{fun {x} {+ x 1}})
      (closureV 'x (add (id 'x) (num 1))
                (mtSub)))
(test (interp-test `{with {y 3} {fun {x} {+ x y}}})
      (closureV 'x (add (id 'x) (id 'y))
                (aSub 'y (numV 3) (mtSub))))
(test (interp-test `{{with {y 3} {fun {x} {+ x y}}}
                     5})
      (numV 8))
(test (interp-test `{with {y 100}
                          {{with {y 3} {fun {x} {+ x y}}}
                           5}})
      (numV 8))

(test/exn (interp-test `{with {z {fun {x} {+ x y}}}
                              {with {y 10}
                                    {z 3}}})
          "free identifier")
;; A: 13 -- wrong
;; B: free identifier -- right

;; ----------

;; 5 -> 5
(test (interp-test `5)
      (numV 5))
;; {+ 1 2} -> 3
(test (interp-test `{+ 1 2})
      (numV 3))
;; {- 3 4} -> -1
(test (interp-test `{- 3 4})
      (numV -1))
;; {+ {+ 1 2} {- 3 4}} -> 2
(test (interp-test `{+ {+ 1 2} {- 3 4}})
      (numV 2))

#|
{with {x {+ 1 2}}
      {+ x x}}
|#
(test (interp-test `{with {x {+ 1 2}}
                          {+ x x}})
      (numV 6))
#|
x
|#
(test/exn (interp-test `z)
          "free identifier")
#|
{+ {with {x {+ 1 2}}
         {+ x x}}
   {with {x {- 4 3}}
         {+ x x}}}
|#
(test (interp-test `{+ {with {x {+ 1 2}}
                             {+ x x}}
                       {with {x {- 4 3}}
                             {+ x x}}})
      (numV 8))
#|
{+ {with {x {+ 1 2}}
         {+ x x}}
   {with {y {- 4 3}}
         {+ y y}}}
|#
(test (interp-test `{+ {with {x {+ 1 2}}
                             {+ x x}}
                       {with {y {- 4 3}}
                             {+ y y}}})
      (numV 8))
#|
{with {x {+ 1 2}}
      {with {x {- 4 3}}
            {+ x x}}}
|#
(test (interp-test `{with {x {+ 1 2}}
                          {with {x {- 4 3}}
                                {+ x x}}})
      (numV 2))
#|
{with {x {+ 1 2}}
      {with {y {- 4 3}}
            {+ x x}}}
|#
(test (interp-test `{with {x {+ 1 2}}
                          {with {y {- 4 3}}
                                {+ x x}}})
      (numV 6))

;; ----------

(test (interp-test `{with {f {fun {x} {+ x 1}}}
                          {f 3}})
      (numV 4))
(test (interp-test `{{fun {x} {+ x 1}} 3})
      (numV 4))
(test (interp-test `{fun {x} {+ x 1}})
      (closureV 'x (parse `{+ x 1}) (mtSub)))
(test/exn (interp-test `{1 2})
          "expected function")
(test/exn (interp-test `{+ 1 {fun {x} x}})
          "expected number")
(test (interp-test `{with {f {with {x 3}
                                   {fun {y} {+ x y}}}}
                          {f 2}})
      (numV 5))
