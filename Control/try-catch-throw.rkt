#lang plai

; Jonathan Kong
; CS 321
; Winter 2025
; Homework 5

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

(define-type EFAE
  [num (n number?)]
  [add (lhs EFAE?)
       (rhs EFAE?)]
  [sub (lhs EFAE?)
       (rhs EFAE?)]
  [id (name symbol?)]
  [fun (param-name symbol?)
       (body EFAE?)]
  [app (fun-expr EFAE?)
       (arg-expr EFAE?)]
  [if0 (tst EFAE?)
       (thn EFAE?)
       (els EFAE?)]
  [throw (tag symbol?)
         (throw-expr EFAE?)]
  [try-catch (try-body EFAE?)
             (tag symbol?)
             (exn-name symbol?)
             (catch-body EFAE?)])

(define-type EFAE-Value
  [numV (n number?)]
  [closureV (param-name symbol?)
            (body EFAE?)
            (ds DefSub?)])

(define-type DefSub
  [mtSub]
  [aSub (name symbol?)
        (value EFAE-Value?)
        (rest DefSub?)])

(define-type Cont
  [numop-do-right  (rhs EFAE?)
                   (ds  DefSub?)
                   (op (-> number? number? number?))
                   (rest-k Cont?)]
  [numop-do-op     (l-val EFAE-Value?)
                   (op (-> number? number? number?))
                   (rest-k Cont?)]
  [app-do-arg      (arg-expr EFAE?)
                   (ds  DefSub?)
                   (rest-k Cont?)]
  [app-do-body     (fun-val EFAE-Value?)
                   (rest-k Cont?)]
  [app-do-return   (rest-k Cont?)]
  [do-if0 (els EFAE?)
          (thn EFAE?)
          (ds DefSub?)
          (rest-k Cont?)]
  [do-try-catch    (expected-tag symbol?) 
                   (exn-name symbol?)
                   (catch-body EFAE?)
                   (rest-k Cont?)]
  [do-early-throw  (throw-tag symbol?) 
                   (rest-k Cont?)]
  [done])

;; ----------------------------------------------------------------------

;; parse : s-expression -> EFAE?
(define (parse s-exp)
  (cond [(number? s-exp)
         (num s-exp)]
        [(symbol? s-exp)
         (id s-exp)]
        [(list? s-exp)
         (when (empty? s-exp)
           (error 'parse "the empty list is not a valid EFAE"))
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
           [(if0)
            (check-pieces s-exp "if0" 4)
            (if0 (parse (second s-exp))
                 (parse (third s-exp))
                 (parse (fourth s-exp)))]
           [(throw)
            (check-pieces s-exp "throw" 3)
            (unless (symbol? (second s-exp))
              (error 'parse "expected variable name, got ~a" (second s-exp)))
            (throw (second s-exp)
                   (parse (third s-exp)))]
           [(try)
            (check-pieces s-exp "try-catch" 3)
            (define catch-list (third s-exp))
            (unless (and (list? catch-list)
                         (= (length catch-list) 3))
              (error 'parse "expected proper catch body, got ~a" catch-list))
            (define tag-list (second catch-list))
            (unless (and (list? tag-list)
                         (= (length tag-list) 4))
              (error 'parse "expected proper tag body, got ~a" tag-list))
            (unless (and (symbol? (second tag-list))
                         (symbol? (fourth tag-list)))
              (error 'parse "expected symbols, got ~a and ~a" (second tag-list) (fourth tag-list)))
            (try-catch (parse (second s-exp))
                       (second tag-list)
                       (fourth tag-list)
                       (parse (third catch-list)))]
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

;; interp-test : s-expression -> EFAE-Value?
(define (interp-test s-exp)
  (interp (parse s-exp) (mtSub) (done)))

(require racket/pretty)
(define (log-interp a-kfae k)
  (printf "interp:\na-efae: ~a\nk: ~a\n\n"
          (pretty-format a-kfae)
          (pretty-format k)))

;; interp : EFAE? DefSub? Cont? -> EFAE-Value?
(define (interp a-efae ds k)
  (type-case EFAE a-efae
    [num (n) (interp-cont (numV n) k)]
    [id (name) (interp-cont (lookup name ds) k)]
    [add (l r) (numop + l r ds k)]
    [sub (l r) (numop - l r ds k)]
    [fun (param-name body) (interp-cont (closureV param-name body ds) k)]
    [app (fun-expr arg-expr)
         (interp fun-expr ds
                 (app-do-arg arg-expr ds k))]
    [if0 (tst els thn)
         (interp tst ds
                 (do-if0 els thn ds k))]
    [throw (tag throw-expr)
           (interp throw-expr ds
                   (do-early-throw tag k))]
    [try-catch (try-body tag exn-name catch-body)
               (interp try-body ds
                       (do-try-catch tag exn-name catch-body k))]))

(define (return ret-val tag k)
  (type-case Cont k
    [done () (error 'interp "missing catch")]
    [numop-do-right (r op ds rest-k)
                    (return ret-val tag rest-k)]
    [numop-do-op (l-val op rest-k)
                 (return ret-val tag rest-k)]
    [app-do-arg (arg-expr ds rest-k)
                (return ret-val tag rest-k)]
    [app-do-body (fun-val rest-k)
                 (return ret-val tag rest-k)]
    [app-do-return (rest-k)
                   (return ret-val tag rest-k)]
    [do-if0 (els thn ds rest-k)
            (return ret-val tag rest-k)]
    [do-early-throw (throw-tag rest-k)
                    (return ret-val tag rest-k)]
    [do-try-catch (expected-tag exn-name catch-body rest-k)
                  (if (equal? expected-tag tag)
                      (interp catch-body
                              (aSub exn-name
                                    ret-val
                                    (mtSub))
                              rest-k)
                      (return ret-val tag rest-k))]))

;; numop : (number? number? -> number?) EFAE? EFAE? DefSub? Cont? -> EFAE-Value?
(define (numop op l r ds k)
  (interp l ds
          (numop-do-right r ds op k)))

(define (log-interp-cont v k)
  (printf "interp-cont:\nv: ~a\nk: ~a\n\n"
          (pretty-format v)
          (pretty-format k)))

;; interp-cont : EFAE-Value? Cont? -> EFAE-Value?
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
                 (type-case EFAE-Value fun-val
                   [closureV (param-name body ds)
                             (interp body
                                     (aSub param-name
                                           arg-val
                                           ds)
                                     (app-do-return rest-k))]
                   [else (error 'interp "expected function")])]
    [do-if0 (els thn ds rest-k)
            (define test-value v)
            (type-case EFAE-Value v
              [numV (n)
                    (if (= n 0)
                        (interp els ds
                                rest-k)
                        (interp thn ds
                                rest-k))]
              [closureV (_1 _2 _3)
                        (interp thn ds
                                rest-k)])]
    [app-do-return (rest-k)
                   (define ret-val v)
                   (interp-cont ret-val rest-k)]
    [do-early-throw (throw-tag rest-k)
                    (define throw-value v)
                    (return throw-value throw-tag rest-k)]
    [do-try-catch (expected-tag exn-name catch-body rest-k)
                  (define ret-val v)
                  (interp-cont ret-val rest-k)]))


;; lookup : symbol? DefSub? -> EFAE-Value?
(define (lookup name ds)
  (type-case DefSub ds
    [mtSub () (error "free identifier")]
    [aSub (name2 value rest)
          (if (equal? name name2)
              value
              (lookup name rest))]))

;; interp-expr : EFAE? -> number? or 'function 
(define (interp-expr an-efae)
  (type-case EFAE-Value (interp an-efae (mtSub) (done))
    [numV (n) n]
    [closureV (a b c) 'function]))


;; written test cases

(test (interp-expr (parse '{try {+ 1 {try {+ 2 {throw z 3}}
                                          {catch {tag x as e} {+ 10 e}}}}
                                {catch {tag z as e} {+ 20 e}}}))
      23)

(test/exn (interp-expr (parse '{try {+ 5 {throw y 7}}
                                    {catch {tag x as e} {+ 10 e}}}))
          "missing catch")

(test (interp-expr (parse '{with {f {fun {x} {+ x {throw t 4}}}}
                                 {try {+ {f 6} 10}
                                      {catch {tag t as e}
                                             {try {throw y {+ 1 2}}
                                                  {catch {tag y as z} {+ z 7}}}}}}))
      10)

(test/exn (interp-expr (parse '{try {throw a 1}
                                    {catch {tag a as e} {+ x 2}}}))
          "free identifier")

(test/exn (interp-expr (parse '{try {throw a 1}
                                    {catch {tag a as e} {2 2}}}))
          "expected function")

(test/exn (interp-expr (parse '{try {throw a 1}
                                    {catch {tag a as e} {+ 2 {fun {x} {+ x 1}}}}}))
          "expected number")

(test (interp-expr (parse '{try {+ 5 10}
                                {catch {tag x as e} {+ e 3}}}))
      15)

(test (interp-expr (parse '{if0 0 1 2}))
      1)

(test (interp-expr (parse '{if0 {fun {x} x} 1 2}))
      2)

(test (interp-expr (parse '{if0 2 1 2}))
      2)

(test (interp-expr (parse '{if0 2 1 2}))
      2)

(test (interp-expr (parse '{if0 2 1 {try {throw x 8}
                                         {catch {tag x as e} {+ e 10}}}}))
      18)

(test (interp-expr (parse '{try {throw x 10}
                                {catch {tag x as e}
                                       {try {throw y 9}
                                            {catch {tag y as z}
                                                   {if0 z 2 {fun {x} x}}}}}}))
      'function)

(test (interp-expr (parse '{try {try {try {throw x 10}
                                          {catch {tag c as b} {+ b 8}}}
                                     {catch {tag y as z} {+ z 9}}}
                                {catch {tag x as e} {+ e 10}}}))
      20)

(test (interp-expr (parse '{try {try {try {throw x 10}
                                          {catch {tag c as b} {+ b 8}}}
                                     {catch {tag y as z} {+ z 9}}}
                                {catch {tag x as e} {if0 e 0
                                                         {fun {x} x}}}}))
      'function)

(test (interp-expr (parse '{try {try {try {throw x 10}
                                          {catch {tag c as b} {+ b 8}}}
                                     {catch {tag y as z} {+ z 9}}}
                                {catch {tag x as e} {if0 e 0
                                                         {{fun {x} x} 2}}}}))
      2)

(test/exn (interp-expr (parse '{try {try {try {throw x 10}
                                              {catch {tag c as b} {+ b 8}}}
                                         {catch {tag y as z} {+ z 9}}}
                                    {catch {tag g as e} {+ e 10}}}))
          "missing catch")

(test (interp-expr (parse '{+ {if0 0 {+ 10 6} 2} 5}))
      21)

(test (interp-expr (parse `(+ ((fun (x) (if0 x 2 3)) 0) 10)))
      12)

(test (interp-expr (parse `(try (if0 (throw a 1) 1 2) (catch (tag a as x) 4))))
      4)

(test (interp-expr
       (parse `(+ 2 (try (+ 1 (if0 (throw a 1) 1 2)) (catch (tag a as x) 4)))))
      6)

(test (interp-expr
       (parse `(if0 (try (+ 1 (if0 (throw a 1) 1 2)) (catch (tag a as x) 0)) 0 1)))
      0)

(test (interp-expr
       (parse
        `(try
          (try
           (if0
            (+
             2
             (try
              ((fun (f) (f 3)) (fun (x) (throw y 1)))
              (catch (tag a as e) (+ 5 e))))
            100
            101)
           (catch (tag z as e) (+ e 2)))
          (catch (tag y as e) (+ 10 e)))))
      11)

;; provided homework 5 test cases

(test (interp-expr (parse `{+ 2 {try {+ 4 {throw x 5}}
                                     {catch {tag x as e} {+ 3 e}}}}))
      10)


(test (interp-expr (parse `{try {+ 2 {try {+ 3 {throw y 5}}
                                          {catch {tag x as e} {+ 6 e}}}}
                                {catch {tag y as e} {+ 10 e}}}))
      15)

(test/exn (interp-expr (parse `{try {throw a 1} {catch {tag a as b} {throw a 1}}}))
          "missing catch")

(test (interp-expr (parse `{with {f {fun {x} {throw a {+ x 1}}}}
                                 {try {throw a {+ {f 3} 10}}
                                      {catch {tag a as j} {+ j 5}}}}))
      9)



;; ----------------------------------------------------------------------

#|

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
|#

;; prior provided tests
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