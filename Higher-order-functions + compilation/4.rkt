#lang plai

; Jonathan Kong
; CS 321
; Winter 2025
; Homework 3

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

(define-type FNWAE 
  [W-num  (n number?)]
  [W-add  (lhs FNWAE?)
          (rhs FNWAE?)]
  [W-sub  (lhs FNWAE?)
          (rhs FNWAE?)]
  [W-with (name symbol?)
          (named-expr FNWAE?)
          (body FNWAE?)]
  [W-id   (name symbol?)]
  [W-if0 (tst FNWAE?)
         (thn FNWAE?)
         (els FNWAE?)]
  [W-fun  (params (listof symbol?))
          (body FNWAE?)]
  [W-app  (fun-expr FNWAE?)
          (arg-exprs (listof FNWAE?))])

(define-type FAE
  [num  (n number?)]
  [add  (lhs FAE?)
        (rhs FAE?)]
  [sub  (lhs FAE?)
        (rhs FAE?)]
  [id   (name symbol?)]
  [if0 (test FAE?) (then FAE?) (else FAE?)]
  [fun  (param-name symbol?)
        (body FAE?)]
  [app  (fun-expr FAE?)
        (arg-expr FAE?)])

(define-type FAE-Value
  [numV (n number?)]
  [closureV (param-name symbol?)
            (body FAE?)
            (ds DefSub?)])

(define-type DefSub
  [mtSub]
  [aSub (name  symbol?)
        (value FAE-Value?)
        (rest  DefSub?)])

;; parse : s-expr -> FNWAE?


(define (parse s-expr)
  (cond [(number? s-expr)
         (W-num s-expr)]
        [(symbol? s-expr)
         (W-id s-expr)]
        [(list? s-expr)
         (when (empty? s-expr)
           (error 'parse "the empty list is not a valid FNWAE"))
         (case (first s-expr)
           [(+)
            (check-pieces s-expr 3 "+")
            (W-add (parse (second s-expr))
                   (parse (third s-expr)))]
           [(-)
            (check-pieces s-expr 3 "-")
            (W-sub (parse (second s-expr))
                   (parse (third s-expr)))]
           [(with)
            (check-pieces s-expr 3 "with")
            (check-pieces (second s-expr) 2 "with binding pair")
            (unless (symbol? (first (second s-expr)))
              (error 'parse "expected variable name, got ~a" (first (second s-expr))))
            (W-with (first (second s-expr))
                    (parse (second (second s-expr)))
                    (parse (third s-expr)))]
           [(if0)
            (check-pieces s-expr 4 "if0")
            (W-if0
             (parse (second s-expr))
             (parse (third s-expr))
             (parse (fourth s-expr)))]
           [(fun)
            (check-pieces s-expr 3 "fun")
            (unless (list? (second s-expr))
              (error 'parse "expected a list of parameters, got: ~a" (second s-expr)))
            (W-fun (second s-expr) (parse (third s-expr)))]
           [else
            (unless (list? s-expr)
              (error 'parse "expected a function application, got: ~a" s-expr))
            (W-app (parse (first s-expr))
                   (map parse (rest s-expr)))])]
        [else
         (error 'parse "expected FNWAE, got ~a" s-expr)]))

(define (check-pieces s-expr n who)
  (unless (and (list? s-expr) (= (length s-expr) n))
    (error 'parse "expected ~a, got ~a" who s-expr)))

;; ----------------------------------------------------------------------------

;; compile : FNWAE? -> FAE?
(define (compile an-fwae)
  ;; {with {x <FWAE 1>} <FWAE 2>}
  ;; {{fun x <FWAE 2>} <FWAE 1>}
  (type-case FNWAE an-fwae
    [W-num (n) (num n)]
    [W-id (name) (id name)]
    [W-add (l r)
           (try-constant-fold
            (add (compile l)
                 (compile r)))]
    [W-sub (l r)
           (try-constant-fold
            (sub (compile l)
                 (compile r)))]
    [W-if0 (tst thn els)
           (if0 (compile tst)
                (compile thn)
                (compile els))]
    [W-fun (listofparams body)
           (when (empty? listofparams)
             (error 'compile "nullary function"))
           (if (= (length listofparams) 1)
               (fun (first listofparams)
                    (compile body))
               (fun (first listofparams)
                    (compile (W-fun (rest listofparams)
                                    body))))]
    [W-app (f listofargs)
           (when (empty? listofargs)
             (error 'compile "nullary application"))
           (if (= (length listofargs) 1)
               (app (compile f)
                    (compile (first listofargs)))
               (app (compile (W-app f (take listofargs (- (length listofargs) 1))))
                    (compile (last listofargs))))]
    [W-with (name named-expr body)
            (app (fun name (compile body))
                 (compile named-expr))]))

;; Constant folding:
;; {+ 1 2} -> 3
;; {+ 1 {+ 2 3}} -> 6

;; FAE? -> FAE?
(define (try-constant-fold an-fae)
  ;; Local function definition: we can refer to `an-fae`
  (define (try-fold-op op l r)
    (if (and (num? l)
             (num? r))
        (num (op (num-n l) (num-n r)))
        an-fae))
  (type-case FAE an-fae
    [add (l r)
         (try-fold-op + l r)]
    [sub (l r)
         (try-fold-op - l r)]
    [else an-fae]))

;; compile and parse tests

(test (parse '{fun {x y} {+ x y}})
      (W-fun '(x y) (W-add (W-id 'x) (W-id 'y))))


(test (compile (W-with 'x (W-num 5) (W-add (W-id 'x) (W-num 1))))
      (app (fun 'x (add (id 'x) (num 1))) (num 5)))

(test (compile (W-fun '(x y) (W-add (W-id 'x) (W-id 'y))))
      (fun 'x (fun 'y (add (id 'x) (id 'y)))))

(test (compile (W-app (W-id 'f) (list (W-id 'x) (W-id 'y) (W-id 'z))))
      (app (app (app (id 'f) (id 'x)) (id 'y)) (id 'z)))

(test (compile
       (W-with 'f
               (W-fun '(x y) (W-add (W-id 'x) (W-id 'y)))
               (W-app (W-id 'f) (list (W-num 3) (W-num 4)))))
      (app (fun 'f (app (app (id 'f) (num 3)) (num 4)))
           (fun 'x (fun 'y (add (id 'x) (id 'y))))))

(test (compile (parse '{with {x 5} {+ x 1}}))
      (app (fun 'x (add (id 'x) (num 1))) (num 5)))


(test/exn (compile (W-fun '() (W-add (W-num 2) (W-num 1))))
          "nullary function")

(test/exn (compile (W-app (W-id 'f) '()))
          "nullary application")

(test/exn (compile (W-app (W-fun '(x) (W-sub (W-num 2) (W-num 2))) '()))
          "nullary application")



;; provided compile and parse tests

(test (compile (W-add (W-num 1) (W-num 2)))
      (num 3))
(test (compile (parse `{+ 1 2}))
      (num 3))
(test (compile (parse `{with {x 3} {+ x 2}}))
      (app (fun 'x (add (id 'x) (num 2)))
           (num 3)))
(test (compile (parse `{+ 2 {with {x 3} {+ x 2}}}))
      (add (num 2)
           (app (fun 'x (add (id 'x) (num 2)))
                (num 3))))
(test (compile (parse `{with {x 3} {with {y 2} {+ x y}}}))
      (app (fun 'x (app (fun 'y (add (id 'x) (id 'y)))
                        (num 2)))
           (num 3)))

(test (compile (parse '{+ 1 {+ 2 3}}))
      (num 6))
(test (compile (parse '{f {+ 1 2}}))
      (app (id 'f) (num 3)))


(test (compile (parse `{+ 2 {+ 3 x}}))
      (add (num 2)
           (add (num 3) (id 'x))))
(test (compile (parse `{+ x {+ 2 3}}))
      (add (id 'x)
           (num 5)))
(test (compile (parse `{f {+ 2 3}}))
      (app (id 'f)
           (num 5)))


;; --------------------------------------------------------------------------------------------

;; interp : FAE? DefSub? -> FAE-Value? 
(define (interp an-fae ds)
  (type-case FAE an-fae
    [num (n) (numV n)]
    [add (l r) (numop + l r ds)]
    [sub (l r) (numop - l r ds)]
    [id (name) (lookup name ds)]
    [fun (param-name body) (closureV param-name body ds)]
    [if0 (cond then else)
         (type-case FAE-Value (interp cond ds)
           [closureV (param-name body ds) (interp else ds)]
           [numV (n) (if (= n 0)
                         (interp then ds)
                         (interp else ds))])]
    [app (fun-expr arg-expr)
         (define fun-val (interp fun-expr ds))
         (define arg-val (interp arg-expr ds))
         (type-case FAE-Value fun-val
           [closureV (param-name body closed-ds)
                     (interp body
                             (aSub param-name
                                   arg-val
                                   closed-ds))]
           [else (error 'interp "expected function, got ~a" fun-val)])]))

(define (numop op l r ds)
  (define l-val (interp l ds))
  (define r-val (interp r ds))
  (unless (and (numV? l-val) (numV? r-val))
    (error 'interp "expected number"))
  (numV (op (numV-n l-val) (numV-n r-val))))

;; lookup : symbol? DefSub? -> FAE-Value?
(define (lookup name ds)
  (type-case DefSub ds
    [mtSub () (error 'interp "free identifier")]
    [aSub (name2 val rest)
          (if (equal? name name2)
              val
              (lookup name rest))]))


;; parse, compile, interp tests

(define initial-def-sub (mtSub))

(test (interp (compile (parse '{with {f {fun {x y} {+ x y}}}
                                     {f 2 3}}))
              initial-def-sub)
      (numV 5))

(test (interp (compile (parse '{with {add2 {fun {x} {+ x 2}}}
                                     {with {add3 {fun {y} {+ y 3}}}
                                           {add2 {add3 4}}}}))
              initial-def-sub)
      (numV 9))

(test (interp (compile (parse '{fun {x} {+ 1 1}}))
              initial-def-sub)
      (closureV 'x (num 2) (mtSub)))

(test (interp (compile (parse '{if0 {+ 2 -2}
                                      {+ 1 10}
                                      {+ 2 10}}))
              initial-def-sub)
      (numV 11))

(test (interp (compile (parse '{if0 {fun {x} {+ x 1}} 
                                      {+ 1 10}      
                                      {+ 2 10}}))    
              initial-def-sub)
      (numV 12)) 

(test (interp (compile (parse '{if0 {+ 1 -2}
                                    {fun {x} {+ x 1}}
                                    {fun {y} {+ y 2}}}))
              initial-def-sub)
      (closureV 'y (add (id 'y) (num 2)) (mtSub)))

(test (interp (compile (parse '{if0 {+ 1 -2}
                                    {{fun {x} {+ x 1}} 2}
                                    {{fun {y} {+ y 2}} 2}}))
              initial-def-sub)
      (numV 4))

(test (interp (compile (parse '{with {f {fun {x} {+ x 1}}}
                                     {with {g {fun {y} {f {+ y 1}}}}
                                           {g 3}}}))
              initial-def-sub)
      (numV 5))

(test/exn (interp (compile (parse `{with {f {fun {x} {+ x 1}}}
                                     {g 5}}))
                  initial-def-sub)
          "free identifier")

(test/exn (interp (compile (parse `{with {a 10}
                                     {with {b {+ a c}}
                                           {+ b 2}}}))
                  initial-def-sub)
          "free identifier")

(test/exn (interp (compile (parse '{1 2}))
                  initial-def-sub)
          "expected function")

(test/exn (interp (compile (parse `{+ {fun {x} x} {1 2}}))
                  initial-def-sub)
          "expected function")

(test/exn (interp (compile (parse `{+ 1 {fun {x} 10}}))
                  initial-def-sub)
          "expected number")

(test/exn (interp (compile (parse `{+ {fun {} x} {1 2}}))
                  initial-def-sub)
          "nullary function")

(test/exn (interp (compile (parse `{with {f {fun {x} {+ x 1}}} {f}}))
                  initial-def-sub)
          "nullary application")



;; provided tests 

;; 5 -> 5
(test (interp (compile (parse `5))
              initial-def-sub)
      (numV 5))
;; {+ 1 2} -> 3
(test (interp (compile (parse `{+ 1 2}))
              initial-def-sub)
      (numV 3))
;; {- 3 4} -> -1
(test (interp (compile (parse `{- 3 4}))
              initial-def-sub)
      (numV -1))
;; {+ {+ 1 2} {- 3 4}} -> 2
(test (interp (compile (parse `{+ {+ 1 2} {- 3 4}}))
              initial-def-sub)
      (numV 2))

#|
{with {x {+ 1 2}}
      {+ x x}}
|#
(test (interp (compile (parse `{with {x {+ 1 2}}
                                     {+ x x}}))
              initial-def-sub)
      (numV 6))
#|
x
|#
(test/exn (interp (compile (parse `x))
                  initial-def-sub)
          "free identifier")
#|
{+ {with {x {+ 1 2}}
         {+ x x}}
   {with {x {- 4 3}}
         {+ x x}}}
|#
(test (interp (compile (parse `{+ {with {x {+ 1 2}}
                                        {+ x x}}
                                  {with {x {- 4 3}}
                                        {+ x x}}}))
              initial-def-sub)
      (numV 8))
#|
{+ {with {x {+ 1 2}}
         {+ x x}}
   {with {y {- 4 3}}
         {+ y y}}}
|#
(test (interp (compile (parse `{+ {with {x {+ 1 2}}
                                        {+ x x}}
                                  {with {y {- 4 3}}
                                        {+ y y}}}))
              initial-def-sub)
      (numV 8))
#|
{with {x {+ 1 2}}
      {with {x {- 4 3}}
            {+ x x}}}
|#
(test (interp (compile (parse `{with {x {+ 1 2}}
                                     {with {x {- 4 3}}
                                           {+ x x}}}))
              initial-def-sub)
      (numV 2))
#|
{with {x {+ 1 2}}
      {with {y {- 4 3}}
            {+ x x}}}
|#
(test (interp (compile (parse `{with {x {+ 1 2}}
                                     {with {y {- 4 3}}
                                           {+ x x}}}))
              initial-def-sub)
      (numV 6))

(test (interp (compile (parse `{with {f {fun {x} {+ x 1}}}
                                     {f 3}}))
              initial-def-sub)
      (numV 4))

(test (interp (compile (parse `{{fun {x} {+ x 1}} 3}))
              initial-def-sub)
      (numV 4))

(test (interp (compile (parse `{fun {x} {+ x 1}}))
              initial-def-sub)
      (closureV 'x (add (id 'x) (num 1)) (mtSub)))



(test/exn (interp (compile (parse `{1 2}))
                  initial-def-sub)
          "expected function")

(test/exn (interp (compile (parse `{+ {fun {x} x} {1 2}}))
                  initial-def-sub)
          "expected function")

(test/exn (interp (compile (parse `{+ 1 {fun {x} 10}}))
                  initial-def-sub)
          "expected number")

(test (interp (compile (parse `{with {f {with {x 3}
                                              {fun {y} {+ x y}}}}
                                     {f 5}}))
              initial-def-sub)
      (numV 8))

(test/exn (interp (compile (parse `{with {z {fun {x} {+ x y}}}
                                         {with {y 10} {z y}}}))
                  initial-def-sub)
          "free identifier")
