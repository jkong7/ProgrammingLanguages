#lang plai

#|
<WAE> ::= <num>
       |  {+ <WAE> <WAE>}
       |  {- <WAE> <WAE>}
       |  {with {<id> <WAE>} <WAE>}
       |  <id>
|#

(print-only-errors)

(define-type WAE
  [num (n number?)]
  [add (lhs WAE?)
       (rhs WAE?)]
  [sub (lhs WAE?)
       (rhs WAE?)]
  [with (name symbol?)
        (named-expr WAE?)
        (body WAE?)]
  [id (name symbol?)])


;; interp : WAE? -> number?
(define (interp an-wae)
  (type-case WAE an-wae
    [num (n) n]
    [add (l r) (+ (interp l) (interp r))]
    [sub (l r) (- (interp l) (interp r))]
    [with (name named-expr body)
          (interp (subst body name (interp named-expr)))]
    [id (name) (error 'interp "free identifier: ~a" name)]))

;; subst : WAE? symbol? number? -> WAE?
(define (subst expr name value)
  (type-case WAE expr
    [num (n) expr]
    [add (l r) (add (subst l name value)
                    (subst r name value))]
    [sub (l r) (sub (subst l name value)
                    (subst r name value))]
    [with (name2 named-expr body)
          (with name2 (subst named-expr name value)
                (if (equal? name name2)
                    body
                    (subst body name value)))]
    [id (name2) (if (equal? name name2) (num value)
                    expr)]))


;; 5
(test (interp (num 5))
      5)


;; {+ 1 2}
(test (interp (add (num 1) (num 2)))
      3)


;; {- 3 2}
(test (interp (sub (num 3) (num 2)))
      1)


;; {+ 1 {- 3 2}}
(test (interp (add (num 1) (sub (num 3) (num 2))))
      2)


#|
{with {x {+ 1 2}}
      {+ x x}}
|#
(test (interp (with 'x (add (num 1) (num 2))
                    (add (id 'x) (id 'x))))
      6)


#|
x
|#
(test/exn (interp (id 'x))
          "free identifier")


#|
{+ {with {x {+ 1 2}}
         {+ x x}}
   {with {x {- 4 3}}
         {+ x x}}}}
|#
(test (interp (add (with 'x (add (num 1) (num 2))
                       (add (id 'x) (id 'x)))
                 (with 'x (sub (num 4) (num 3))
                       (add (id 'x) (id 'x)))))
      8)


#|
{+ {with {x {+ 1 2}}
         {+ x x}}
   {with {y {- 4 3}}
         {+ y y}}}}
|#

(test (interp (add (with 'x (add (num 1) (num 2)) (add (id 'x) (id 'x)))
                   (with 'y (sub (num 4) (num 3)) (add (id 'y) (id 'y)))))
      8)

#|
{with {x {+ 1 2}}
      {with {x {- 4 3}}
            {+ x x}}}
|#
(test (interp (with 'x (add (num 1) (num 2))
                    (with 'x (sub (num 4) (num 3))
                          (add (id 'x) (id 'x)))))
      2)

#|
{with {x {+ 1 2}}
      {with {y {- 4 3}}
            {+ x x}}}
|#
(test (interp (with 'x (add (num 1) (num 2))
                    (with 'y (sub (num 4) (num 3))
                          (add (id 'x) (id 'x)))))
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
substitute 10 for x in {- x 1}
|#
(test (subst (sub (id 'x) (num 1))
             'x 10)
      (sub (num 10) (num 1)))

#|
substitute 10 for x in {with {y 17} x}
|#
(test (subst (with 'y (num 17) (id 'x))
             'x 10)
      (with 'y (num 17) (num 10)))

#|
substitute 10 for x in {with {y x} y}
|#
(test (subst (with 'y (id 'x) (id 'y))
             'x 10)
      (with 'y (num 10) (id 'y)))

#|
substitute 10 for x in {with {x y} x}
|#


(test (subst (with 'x (id 'y) (id 'x))
             'x 10)
      (with 'x (id 'y) (id 'x)))

#|
substitute 10 for x in 
  {with {x {+ x 1}}
    x}}
|#
(test (subst (with 'x (add (id 'x) (num 1)) (id 'x))
             'x 10)
      (with 'x (add (num 10) (num 1)) (id 'x)))
