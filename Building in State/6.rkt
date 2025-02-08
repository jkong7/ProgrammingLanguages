#lang plai
(print-only-errors)

(define-type SFAE
  [num (n number?)]
  [add (lhs SFAE?)
       (rhs SFAE?)]
  [sub (lhs SFAE?)
       (rhs SFAE?)]
  [id (name symbol?)]
  [fun (param-name symbol?)
       (body SFAE?)]
  [app (fun-expr SFAE?)
       (arg-expr SFAE?)]
  [new-struct (fields (listof (cons symbol? SFAE?)))]
  [struct-get (targetstruct SFAE?)
             (id symbol?)]
  [struct-set (targetstruct SFAE?)
             (id symbol?)
             (newval SFAE?)]
  [seqn (expr1 SFAE?)
        (expr2 SFAE?)])

(define-type Store
  [mtSto]
  [aSto (address integer?)
        (value (listof (cons symbol? SFAE-Value?)))
        (rest Store?)])

(define-type Value*Store
  [v*s (v SFAE-Value?)
       (s Store?)])

(define-type SFAE-Value
  [numV (n number?)]
  [closureV (param-name symbol?)
            (body SFAE?)
            (ds DefSub?)]
  [structV (address integer?)])

(define-type DefSub
  [mtSub]
  [aSub  (name symbol?)
         (value SFAE-Value?)
         (rest DefSub?)])

;; ----------------------------------------------------------------------

;; parse : s-expression -> SFAE?
(define (parse s-exp)
  (cond [(number? s-exp)
         (num s-exp)]
        [(symbol? s-exp)
         (id s-exp)]
        [(list? s-exp)
         (when (empty? s-exp)
           (error 'parse "the empty list is not a valid BFAE"))
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
           [(struct)
            (check-pieces s-exp "struct" (length s-exp)) 
            (new-struct
             (map (lambda (field-pair)
                    (unless (and (list? field-pair) (= (length field-pair) 2))
                      (error 'parse "expected {id value} pair in struct, got ~a" field-pair))
                    (unless (symbol? (first field-pair))
                      (error 'parse "expected field name to be a symbol, got ~a" (first field-pair)))
                    (cons (first field-pair) (parse (second field-pair))))
                  (rest s-exp)))]  
           [(get)
            (check-pieces s-exp "get" 3)
            (unless (symbol? (third s-exp))
              (error 'parse "expected symbol, got ~a" (third s-exp)))
            (struct-get (parse (second s-exp))
                        (third s-exp))]
           [(set)
            (check-pieces s-exp "set" 4)
            (unless (symbol? (third s-exp))
              (error 'parse "expected symbol, got ~a" (third s-exp)))
            (struct-set (parse (second s-exp))
                        (third s-exp)
                        (parse (first (rest (rest (rest s-exp))))))]
           [(seqn)
            (check-pieces s-exp "seqn" 3)
            (seqn (parse (second s-exp))
                  (parse (third s-exp)))]
           [else
            (check-pieces s-exp "app" 2)
            (app (parse (first s-exp))
                 (parse (second s-exp)))])]
        [else
         (error 'parse "expected SFAE got ~a" s-exp)]))

(define (check-pieces s-exp expected n-pieces)
  (unless (and (list? s-exp)
               (= n-pieces (length s-exp)))
    (error 'parse "expected ~a got ~a" expected s-exp)))

;; ----------------------------------------------------------------------

;; interp-test : s-expression -> SFAE-Value?
(define (interp-test s-exp)
  (v*s-v (interp (parse s-exp) (mtSub) (mtSto))))

;; interp : SFAE? DefSub? Store? -> Value*Store?
(define (interp a-bfae ds st) ; NEW
  (type-case SFAE a-bfae
    [num (n) (v*s (numV n)
                  st)]
    [add (l r) (numop + l r ds st)]
    [sub (l r) (numop - l r ds st)]
    [id (name)
        (v*s (lookup name ds)
             st)]
    [fun (param-name body)
         (v*s (closureV param-name body ds)
              st)]
    [app (fun-expr arg-expr)
         (interp-two fun-expr arg-expr ds st
                     (lambda (fun-val arg-val st3)
                       (unless (closureV? fun-val)
                         (error 'interp "expected function"))
                       (interp (closureV-body fun-val)
                               (aSub (closureV-param-name fun-val)
                                     arg-val
                                     (closureV-ds fun-val))
                               st3)))]
    [new-struct (fields)
                (define evaluated-fields-and-store
                  (evaluate-and-accumulate-fields fields ds st (list)))
                (define evaluated-fields (car evaluated-fields-and-store))
                (define stX (cdr evaluated-fields-and-store))
                (define address (malloc stX))
                (v*s (structV address)
                     (aSto address
                           evaluated-fields
                           stX))]
    [struct-get (targetstruct id)
                (define evaluated-targetstruct (interp targetstruct ds st))
                (type-case Value*Store evaluated-targetstruct
                  [v*s (sfae st2)
                       (type-case SFAE-Value sfae
                         [numV (n) (error 'interp "expected struct")]
                         [closureV (_1 _2 _3) (error 'interp "expected struct")]
                         [structV (address)
                                  (define targetstruct-fields (lookup-store address st2))
                                  (lookup-struct id targetstruct-fields st2)])])]
  

   
    [struct-set (targetstruct id newval)]
    [seqn (expr1 expr2)]
    
    [setbox (box-expr new-expr)
            (interp-two box-expr new-expr ds st
                        (lambda (box-val new-val st3)
                          (type-case BFAE-Value box-val
                            [numV (n) (error 'interp "expected box")]
                            [closureV (_1 _2 _3) (error 'interp "expected box")]
                            [boxV (addr)
                                  (v*s (boxV addr)
                                       (aSto addr
                                             new-val
                                             st3))])))]
    [seqn (expr1 expr2)
          (interp-two expr1 expr2 ds st
                      (lambda (expr1-val expr2-val st3)
                        (v*s expr2-val st3)))]))

;; lookup-struct : symbol? (listof (pair symbol? SFAE-Value?)) -> SFAE-Value?
(define (lookup-struct id fields st)
  (cond
    [(empty? fields) (error 'interp "unknown field")]
    [(equal? id (car (first fields)))
     (v*s (cdr (first fields))
          st)]
    [else (lookup-struct id (rest fields) st)]))



;; evaluate-and-accumulate-fields : (listof (pair symbol? SFAE?)) DefSub? Store? (listof (pair symbol? SFAE-Value?)) 
;;                                  -> (pair (listof (pair symbol? SFAE-Value?)) Store?)
(define (evaluate-and-accumulate-fields fields ds st acc)
  (cond 
    [(empty? fields) (cons acc st)]
    [else
     (define evaluated-val (interp (second (first fields)) ds st))
     (type-case Value*Store evaluated-val
       [v*s (val st2)
            (define new-acc (append acc (list (cons (first (first fields)) val))))
            (evaluate-and-accumulate-fields (rest fields) ds st2 new-acc)])]))


;; interp-two : SFAE? SFAE? DefSub? Store?
;;              (SFAE-Value? SFAE-Value? Store? -> Value*Store?)
;;              -> Value*Store?
(define (interp-two e1 e2 ds st finish)
  (type-case Value*Store (interp e1 ds st)
            [v*s (v1 st2)
                 (type-case Value*Store (interp e2 ds st2)
                   [v*s (v2 st3)
                        (finish v1 v2 st3)])]))

;; lookup-store : integer? Store -> (listof (pair symbol? SFAE-Value?))
(define (lookup-store a s)
  (type-case Store s
    [mtSto () (error 'interp "internal error: dangling pointer")]
    [aSto (a2 v r)
          (if (= a a2) v (lookup-store a r))]))

;; malloc : Store? -> integer?
(define (malloc st)
  (+ 1 (max-address st)))
(define (max-address st)
  (type-case Store st
    [mtSto () 0]
    [aSto (a v r) (max a (max-address r))]))

;; numop : (number? number? -> number?)
;;         SFAE? SFAE? DefSub? Store? -> Value*Store?
(define (numop op l r ds st)
  (interp-two
   l r ds st
   (lambda (l-val r-val st3)
     (unless (numV? l-val)
       (error 'interp "expected number, got ~a" l-val))
     (unless (numV? r-val)
       (error 'interp "expected number, got ~a" r-val))
     (v*s (numV (op (numV-n l-val) (numV-n r-val)))
          st3))))

;; lookup : symbol? DefSub? -> SFAE-Value?
(define (lookup name ds)
  (type-case DefSub ds
    [mtSub () (error 'interp "free identifier")]
    [aSub (name2 val rest)
          (if (equal? name name2)
              val
              (lookup name rest))]))

(test (interp-test `{newbox 10})
      (boxV 1))

(test (interp-test `{openbox {newbox {newbox 5}}})
      (boxV 1))

(test (interp-test `{openbox {newbox 10}})
      (numV 10))

(test (interp-test `{openbox {openbox {newbox {newbox 5}}}})
      (numV 5))

(test (interp-test `{seqn {openbox {newbox 10}}
                          {+ 2 1}})
      (numV 3))

(test (interp-test `{with {b {newbox 0}}
                          {seqn {setbox b 10}
                                {openbox b}}})
      (numV 10))

(test (interp-test `{with {b {newbox 10}}
                          {seqn
                           {setbox b 12}
                           {seqn
                            {setbox b 14}
                            {openbox b}}}})
      (numV 14))

(test (interp-test `{with {b1 {newbox 5}}
                          {with {b2 {newbox 7}}
                                {+ {openbox b1}
                                   {openbox b2}}}})
      (numV 12))

(test (interp-test `{with {b1 {newbox 8}}
                          {with {b2 {newbox 4}}
                                {seqn {setbox b1 6}
                                      {+ {openbox b1}
                                         {openbox b2}}}}})
      (numV 10))

(test (interp-test `{with {b1 {newbox 8}}
                          {with {b2 {newbox 4}}
                                {seqn {setbox b1 6}
                                      {seqn {setbox b2 8}
                                            {+ {openbox b1}
                                               {openbox b2}}}}}})
      (numV 14))


(test (interp-test `{with {b {newbox 10}}
                          {openbox
                           {setbox {seqn {setbox b 12} b}
                                   {openbox b}}}})
      (numV 12))

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