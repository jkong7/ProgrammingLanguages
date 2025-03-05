#lang plaitypus

; Jonathan Kong
; CS 321
; Winter 2025
; Homework 7

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

(print-only-errors #t)

(define-type TLFAE
  [num (n : number)]
  [bool (b : boolean)]
  [add (l : TLFAE) (r : TLFAE)]
  [sub (l : TLFAE) (r : TLFAE)]
  [eql (l : TLFAE) (r : TLFAE)]
  [id (name : symbol)]
  [ifthenelse (tst : TLFAE) (thn : TLFAE) (els : TLFAE)]
  [fun (arg : symbol) (typ : Type) (body : TLFAE)]
  [app (rator : TLFAE) (rand : TLFAE)]
  [consl (fst : TLFAE) (rst : TLFAE)]
  [firstl (lst : TLFAE)]
  [restl (lst : TLFAE)]
  [nil (typ : Type)]
  [mtl? (lst : TLFAE)]
  [makevector (size : TLFAE) (initial : TLFAE)]
  [set (vec : TLFAE) (index : TLFAE) (val : TLFAE)]
  [lengthl (col : TLFAE)]
  [get (col : TLFAE) (index : TLFAE)])

(define-type Type
  [numberT]
  [booleanT]
  [arrowT (dom : Type) (codom : Type)]
  [listT (typ : Type)]
  [vectorT (typ : Type)])

(define-type TypeEnv
  [mtEnv]
  [aBind (name : symbol)
         (type : Type)
         (rest : TypeEnv)])

(define parse : (s-expression -> TLFAE)
  (lambda (s-exp)
    (cond
      [(s-exp-number? s-exp)
       (num (s-exp->number s-exp))]
      [(s-exp-symbol? s-exp)
       (case (s-exp->symbol s-exp)
         [(true)  (bool #t)]
         [(false) (bool #f)]
         [else (id (s-exp->symbol s-exp))])]
      [(s-exp-list? s-exp)
       (define as-list (s-exp->list s-exp))
       (cond [(empty? as-list)
              (error 'parse "the empty list is not a valid TLFAE")]
             [(s-exp-symbol? (first as-list))
              (case (s-exp->symbol (first as-list))
                [(+)
                 (check-pieces as-list "add" 3)
                 (add (parse (second as-list))
                      (parse (third as-list)))]
                [(-)
                 (check-pieces as-list "sub" 3)
                 (sub (parse (second as-list))
                      (parse (third as-list)))]
                [(=)
                 (check-pieces as-list "eql" 3)
                 (eql (parse (second as-list))
                      (parse (third as-list)))]
                [(if)
                 (check-pieces as-list "if" 4)
                 (ifthenelse (parse (second as-list))
                             (parse (third as-list))
                             (parse (fourth as-list)))]
                [(fun)
                 (check-pieces as-list "fun" 3)
                 (unless (s-exp-list? (second as-list))
                   (error 'parse "expected parameter list"))
                 (define param-list (s-exp->list (second as-list)))
                 (check-pieces param-list "parameter list" 3)
                 (unless (s-exp-symbol? (first param-list))
                   (error 'parse "expected symbol as parameter name"))
                 (unless (and (s-exp-symbol? (second param-list))
                              (equal? ': (s-exp->symbol (second param-list))))
                   (error 'parse "expected `:`"))
                 (fun (s-exp->symbol (first param-list))
                      (parse-type (third param-list))
                      (parse (third as-list)))]
                [(cons)
                 (check-pieces as-list "cons" 3)
                 (consl (parse (second as-list))
                        (parse (third as-list)))]
                [(first)
                 (check-pieces as-list "first" 2)
                 (firstl (parse (second as-list)))]
                [(rest)
                 (check-pieces as-list "rest" 2)
                 (restl (parse (second as-list)))]
                [(nil)
                 (check-pieces as-list "nil" 2)
                 (nil (parse-type (second as-list)))]
                [(empty?)
                 (check-pieces as-list "empty?" 2)
                 (mtl? (parse (second as-list)))]
                [(make-vector)
                 (check-pieces as-list "make-vector" 3)
                 (makevector (parse (second as-list))
                             (parse (third as-list)))]
                [(set)
                 (check-pieces as-list "set" 4)
                 (set (parse (second as-list))
                      (parse (third as-list))
                      (parse (fourth as-list)))]
                [(length)
                 (check-pieces as-list "length" 2)
                 (lengthl (parse (second as-list)))]
                [(get)
                 (check-pieces as-list "get" 3)
                 (get (parse (second as-list))
                      (parse (third as-list)))]
                [else (parse-app as-list)])]
             [else (parse-app as-list)])]
      [else
       (error 'parse "expected TLFAE")])))

(define parse-app : ((listof s-expression) -> TLFAE)
  (lambda (s-exps)
    (check-pieces s-exps "app" 2)
    (app (parse (first  s-exps))
         (parse (second s-exps)))))

(define parse-type : (s-expression -> Type)
  (lambda (s-exp)
    (cond [(and (s-exp-symbol? s-exp)
                (equal? 'number (s-exp->symbol s-exp)))
           (numberT)]
          [(and (s-exp-symbol? s-exp)
                (equal? 'boolean (s-exp->symbol s-exp)))
           (booleanT)]
          [(s-exp-list? s-exp)
           (define as-list (s-exp->list s-exp))
           (case (length as-list)
             [(2)
              (unless (s-exp-symbol? (first as-list))
                (error 'parse "expected `listof` or `vectorof`"))
              (case (s-exp->symbol (first as-list))
                [(listof)
                 (listT (parse-type (second as-list)))]
                [(vectorof)
                 (vectorT (parse-type (second as-list)))]
                [else
                 (error 'parse "expected `listof` or `vectorof`")])]
             [(3)
              (unless (and (s-exp-symbol? (second as-list))
                           (equal? '-> (s-exp->symbol (second as-list))))
                (error 'parse "expected `->`"))
              (arrowT (parse-type (first as-list))
                      (parse-type (third as-list)))]
             [else (error 'parse-type "expected type")])]
          [else (error 'parse-type "expected type")])))

(define check-pieces : ((listof s-expression) string number -> void)
  (lambda (s-exps expected n-pieces)
    (unless (= n-pieces (length s-exps))
      (error 'parse
             (string-append (string-append "expected " expected)
                            (string-append " got " (to-string s-exps)))))))


;; ----------------------------------------------------------------------

(define typecheck : (TLFAE TypeEnv -> Type)
  (lambda (a-tlfae gamma)
    (type-case TLFAE a-tlfae
      [num (n)
           (numberT)] 
      [add (l r)
           (unless (numberT? (typecheck l gamma))
             (error 'typecheck "expected number"))
           (unless (numberT? (typecheck r gamma))
             (error 'typecheck "expected number"))
           (numberT)]
      [sub (l r)
           (unless (numberT? (typecheck l gamma))
             (error 'typecheck "expected number"))
           (unless (numberT? (typecheck r gamma))
             (error 'typecheck "expected number"))
           (numberT)]
      [id  (name)
           (define tau (lookup-type name gamma))
           tau]
      [fun (param-name param-type body)
           (define tau1 param-type)
           (define tau2 (typecheck body (aBind param-name
                                               param-type
                                               gamma)))
           (arrowT tau1 tau2)]
      [app (fun-expr arg-expr)
           (define fun-type (typecheck fun-expr gamma))
           (type-case Type fun-type
             [arrowT (tau2 tau3)
                     (define arg-type (typecheck arg-expr gamma))
                     (unless (equal? tau2 arg-type)
                       (error 'typecheck "type mismatch"))
                     tau3]
             [else (error 'typecheck "expected function")])]
      [bool (bool-value)
            (booleanT)]
      [eql (l r)
           (unless (numberT? (typecheck l gamma))
             (error 'typecheck "expected number"))
           (unless (numberT? (typecheck r gamma))
             (error 'typecheck "expected number"))
           (booleanT)]
      [ifthenelse (tst thn els)
                  (define tst-type (typecheck tst gamma))
                  (unless (booleanT? tst-type)
                           (error 'typecheck "expected boolean"))
                  (define then-type (typecheck thn gamma))
                  (define els-type (typecheck els gamma))
                  (unless (equal? then-type els-type)
                           (error 'typecheck "type mismatch"))
                  then-type]
      [consl (fst rst)
             (define fst-type (typecheck fst gamma))
             (define rst-type (typecheck rst gamma))
             (unless (listT? rst-type)
                      (error 'typecheck "expected list"))
             (unless (equal? fst-type (listT-typ rst-type))
                      (error 'typecheck "type mismatch"))
             rst-type]
      [firstl (lst)
              (define lst-type (typecheck lst gamma))
              (unless (listT? lst-type)
                (error 'typecheck "expected list"))
              (listT-typ lst-type)]
      [restl (lst)
             (define lst-type (typecheck lst gamma))
             (unless (listT? lst-type)
               (error 'typecheck "expected list"))
             lst-type]
      [nil (typ)
           (listT typ)]
      [mtl? (lst)
            (define lst-type (typecheck lst gamma))
            (unless (listT? lst-type)
              (error 'typecheck "expected list"))
            (booleanT)]
      [makevector (size initial)
                  (define size-type (typecheck size gamma))
                  (unless (numberT? size-type)
                    (error 'typecheck "expected number"))
                  (define val-type (typecheck initial gamma))
                  (vectorT val-type)]
      [set (vec index val)
           (define index-type (typecheck index gamma))
           (unless (numberT? index-type)
             (error 'typecheck "expected number"))
           (define vec-type (typecheck vec gamma))
           (unless (vectorT? vec-type)
             (error 'typecheck "expected vector"))
           (define val-type (typecheck val gamma))
           (unless (equal? (vectorT-typ vec-type) val-type)
             (error 'typecheck "type mismatch"))
           (vectorT-typ vec-type)]
      [get (col index)
           (define col-type (typecheck col gamma))
           (unless (or (listT? col-type)
                       (vectorT? col-type))
             (error 'typecheck "expected list or vector"))
           (define index-type (typecheck index gamma))
           (unless (numberT? index-type)
             (error 'typecheck "expected number"))
           (if (listT? col-type) 
               (listT-typ col-type)
               (vectorT-typ col-type))] 
      [lengthl (col)
               (define col-type (typecheck col gamma))
               (unless (or (listT? col-type)
                           (vectorT? col-type))
                 (error 'typecheck "expected list or vector"))
               (numberT)])))

(define lookup-type : (symbol TypeEnv -> Type)
  (lambda (name gamma)
    (type-case TypeEnv gamma
      [mtEnv () (error 'typecheck "free identifier")]
      [aBind (n t rest)
             (if (equal? name n)
                 t
                 (lookup-type name rest))])))

;; typecheck-expr : TLFAE -> Type
(define (typecheck-expr tlfae)
  (typecheck tlfae (mtEnv)))


;; written test cases

(test (typecheck-expr (parse `true))
      (booleanT))

(test (typecheck-expr (parse `false))
      (booleanT))

(test (typecheck-expr (parse `{= 2 2}))
      (booleanT))

(test (typecheck-expr (parse `{= {- 5 3} {+ 1 1}}))
      (booleanT))

(test/exn (typecheck-expr (parse `{= true false}))
          "expected number")

(test (typecheck-expr (parse `{if true 3 4}))
      (numberT))

(test/exn (typecheck-expr (parse `{if 3 4 5}))
          "expected boolean")

(test/exn (typecheck-expr (parse `{if true 3 false}))
          "type mismatch")

(test (typecheck-expr (parse `{cons 1 {cons 2 {nil number}}}))
      (parse-type `{listof number}))

(test (typecheck-expr (parse `{first {cons 5 {nil number}}}))
      (numberT))

(test (typecheck-expr (parse `{first {nil number}}))
          (numberT))

(test (typecheck-expr (parse '{rest {cons 5 {nil number}}}))
      (listT (numberT)))

(test (typecheck-expr (parse `{rest {cons 5 {cons 10 {nil number}}}}))
      (parse-type `{listof number}))

(test (typecheck-expr (parse `{rest {nil number}}))
          (listT (numberT)))

(test (typecheck-expr (parse `{empty? {nil number}}))
      (booleanT))

(test (typecheck-expr (parse `{empty? {cons 5 {nil number}}}))
      (booleanT))

(test (typecheck-expr (parse `{make-vector 3 true}))
      (vectorT (booleanT)))

(test/exn (typecheck-expr (parse `{make-vector true false}))
          "expected number")

(test (typecheck-expr (parse `{set {make-vector 5 0} 2 10}))
      (numberT))

(test/exn (typecheck-expr (parse `{set {make-vector 5 true} 2 10}))
          "type mismatch")

(test/exn (typecheck-expr (parse `{set {make-vector 5 0} true 10}))
          "expected number")

(test (typecheck-expr (parse `{length {make-vector 5 0}}))
      (numberT))

(test (typecheck-expr (parse `{length {cons 5 {cons 10 {nil number}}}}))
      (numberT))

(test (typecheck-expr (parse `{get {make-vector 5 0} 2}))
      (numberT))

(test (typecheck-expr (parse `{get {cons 5 {cons 10 {nil number}}} 0}))
      (numberT))

(test/exn (typecheck-expr (parse `{get 3 1}))
          "expected list or vector")

(test (typecheck-expr (parse `{{fun {x : number} {+ x 1}} 5}))
      (numberT))

(test (typecheck-expr (parse `{{fun {x : (number -> number)} {fun {y : number} {x y}}} {fun {z : number} {+ z 1}}}))
      (parse-type `(number -> number)))

(test/exn (typecheck-expr (parse `{{fun {x : number} {+ x 1}} true}))
          "type mismatch")

(test/exn (typecheck-expr (parse `x))
          "free identifier")

(test/exn (typecheck-expr (parse `{5 7}))
          "expected function")

(test/exn (typecheck-expr (parse `{if 3 4 5}))
          "expected boolean")

(test/exn (typecheck-expr (parse `{+ true 3}))
          "expected number")

(test/exn (typecheck-expr (parse `{= true 3}))
          "expected number")

(test/exn (typecheck-expr (parse `{first 3}))
          "expected list")

(test/exn (typecheck-expr (parse `{rest 3}))
          "expected list")

(test/exn (typecheck-expr (parse `{cons 5 10}))  
          "expected list")

(test/exn (typecheck-expr (parse `{set 3 1 5}))
          "expected vector")

(test/exn (typecheck-expr (parse `{get 3 1}))
          "expected list or vector")

(test/exn (typecheck-expr (parse `{length 3}))
          "expected list or vector")

(test/exn (typecheck-expr (parse `{if true 3 false}))
          "type mismatch")

(test/exn (typecheck-expr (parse `{set {make-vector 5 0} 2 true}))  
          "type mismatch")

(test/exn (typecheck-expr (parse `{cons true {nil number}})) 
          "type mismatch")

(test/exn (typecheck-expr (parse `{{fun {x : number} {+ x 5}} true}))  
          "type mismatch")

;; provided test cases 

(test (typecheck (parse `5)
                 (mtEnv))
      (numberT))

(test (typecheck (parse `{+ 2 3})
                 (mtEnv))
      (parse-type `number))
(test (typecheck (parse `{- 2 3})
                 (mtEnv))
      (parse-type `number))
(test (typecheck (parse `{+ 1 {- 2 3}})
                 (mtEnv))
      (parse-type `number))

(test (typecheck (parse `{fun {x : number} {+ x 5}})
                 (mtEnv))
      (parse-type `(number -> number))) 
(test (typecheck (parse `{{fun {x : number} {+ x 5}}
                          5})
                 (mtEnv))
      (parse-type `number)) 

(test/exn (typecheck (parse `{+ 1 {fun {x : number} {+ x 5}}})
                 (mtEnv))
          "expected number") 
(test/exn (typecheck (parse `{5 7})
                     (mtEnv))
          "expected function")


(test (typecheck (parse `{fun {f : (number -> number)}
                              {fun {x : number} {f x}}})
                 (mtEnv))
      (parse-type `((number -> number) -> (number -> number))))
(test/exn (typecheck (parse `{{fun {f : (number -> number)}
                                   {fun {x : number} {f x}}}
                              3})
                     (mtEnv))
          "type mismatch") 
(test (typecheck (parse `{{fun {f : (number -> number)}
                               {fun {x : number} {f x}}}
                          {fun {x : number} {+ x 5}}})
                 (mtEnv))
      (parse-type `(number -> number))) ; QUIZ
(test/exn (typecheck (parse `{{fun {f : (number -> number)}
                                   {fun {x : number} {f x}}}
                              {fun {y : (number -> number)}
                                   {y 8}}})
                     (mtEnv))
          "type mismatch")
(test (typecheck (parse `{{{fun {f : (number -> number)}
                                {fun {x : number} {f x}}}
                           {fun {x : number} {+ x 5}}}
                          5})
                 (mtEnv))
      (parse-type `number))