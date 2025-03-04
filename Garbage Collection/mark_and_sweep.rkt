#lang plai/gc2/collector

(print-only-errors)

#|
heap:    | 'free | 'free | 'free | ...                          NEW!
flat:    ... | 'flat | <payload>   | ...
pair:    ... | 'cons | <first-ptr> | <rest-ptr> | ...
closure: ... | 'clos | <code-ptr> | <n-free-vars> | <fv0> | <fv1> | ... | ...  
|#

(define (init-allocator)
  (for ([i (in-range (heap-size))])
    (heap-set! i 'free)))


;; malloc : size -> address
(define (malloc n root1 root2)
  (define a (find-free-spaces n 0))
  (cond [(integer? a)
         a]
        [else
         (collect-garbage root1 root2)
         (define a (find-free-spaces n 0))
         (cond [(integer? a)
                a]
               [else
                (error 'malloc "out of memory")])]))



(define (collect-garbage root1 root2)
  (validate-heap 0)
  (mark-white! 0)
  (traverse/roots (get-root-set))
  (traverse/roots root1)
  (traverse/roots root2)
  (free-white! 0)
  (validate-heap 0))

(define (validate-pointer a)
  (define ptr (heap-ref a))
  (unless (and (integer? ptr)
               (ptr . >= . 0)
               (ptr . < . (heap-size))
               (member (heap-ref ptr) '(flat cons clos)))
    (error 'validate-pointer "invalid pointer @ ~a" a)))

(define (validate-heap start)
  (unless (>= start (heap-size))
    (case (heap-ref start)
      [(flat) (validate-heap (+ start 2))]
      [(cons) (validate-pointer (+ start 1))
              (validate-pointer (+ start 2))
              (validate-heap (+ start 3))]
      [(clos) (for ([i (in-range (heap-ref (+ start 2)))])
                (validate-pointer (+ start 3 i)))
              (validate-heap (+ start 3 (heap-ref (+ start 2))))]
      [(free) (validate-heap (+ start 1))]
      [else (error 'validate-heap "unexpected tag @ ~a" start)])))


(define (mark-white! start)
  (unless (>= start (heap-size))
    (case (heap-ref start)
      [(flat) (heap-set! start 'white-flat)
              (mark-white! (+ start 2))]
      [(cons) (heap-set! start 'white-cons)
              (mark-white! (+ start 3))]
      [(clos) (heap-set! start 'white-clos)
              (mark-white! (+ start 3 (heap-ref (+ start 2))))]
      [(free) (mark-white! (+ start 1))]
      [else (error 'mark-white! "unexpected tag @ ~a" start)])))

(define (free-white! start)
  (unless (>= start (heap-size))
    (case (heap-ref start)
      [(flat) (free-white! (+ start 2))]
      [(cons) (free-white! (+ start 3))]
      [(clos) (free-white! (+ start 3 (heap-ref (+ start 2))))]
      [(free) (free-white! (+ start 1))]
      [(white-flat)
       (heap-set! start 'free)
       (heap-set! (+ start 1) 'free)
       (free-white! (+ start 2))]
      [(white-cons)
       (heap-set! start 'free)
       (heap-set! (+ start 1) 'free)
       (heap-set! (+ start 2) 'free)
       (free-white! (+ start 3))]
      [(white-clos)
       (define n-free-vars (heap-ref (+ start 2)))
       (for ([i (in-range (+ 3 n-free-vars))])
         (heap-set! (+ start i) 'free))
       (free-white! (+ start 3 n-free-vars))]
      [else (error 'free-white! "unexpected tag @ ~a" start)])))

(define (traverse/roots roots)
  (cond [(false? roots)
         (void)]
        [(list? roots)
         (for ([r (in-list roots)])
           (traverse/loc (read-root r)))]
        [(root? roots)
         (traverse/loc (read-root roots))]
        [else
         (error 'traverse/roots "unexpected root: ~a" roots)]))

(define (traverse/loc ptr)
  (case (heap-ref ptr)
    [(flat) (void)]
    [(cons) (void)]
    [(clos) (void)]
    [(white-flat) (heap-set! ptr 'gray-flat)
                  (heap-set! ptr 'flat)]
    [(white-cons) (heap-set! ptr 'gray-cons)
                  (traverse/loc (heap-ref (+ ptr 1)))
                  (traverse/loc (heap-ref (+ ptr 2)))
                  (heap-set! ptr 'cons)]
    [(white-clos) (heap-set! ptr 'gray-clos)
                  (for ([i (in-range (heap-ref (+ ptr 2)))])
                    (traverse/loc (heap-ref (+ ptr 3 i))))
                  (heap-set! ptr 'clos)]
    [(gray-flat) (void)]
    [(gray-cons) (void)]
    [(gray-clos) (void)]
    [(free) (error 'traverse/loc "dangling pointer!")]
    [else (error 'traverse/loc "unexpected tag @ ~a" ptr)]))

;; find-free-space : int? int? -> (or/c location false?)
(define (find-free-spaces n start)
  (if (>= start (heap-size))
      #f
      (case (heap-ref start)
        [(free) (if (n-free-spaces? n start)
                    start
                    (find-free-spaces n (+ start 1)))]
        [(flat) (find-free-spaces n (+ start 2))]
        [(cons) (find-free-spaces n (+ start 3))]
        [(clos) (find-free-spaces
                 n
                 (+ start 3 (heap-ref (+ start 2))))]
        [else (error 'find-free-spaces "unexpected tag @ ~a" start)])))

;; n-free-spaces? : int? int? -> boolean?
(define (n-free-spaces? n start)
  (cond [(>= start (heap-size))
         #f]
        [(= n 0)
         #t]
        [(equal? (heap-ref start) 'free)
         (n-free-spaces? (- n 1) (+ start 1))]
        [else
         #f]))

;; ----------------------------------------------------------------------

#|
flat:    ... | 'flat | <payload> | ...
|#
;; gc:alloc-flat : flat-value -> location
(define (gc:alloc-flat value)
  (define address (malloc 2 #f #f))
  (heap-set! address 'flat)
  (heap-set! (+ address 1) value)
  address)
;; gc:flat? : location -> boolean
(define (gc:flat? address)
  (equal? (heap-ref address) 'flat))
;; gc:deref : location -> flat-value
(define (gc:deref address)
  (unless (gc:flat? address)
    (error 'gc:deref "expected flat @ ~a" address))
  (heap-ref (+ address 1)))


#|
pair:    ... | 'cons | <first-ptr> | <rest-ptr> | ...
|#
;; gc:cons : root root -> location
(define (gc:cons root1 root2)
  (define address (malloc 3 root1 root2))
  (heap-set! address 'cons)
  (heap-set! (+ address 1) (read-root root1))
  (heap-set! (+ address 2) (read-root root2))
  address)
;; gc:cons? : location -> boolean
(define (gc:cons? address)
  (equal? (heap-ref address) 'cons))
;; gc:first : location -> location
(define (gc:first address)
  (unless (gc:cons? address)
    (error 'gc:first "expected cons @ ~a" address))
  (heap-ref (+ address 1)))
;; gc:rest : location -> location
(define (gc:rest address)
  (unless (gc:cons? address)
    (error 'gc:rest "expected cons @ ~a" address))
  (heap-ref (+ address 2)))
;; gc:set-first! : location location -> void
(define (gc:set-first! address new-value-address)
  (unless (gc:cons? address)
    (error 'gc:set-first! "expected cons @ ~a" address))
  (heap-set! (+ address 1) new-value-address))
;; gc:set-rest! : location location -> void
(define (gc:set-rest! address new-value-address)
  (unless (gc:cons? address)
    (error 'gc:set-rest! "expected cons @ ~a" address))
  (heap-set! (+ address 2) new-value-address))


#|
closure: ... | 'clos | <code-ptr> | <n-free-vars> | <fv0> | <fv1> | ... | ...
|#
;; gc:closure : opaque-value (listof root) ->  location
(define (gc:closure code-ptr free-vars)
  (define address (malloc (+ 3 (length free-vars))
                          free-vars #f))
  (heap-set! address 'clos)
  (heap-set! (+ address 1) code-ptr)
  (heap-set! (+ address 2) (length free-vars))
  (for ([i  (in-range (length free-vars))]
        [fv (in-list free-vars)])
    (heap-set! (+ address 3 i) (read-root fv)))
  address)
;; gc:closure? :  location -> boolean
(define (gc:closure? address)
  (equal? (heap-ref address) 'clos))
;; gc:closure-code-ptr : location -> opaque-value
(define (gc:closure-code-ptr address)
  (unless (gc:closure? address)
    (error 'gc:closure-code-ptr "expected closure @ ~a" address))
  (heap-ref (+ address 1)))
;; gc:closure-env-ref : location integer -> location
(define (gc:closure-env-ref address i)
  (unless (gc:closure? address)
    (error 'gc:closure-env-ref "expected closure @ ~a" address))
  (heap-ref (+ address 3 i)))