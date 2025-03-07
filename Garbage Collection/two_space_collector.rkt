#lang plai/gc2/collector

; Jonathan Kong
; CS 321
; Winter 2025
; Homework 6

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

#|
heap:    | 'free | 'free | 'free | ...                          NEW!
flat:    ... | 'flat | <payload>   | ...
pair:    ... | 'cons | <first-ptr> | <rest-ptr> | ...
closure: ... | 'clos | <code-ptr> | <n-free-vars> | <fv0> | <fv1> | ... | ...  
|#

(define (init-allocator)
  (heap-set! 0 (+ 4 (/ (- (heap-size) 4) 2))) 
  (heap-set! 1 (+ 4 (/ (- (heap-size) 4) 2)))
  (heap-set! 2 4)
  (heap-set! 3 4)
  (for ([i (in-range 4 (heap-size))])
    (heap-set! i 'free)))


(define (malloc n root1 root2)
  (define current (heap-ref 0))
  (define start (heap-ref 1))
  (define end (+ start (/ (- (heap-size) 4) 2)))
  (cond
    [(>= (+ current n) end)
     (collect-garbage root1 root2)
     (define newcurrent (heap-ref 0))
     (define newstart (heap-ref 1))
     (define newend (+ newstart (/ (- (heap-size) 4) 2)))
     (cond
       [(>= (+ newcurrent n)  newend)
        (error 'malloc "out of memory")]
       [else
        (heap-set! 0 (+ newcurrent n))
        newcurrent])]
    [else
     (heap-set! 0 (+ current n))
     current]))


(define (collect-garbage root1 root2)
  (init-queue root1 root2)
  (process-queue)
  (change-queue-spaces))

(define (process-queue)
  (define (pop-queue addr)
    (case (heap-ref addr)
      [(free)
       (error 'process-queue "invalid address ~a" addr)]
      [(flat)
       (heap-set! 2 (+ addr 2))]
      [(cons)
       (process-pointer (+ addr 1))
       (process-pointer (+ addr 2))
       (heap-set! 2 (+ addr 3))]
      [(clos)
       (define n-vars (heap-ref (+ addr 2)))
       (for ([i (in-range n-vars)])
         (process-pointer (+ addr 3 i)))
       (heap-set! 2 (+ addr 3 n-vars))]
      [else
       (error 'process-queue "unexpected tag ~a" addr)]))

  (define (bfs-loop)
    (define L (heap-ref 2))
    (define R (heap-ref 3))
    (cond
      [(>= L R)
       (void)]
      [else
       (pop-queue L)
       (bfs-loop)]))
  (bfs-loop))

(define (process-pointer p)
  (if (equal? (heap-ref (heap-ref p)) 'forwarding)
      (heap-set! p (heap-ref (+ (heap-ref p) 1)))
      (let ([oldR (heap-ref 3)])
        (move-obj (heap-ref p))
        (heap-set! p oldR))))

(define (init-queue root1 root2)
  (move-or-follow root1)
  (move-or-follow root2)
  (move-or-follow (get-root-set)))

(define (move-or-follow root)
  (cond
    [(false? root) void]
    [(root? root)
     (define obj (read-root root))
     (if (equal? (heap-ref obj) 'forwarding)
         (set-root! root (heap-ref (+ 1 obj)))
         (let ([oldR (heap-ref 3)])
           (move-obj obj)
           (set-root! root oldR)))]
    [(list? root)
     (for ([r (in-list root)])
       (move-or-follow r))]
    [else (error 'move-or-follow "unexpected object, got ~a" root)]))

(define (move-obj root)
  (define R (heap-ref 3))
  (case (heap-ref root)
    [(flat)
     (heap-set! R 'flat)
     (heap-set! (+ R 1) (heap-ref (+ root 1)))
     (heap-set! root 'forwarding)
     (heap-set! (+ root 1) R)
     (heap-set! 3 (+ R 2))] 
    [(cons)
     (heap-set! R 'cons)
     (heap-set! (+ R 1) (heap-ref (+ root 1)))
     (heap-set! (+ R 2) (heap-ref (+ root 2)))
     (heap-set! root 'forwarding)
     (heap-set! (+ root 1) R)
     (heap-set! 3 (+ R 3))]  
    [(clos)
     (define n-vars (heap-ref (+ root 2)))
     (heap-set! R 'clos)
     (heap-set! (+ R 1) (heap-ref (+ root 1)))
     (heap-set! (+ R 2) (heap-ref (+ root 2)))
     (for ([i (in-range n-vars)])
       (heap-set! (+ R 3 i) (heap-ref (+ root 3 i))))
     (heap-set! root 'forwarding)
     (heap-set! (+ root 1) R)
     (heap-set! 3 (+ R 3 n-vars))]  
    [else
     (error 'move-obj "unexpected object ~a" root)]))

(define (change-queue-spaces)
  (define oldR (heap-ref 3))
  (define oldActiveStart (heap-ref 1))
  (define newActiveStart
    (if (= oldActiveStart 4)
        (+ 4 (/ (- (heap-size) 4) 2))
        4))
  (heap-set! 0 oldR)
  (heap-set! 1 newActiveStart)
  (heap-set! 2 oldActiveStart)
  (heap-set! 3 oldActiveStart))


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