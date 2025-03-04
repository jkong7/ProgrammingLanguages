#lang plai/gc2/collector

(print-only-errors)

#|
heap:    | 'free | 'free | 'free | ...                          NEW!
flat:    ... | 'flat | <payload>   | ...
pair:    ... | 'cons | <first-ptr> | <rest-ptr> | ...
closure: ... | 'clos | <code-ptr> | <n-free-vars> | <fv0> | <fv1> | ... | ...  
|#

;; alloc-pointer: (heap-ref 0)
;; active-half: (heap-ref 1)
;; L: (heap-ref 2)
;; R: (heap-ref 3)

(define (init-allocator)
  (heap-set! 0 4)
  (heap-set! 1 'first-half)
  (heap-set! 2 4)
  (heap-set! 3 4)
  (for ([i (in-range 4 (heap-size))])
    (heap-set! i 'free)))



(define (malloc n root1 root2)
  (if (equal? (heap-ref 1) 'second-half)
      (cond
        [(>= (+ n (heap-ref 0)) (heap-size))
         (collect-garbage root1 root2)
         (if (>= (+ n (heap-ref 0)) (heap-size))
             (error 'malloc "out of memory")
             (begin0 (heap-ref 0)
               (heap-set! 0 (+ (heap-ref 0) n))))]
        [else
         (begin0 (heap-ref 0)
           (heap-set! 0 (+ (heap-ref 0) n)))])
      (cond
        [(>= (+ n (heap-ref 0)) (heap-size))
         (collect-garbage root1 root2)
         (if (>= (+ n (heap-ref 0)) 
                 (+ 4 (/ (- (heap-size) 4) 2)))
             (error 'malloc "out of memory")
             (begin0 (heap-ref 0)
               (heap-set! 0 (+ (heap-ref 0) n))))]
        [else
         (begin0 (heap-ref 0)
           (heap-set! 0 (+ (heap-ref 0) n)))])))



(define (collect-garbage root1 root2)
  (init-queue root1 root2)
  (process-queue))


(define (process-queue)
  (define (loop-while)
    (when (< (heap-ref 2) (heap-ref 3))
           (case (heap-ref (heap-ref 2))
             [(flat)
              (heap-set! 2 (+ (heap-ref 2) 2))]
             [(cons)
              (move-or-follow (+ (heap-ref 2) 1))
              (move-or-follow (+ (heap-ref 2) 2))
              (heap-set! 2 (+ (heap-ref 2) 3))]
             [(clos)
              (for ([i (in-range (heap-ref (+ (heap-ref 2) 2)))])
                (move-or-follow (+ (heap-ref 2) 3 i)))
              (heap-set! 2 (+ (heap-ref 2) 3 (heap-ref (+ (heap-ref 2) 2))))]
             [else 'process-queue "unexpected object ~a" (heap-ref (heap-ref 2))]))
    (loop-while))
  (loop-while)
  (if (equal? (heap-ref 1) 'first-half)
      (begin
        (heap-set! 1 'second-half)
        (heap-set! 0 (+ 4 (/ (- (heap-size) 4) 2))))
      (begin
        (heap-set! 1 'first-half)
        (heap-set! 0 4))))


(define (init-queue root1 root2)
  (define heap-start
    (if (equal? (heap-ref 1) 'second-half)
        (+ 4 (/ (- (heap-size) 4) 2))
        4))
  (heap-set! 2 heap-start)
  (heap-set! 3 heap-start)

  (move-or-skip root1)
  (move-or-skip root2)
  (for ([r (in-list (get-root-set))])
    (move-or-skip r)))

(define (move-or-skip x)
  (cond
    [(eq? x #f)
     (void)]                           
    [(integer? x)
     (move-obj x)]                 
    [(pair? x)
     (for ([sub (in-list x)])
       (move-or-skip sub))]
    [else
     (void)]))                    


(define (move-or-follow pointer)
  (when (and (integer? pointer)
             (not (eq? pointer #f)))
    (case (heap-ref pointer)
      [(f)
       (heap-set! pointer (heap-ref (+ 1 pointer)))]
      [else
       (move-obj (heap-ref pointer))])))

(define (move-obj root)
  (case (heap-ref root)
    [(flat)
     (heap-set! root 'f)
     (heap-set! (+ root 1) (heap-ref 3))
     (heap-set! (heap-ref 3) 'flat)
     (heap-set! (+ (heap-ref 3) 1) (heap-ref (+ root 1)))
     (heap-set! 3 (+ (heap-ref 3) 2))] 
    [(cons)
     (heap-set! root 'f)
     (heap-set! (+ root 1) (+ (heap-ref 3) 1))
     (heap-set! (+ root 2) (+ (heap-ref 3) 2))
     (heap-set! (heap-ref 3) 'cons)
     (heap-set! (+ (heap-ref 3) 1) (heap-ref (+ root 1))) 
     (heap-set! (+ (heap-ref 3) 2) (heap-ref (+ root 2))) 
     (heap-set! 3 (+ (heap-ref 3) 3))]  
    [(clos)
     (heap-set! root 'f)
     (heap-set! (+ root 1) (+ (heap-ref 3) 1))
     (heap-set! (+ root 2) (+ (heap-ref 3) 2))
     (for ([i (in-range (heap-ref (+ root 2)))])
       (heap-set! (+ root 3 i) (+ (heap-ref 3) 3 i))) 
     (heap-set! (heap-ref 3) 'clos)
     (heap-set! (+ (heap-ref 3) 1) (heap-ref (+ root 1))) 
     (heap-set! (+ (heap-ref 3) 2) (heap-ref (+ root 2))) 
     (for ([i (in-range (heap-ref (+ root 2)))])
       (heap-set! (+ (heap-ref 3) 3 i) (heap-ref (+ root 3 i))))
     (heap-set! 3 (+ (heap-ref 3) 3 (heap-ref (+ root 2))))]  
    [else
     (error 'move-obj "unexpected object ~a" root)])
  (set-root! root (heap-ref 3)))

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
