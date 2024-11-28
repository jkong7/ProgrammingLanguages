#lang plai

; Jonathan Kong
; CS 321
; Winter 2025 

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

; Tree datatype 
(define-type Tree
  [positive-leaf (val natural?)]
  [negative-leaf (val natural?)]
  [interior-node (left Tree?) (right Tree?)])

; Trees for testing

(define tree1
  (interior-node
   (interior-node
    (positive-leaf 5)
    (negative-leaf 3))
   (interior-node
    (positive-leaf 10)
    (negative-leaf 20))))

(define tree2
  (interior-node
   (interior-node
    (positive-leaf 5)
    (negative-leaf 4))
   (positive-leaf 3)))

(define tree3
  (interior-node
   (positive-leaf 8)
   (negative-leaf 2)))

(define tree4
  (interior-node
   (interior-node
    (interior-node
     (positive-leaf 15)
     (negative-leaf 25))
    (positive-leaf 5))
   (negative-leaf 10)))

(define tree5
  (positive-leaf 42))

(define tree6
  (interior-node
   (interior-node
    (positive-leaf 13)
    (negative-leaf 10))
   (interior-node
    (positive-leaf 37)
    (negative-leaf 40))))

(define tree7
  (interior-node
   (interior-node
    (positive-leaf 13)
    (interior-node
     (positive-leaf 3)
     (negative-leaf 13)))
   (interior-node
    (positive-leaf 37)
    (negative-leaf 40))))

(define tree8
  (interior-node
   (interior-node
    (positive-leaf 0)
    (interior-node
     (positive-leaf 3)
     (negative-leaf 3)))
   (interior-node
    (positive-leaf 40)
    (negative-leaf 40))))

(define tree9
  (interior-node
   (interior-node
    (positive-leaf 5)
    (negative-leaf 5))
   (interior-node
    (positive-leaf 10)
    (negative-leaf 10))))

(define tree10
  (interior-node
   (interior-node
    (positive-leaf 3)
    (negative-leaf 2)) 
   (interior-node
    (positive-leaf 10)
    (negative-leaf 10))))

(define tree11
  (interior-node
   (interior-node
    (negative-leaf 3)
    (positive-leaf 2)) 
   (interior-node
    (negative-leaf 10)
    (positive-leaf 10))))

(define tree12
  (interior-node
   (interior-node
    (negative-leaf 0)
    (interior-node
     (negative-leaf 3)
     (positive-leaf 3)))
   (interior-node
    (negative-leaf 40)
    (positive-leaf 40))))

(define tree13
  (interior-node
   (interior-node
    (negative-leaf 1)
    (positive-leaf 4)) 
   (interior-node
    (negative-leaf 8)
    (positive-leaf 12))))

(define tree14
  (interior-node
   (interior-node
    (positive-leaf 1)
    (positive-leaf 6)) 
   (interior-node
    (negative-leaf 6)
    (positive-leaf 14))))

(define tree15
  (interior-node
   (interior-node
    (negative-leaf 14)
    (negative-leaf 9)) 
   (interior-node
    (negative-leaf 21)
    (negative-leaf 1))))

(define tree16
  (interior-node
   (interior-node
    (negative-leaf 3)
    (positive-leaf 2)) 
   (interior-node
    (negative-leaf 10)
    (positive-leaf 10))))

(define tree17
  (interior-node
    (interior-node
      (positive-leaf 5)
      (negative-leaf 3))
    (positive-leaf 7)))

(define tree18
  (interior-node
    (positive-leaf 5)
    (positive-leaf 7)))

(define tree19
  (interior-node
    (negative-leaf 10)
    (interior-node
      (negative-leaf 6)
      (positive-leaf 9))))

(define tree20
  (positive-leaf 9))

(define tree21
  (interior-node
    (interior-node
      (negative-leaf 4)
      (negative-leaf 3))
    (negative-leaf 5)))






; 1.
; contains? : Tree integer -> boolean

(define (contains? tree num)
  (type-case Tree tree
  [positive-leaf (val) (equal? val num)]
  [negative-leaf (val) (equal? (- val) num)]
  [interior-node (l r) (or (contains? l num) (contains? r num))]))

(test (contains? tree1 -3) true)

(test (contains? tree1 10) true)

(test (contains? tree1 30) false)

(test (contains? tree2 -4) true)

(test (contains? tree2 5) true)

(test (contains? tree2 2) false)

; 2.
; smallest: Tree -> integer

(define (smallest tree)
  (type-case Tree tree
    [positive-leaf (val) val]
    [negative-leaf (val) (- val)]
    [interior-node (l r) (min (smallest l) (smallest r))]))

(test (smallest tree1) -20)

(test (smallest tree2) -4)

(test (smallest tree3) -2)

(test (smallest tree4) -25)

(test (smallest tree5) 42)

; 3.
; balanced?: Tree -> boolean

; Helper function to compute the sum of a tree's leaves
; sum: Tree -> integer 
(define (sum tree)
  (type-case Tree tree
    [positive-leaf (val) val]
    [negative-leaf (val) (- val)]
    [interior-node (l r) (+ (sum l) (sum r))]))

(define (balanced? tree)
  (equal? (sum tree) 0))

(test (balanced? tree1) false)

(test (balanced? tree2) false)

(test (balanced? tree3) false)

(test (balanced? tree6) true)

(test (balanced? tree7) true)

; 4.
; deep-balanced?: Tree -> boolean
(define (deep-balanced? tree)
  (type-case Tree tree
    [positive-leaf (val) true]
    [negative-leaf (val) true]
    [interior-node (l r) (and
                          (equal? (+ (sum l) (sum r)) 0)
                          (deep-balanced? l) (deep-balanced? r))]))

(test (deep-balanced? tree5) true)

(test (deep-balanced? tree7) false)

(test (deep-balanced? tree8) true)

(test (deep-balanced? tree9) true)

(test (deep-balanced? tree10) false)

; 5.
; negate: Tree -> Tree
(define (negate tree)
  (type-case Tree tree
    [positive-leaf (val) (negative-leaf val)]
    [negative-leaf (val) (positive-leaf val)]
    [interior-node (l r) (interior-node
                          (negate l)
                          (negate r))]))

(test (negate tree10) tree11)

(test (negate tree11) tree10)

(test (negate tree8) tree12)

(test (negate tree12) tree8)

; 6.
; add: Tree integer -> Tree
(define (add tree num)
  (type-case Tree tree
    [positive-leaf (val) (if (< (+ val num) 0) (negative-leaf (- (+ val num))) (positive-leaf (+ val num)))]
    [negative-leaf (val) (if (< (+ (- val) num) 0) (negative-leaf (- (+ (- val) num))) (positive-leaf (+ (- val) num)))]
    [interior-node (l r) (interior-node
                          (add l num)
                          (add r num))]))

(test (add tree11 2) tree13)

(test (add tree11 4) tree14)

(test (add tree11 -11) tree15)

; 7.
; positive-thinking: Tree -> (U Tree false)

(define (positive-thinking tree)
  (type-case Tree tree
    [positive-leaf (val) (positive-leaf val)]      
    [negative-leaf (val) false]                  
    [interior-node (l r)
     (let ([left (positive-thinking l)]            
           [right (positive-thinking r)])          
       (cond
         [(and (equal? left false) (equal? right false)) false] 
         [(equal? left false) right]                           
         [(equal? right false) left]                         
         [else (interior-node left right)]))]))

(test (positive-thinking tree17) tree18)

(test (positive-thinking tree19) tree20)

(test (positive-thinking tree21) false)


