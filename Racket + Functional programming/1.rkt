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


