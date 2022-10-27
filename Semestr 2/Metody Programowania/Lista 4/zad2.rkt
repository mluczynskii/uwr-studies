#lang racket

(define-struct leaf () #:transparent)
(define-struct node (l val r) #:transparent)

(define example-tree (node (node (leaf) 1 (leaf))
                           2
                           (node (node (leaf) 3 (leaf))
                                 4
                                 (node (leaf) 5 (leaf)))))

(define (fold-tree f t x)
  (if (leaf? t)
      x
      (f (fold-tree f (node-l t) x) (node-val t) (fold-tree f (node-r t) x)))) 

(define (tree-sum t)
  (fold-tree (lambda (sum-l val sum-r)
               (+ sum-l val sum-r)) t 0))

(define (tree-flip t)
  (fold-tree (lambda (flip-l val flip-r)
               (node flip-r val flip-l)) t (leaf)))

(define (tree-height t)
  (fold-tree (lambda (height-l val height-r)
               (+ (max height-l height-r) 1)) t 0))

(define (tree-span t)
  (cons
   (fold-tree min t +inf.0)
   (fold-tree max t -inf.0)))

(define (flatten t)
  (fold-tree (lambda (flat-l val flat-r)
               (append flat-l (list val) flat-r)) t null))
  





               
