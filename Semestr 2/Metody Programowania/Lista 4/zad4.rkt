#lang racket

(define-struct node (l val r) #:transparent)
(define-struct leaf () #:transparent)

(define example-tree (node (node (leaf) 1 (leaf))
                           2
                           (node (node (leaf) 3 (leaf))
                                 4
                                 (node (leaf) 5 (leaf)))))

(define (flat-append t xs)
  (cond [(leaf? t)
         xs]
        [else
         (flat-append (node-l t) (cons (node-val t) (flat-append (node-r t) xs)))]))
      
(define (flatten t)
  (flat-append t null))

(flatten example-tree)
  