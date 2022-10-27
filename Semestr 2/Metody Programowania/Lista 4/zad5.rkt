#lang racket

(define-struct node (l val r) #:transparent)
(define-struct leaf () #:transparent)

(define example-tree (node (node (leaf) 1 (leaf))
                           2
                           (node (node (leaf) 3 (leaf))
                                 4
                                 (node (leaf) 5 (leaf)))))

(define (insert-bst val t)
  (if (leaf? t)
      (node (leaf) val (leaf))
      (cond [(< val (node-val t))
             (node (insert-bst val (node-l t)) (node-val t) (node-r t))]
            [else
             (node (node-l t) (node-val t) (insert-bst val (node-r t)))])))

(define (flat-append t xs)
  (cond [(leaf? t)
         xs]
        [else
         (flat-append (node-l t) (cons (node-val t) (flat-append (node-r t) xs)))]))
      
(define (flatten t)
  (flat-append t null))
  
(define (treesort xs)
  (define tree-xs
    (foldl insert-bst (leaf) xs))
  (flatten tree-xs))

(treesort '(4 3 1 0 14 -5 4))