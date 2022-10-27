#lang racket

(define-struct leaf () #:transparent)
(define-struct node (l val r) #:transparent)

(define example-tree (node (node (leaf) 1 (leaf))
                           2
                           (node (node (leaf) 3 (leaf))
                                 4
                                 (node (leaf) 5 (leaf)))))

(define (flat-append t xs)
  (cond [(leaf? t)
         null]
        [(leaf? (node-r t))
         (cons (node-val t) xs)]
        [(leaf? (node-l t))
         (cons (node-val t) (flat-append (node-r t) xs))]
        [else
         (flat-append (node-l t) (cons (node-val t) (flat-append (node-r t) xs)))]))
      
(define (flatten t)
  (flat-append t null))

(define (bst? t)
  (define lt (flatten t))
  (equal? lt (sort lt <)))

(define (sum-paths t)
  (define (f t acc)
    (if (leaf? t)
        (leaf)
        (node (f (node-l t) (+ acc (node-val t))) (+ (node-val t) acc) (f (node-r t) (+ acc (node-val t))))))
  (f t 0))

(bst? example-tree)
(sum-paths example-tree)




      
  