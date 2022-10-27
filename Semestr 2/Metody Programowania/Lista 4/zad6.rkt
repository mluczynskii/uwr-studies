#lang racket

(define-struct node (l val r) #:transparent)
(define-struct leaf () #:transparent)

(define example-tree (node (node (leaf) 1 (leaf))
                           2
                           (node (node (leaf) 3 (leaf))
                                 4
                                 (node (leaf) 5 (leaf)))))

(define (find-new-root t)
  (cond [(leaf? (node-l t))
         (node-val t)]
        [else
         (find-new-root (node-l t))]))

(define (delete elem t)
  (if (leaf? t)
      (leaf)
      (cond [(= elem (node-val t))
             (cond [(leaf? (node-r t))
                    (node-l t)]
                   [else
                    (let ([n-val (find-new-root (node-r t))])
                      (node (node-l t) n-val (delete n-val (node-r t))))])]
            [(< elem (node-val t))
             (node (delete elem (node-l t)) (node-val t) (node-r t))]
            [(> elem (node-val t))
             (node (node-l t) (node-val t) (delete elem (node-r t)))])))