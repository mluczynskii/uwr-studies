#lang plait

(define-type (Tree 'a)
  (leaf)
  (node [l : (Tree 'a)] [elem : 'a] [r : (Tree 'a)]))

(define example-tree
  (node
   (node (leaf) 1 (leaf))
   2
   (node
    (node (leaf) 3 (leaf))
    4
    (node (leaf) 5 (leaf)))))

(define (process-tree [node-f : ('a 'b 'c 'b -> 'b)]
                      [leaf-f : ('a -> 'b)]
                      [acc-l : ('a 'c -> 'a)]
                      [acc-r : ('a 'c -> 'a)]
                      [acc : 'a]
                      [tree : (Tree 'c)])
  (if (leaf? tree)
      (leaf-f acc)
      (node-f acc
              (process-tree node-f
                            leaf-f
                            acc-l
                            acc-r
                            (acc-l acc (node-elem tree))
                            (node-l tree))
              (node-elem tree)
              (process-tree node-f
                            leaf-f
                            acc-l
                            acc-r
                            (acc-r acc (node-elem tree))
                            (node-r tree)))))
                
(define (sum-paths [t : (Tree Number)])
  (process-tree (lambda (acc process-l elem process-r) (node process-l (+ acc elem) process-r))
                (lambda (acc) (leaf))
                (lambda (acc elem) (+ acc elem))
                (lambda (acc elem) (+ acc elem))
                0
                t))
                
(bst? example-tree)                 
  