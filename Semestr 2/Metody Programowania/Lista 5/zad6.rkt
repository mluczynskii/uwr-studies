#lang plait

(define-type (rose-tree 'a)
  (leaf)
  (node [v : 'a]
        [sub : (Listof (rose-tree 'a))]))

(define (flatten [t : (rose-tree 'a)])
  
                    