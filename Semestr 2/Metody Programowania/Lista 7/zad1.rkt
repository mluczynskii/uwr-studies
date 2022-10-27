#lang plait

(define-type (Tree 'a)
  (leaf)
  (2-node [l : (Tree 'a)] [elem : 'a] [r : (Tree 'a)])
  (3-node [l : (Tree 'a)] [elem-l : 'a] [m : (Tree 'a)] [elem-r : 'a] [r : (Tree 'a)]))

(define example-tree
  (2-node
   (3-node
    (2-node (leaf) 1 (leaf))
    4
    (2-node (leaf) 3 (leaf))
    2
    (3-node (leaf) 5 (leaf) 6 (leaf)))
   10
   (2-node
    (2-node (leaf) 11 (leaf))
    12
    (3-node (leaf) 15 (leaf) 14 (leaf)))))

(define (check [comp : (Number Number -> Boolean)]
               [elem : Number]
               [t : (Tree Number)])
  (type-case (Tree Number) t
    [(leaf) #t]
    [(2-node l x r)
     (and (comp elem x)
          (check comp elem l) (check comp elem r))]
    [(3-node l a m b r)
     (and (comp elem a) (comp elem b)
          (check comp elem l) (check comp elem m) (check comp elem r))]))

(define (check-if-inbetween [upperbound : Number]
                            [lowerbound : Number]
                            [t : (Tree Number)])
  (type-case (Tree Number) t
    [(leaf) #t]
    [(2-node l x r)
     (and (< x upperbound) (> x lowerbound)
          (check-if-inbetween upperbound lowerbound l)
          (check-if-inbetween upperbound lowerbound r))]
    [(3-node l a m b r)
     (and (< a upperbound) (> a lowerbound)
          (< b upperbound) (> b lowerbound)
          (check-if-inbetween upperbound lowerbound l)
          (check-if-inbetween upperbound lowerbound m)
          (check-if-inbetween upperbound lowerbound r))]))

(define (tree-height [t : (Tree 'a)])
  (type-case (Tree 'a) t
    [(leaf) 0]
    [(2-node l x r)
     (+ 1 (max (tree-height l) (tree-height r)))]
    [(3-node l a m b r)
     (+ 1 (max (tree-height l) (max (tree-height m) (tree-height r))))]))

(define (2-3-Tree? [t : (Tree Number)])
  (type-case (Tree Number) t
    [(leaf) #t]
    [(2-node l x r)
     (and
      (= (tree-height l) (tree-height r))
      (and (check > x l) (check < x r))
      (and (2-3-Tree? l) (2-3-Tree? r)))]
    [(3-node l a m b r)
     (and
      (= (tree-height l) (tree-height m))
      (= (tree-height l) (tree-height r))
      (= (tree-height m) (tree-height r))
      (and (check > a l) (check > b l) (check < a r) (check < b r))
      (> a b)
      (check-if-inbetween a b m))])) 
      