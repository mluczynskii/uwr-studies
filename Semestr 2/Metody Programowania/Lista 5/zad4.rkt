#lang plait

(define (remove [x : 'a]
                [xs : (Listof 'a)])
  (cond [(empty? xs)
         xs]
        [(equal? x (first xs))
         (rest xs)]
        [else
         (cons (first xs) (remove x (rest xs)))]))

(define (remove-duplicates [xs : (Listof 'a)])
  (foldr
   (lambda (x ys) (cons x (filter (lambda (y) (not (equal? x y))) ys)))
   empty
   xs))

(define (pom [x : 'a]
             [xs : (Listof 'a)])
  (map (lambda (ys) (cons x ys)) (perms xs)))
                            
(define (perms [xs : (Listof 'a)])
  (local
    [(define (it [ys : (Listof 'a)]
                 [acc : (Listof (Listof 'a))]
                 [xs : (Listof 'a)])
       (if (empty? ys)
           acc
           (it (rest ys)
               (append (pom (first ys) (remove (first ys) xs)) acc)
               xs)))]
    (if (= (length xs) 1)
        (list xs)
        (remove-duplicates (it xs empty xs)))))
  

      
  