#lang plait

(define-type Prop
  (var [v : String])
  (conj [l : Prop] [r : Prop])
  (disj [l : Prop] [r : Prop])
  (neg [f : Prop]))

(define example-prop
  (conj
   (disj (var "x") (neg (var "y")))  
   (neg (var "x"))))

(define (free-vars [formula : Prop])
  (local
    [(define (it [f : Prop]
                 [acc : (Listof String)])
       (cond [(var? f)
              (cons (var-v f) acc)]
             [(neg? f)
              (it (neg-f f) acc)]
             [(conj? f)
              (append (it (conj-l f) acc) (it (conj-r f) acc))]
             [(disj? f)
              (append (it (disj-l f) acc) (it (disj-r f) acc))]))
     (define (remove-duplicates [xs : (Listof 'a)])
       (foldr (lambda (x y) (cons x (filter (lambda (z) (not (equal? x z))) y))) empty xs))]
    (remove-duplicates (it formula empty))))
  
(free-vars example-prop)