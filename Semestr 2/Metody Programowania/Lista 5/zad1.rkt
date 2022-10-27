#lang plait

(define (jeden a b) a) ; ('a 'b -> a')

(define (dwa f g x) ; (('a 'b -> 'c) ('a -> 'b) 'a -> 'c)
  (f x (g x)))

(define (trzy [f : (('a -> 'a) -> 'a)]) ; ((('a -> 'a) -> 'a) -> 'a)
  (f (lambda (x) x)))

(define (cztery f g) ; (('a -> 'b) ('a -> 'c) -> ('a -> ('b * 'c)))
  (lambda (x) (pair (f x) (g x))))

(define (piec [f : ('a -> (Optionof ('a * 'b)))]
              [x : 'a]) 
  (list (snd (some-v (f x))))) ; (('a -> (Optionof ('a * 'b))) 'a -> (Listof 'b))


  

  