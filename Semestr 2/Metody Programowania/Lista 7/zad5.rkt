#lang racket

;( parametric- >/ c [ a b ] (- > a b a ) )

(define/contract (f1 x y)
  (parametric->/c [a b] (-> a b a))
  x)

;( parametric- >/ c [ a b c ] (- > (- > a b c ) (- > a b ) a c ) )

(define/contract (f2 f g x)
  (parametric->/c [a b c] (-> (-> a b c) (-> a b) a c))
  (f x (g x)))

;( parametric- >/ c [ a b c ] (- > (- > b c ) (- > a b ) (- > a c ) ) )

(define/contract (f3 f g)
  (parametric->/c [a b c] (-> (-> b c) (-> a b) (-> a c)))
  (lambda (x) (f (g x))))

;( parametric- >/ c [ a ] (- > (- > (- > a a ) a ) a ) )

(define/contract (f4 f)
  (parametric->/c [a] (-> (-> (-> a a) a) a))
  (f (lambda (x) x)))