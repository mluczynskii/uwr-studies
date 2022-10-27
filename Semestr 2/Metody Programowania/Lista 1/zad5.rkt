#lang racket

(define (sum-pow a b)
  (+ (* a a) (* b b)))

(define (zad5 x y z)
  (cond [(and (<= x y) (<= x z)) (sum-pow y z)]
        [(and (<= y x) (<= y z)) (sum-pow x z)]
        [(and (<= z x) (<= z y)) (sum-pow x y)]
        [else (sum-pow x y)]))
  