#lang racket

(define/contract (suffixes xs)
  (parametric->/c [a] (-> (listof a) (listof (listof a))))
  (match xs
    ['() null]
    [(cons x xs) (cons xs (suffixes xs))]))