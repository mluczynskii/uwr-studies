#lang plait

(define (fib n cont)
  (if (< n 2)
      (cont n)
      (fib (- n 1) (lambda (a) (fib (- n 2) (lambda (b) (cont (+ a b)))))))) 