#lang racket

(define (fib n)
  (cond [(= n 0) 0]
        [(= n 1) 1]
        [else (+ (fib (- n 1)) (fib (- n 2)))])
)

(define (fib-iter n)
  (define (it n a b)
    (cond [(= n 0) a]
          [(= n 1) b]
          [else (it (- n 1) b (+ a b))])
  )
  (it n 0 1)
)