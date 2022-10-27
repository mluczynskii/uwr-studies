#lang racket

(define-struct matrix (a b c d))

(define (matrix-mult m n)
  (let ([m-a (matrix-a m)] [m-b (matrix-b m)] [m-c (matrix-c m)] [m-d (matrix-d m)]
        [n-a (matrix-a n)] [n-b (matrix-b n)] [n-c (matrix-c n)] [n-d (matrix-d n)])
  (matrix (+ (* m-a n-a) (* m-b n-c))
           (+ (* m-a n-b) (* m-b n-d))
           (+ (* m-c n-a) (* m-d n-c))
           (+ (* m-c n-b) (* m-d n-d))))
)

(define matrix-id (matrix 1 0 0 1))

(define xd (matrix 2 1 3 7))

(define (matrix-expt m k)
  (define (it k n)
    (cond [(= k 0) matrix-id]
          [(= k 1) n]
          [else (it (- k 1) (matrix-mult n m))]))
  (it k m))

(define (print-matrix m)
  (list (matrix-a m) (matrix-b m) (matrix-c m)(matrix-d m)))

(define (fib-matrix n)
  (matrix-b (matrix-expt (matrix 1 1 1 0) n)))