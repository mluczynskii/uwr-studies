#lang racket

(define (split xs)
  (define (it n xs acc)
    (cond [(null? xs) (append (list acc) xs)]
          [(= n 0) (append (list acc) xs)]
          [else (let ([acc (append acc (list (car xs)))])
                  (it (- n 1) (cdr xs) acc))]))
  (define n (quotient (length xs) 2))
  (it n xs (list)))

(define (merge xs ys)
  (define (it xs ys acc)
    (cond [(null? xs) (append acc ys)]
          [(null? ys) (append acc xs)]
          [else (let ([x (car xs)] [y (car ys)])
                  (if (<= x y)
                      (it (cdr xs) ys (append acc (list x)))
                      (it xs (cdr ys) (append acc (list y)))))]))
  (it xs ys (list)))

(define (merge-sort xs)
  (if (null? xs)
      (list)
      (if (= (length xs) 1)
          xs
          (let* ([xs (split xs)] [xs1 (car xs)] [xs2 (cdr xs)])
            (merge (merge-sort xs1) (merge-sort xs2))))))