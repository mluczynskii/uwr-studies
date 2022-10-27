#lang racket

(define (min-list xs)
  (define (it xs acc)
    (cond ([null? xs] acc)
          ([< (car xs) acc] (it (cdr xs) (car xs)))
          (else (it (cdr xs) acc))))
  (it xs +inf.0))

(define (select xs)
  (define x (min-list xs))
  (define (it xs acc)
    (if (= x (car xs))
        (append (list x) (append acc (cdr xs)))
        (it (cdr xs) (append acc (list (car xs))))))
  (it xs (list)))

(define (select-sort xs)
  (if (null? xs)
      (list)
      (let ([xs (select xs)])
        (append (list (car xs)) (select-sort (cdr xs))))))