#lang racket
(define (sorted? xs)
  (if (or (null? xs) (= (length xs) 1)) #t
      (let ([x (car xs)] [xs (cdr xs)])
        (if (and (<= x (car xs)) (sorted? xs)) #t #f)
        )
      )
  )
      