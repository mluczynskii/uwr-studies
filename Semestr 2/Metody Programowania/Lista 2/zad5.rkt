#lang racket
(define (maximum xs)
  (if (null? xs) -inf.0
      (let ([temp (maximum (cdr xs))])
        (if (> (car xs) temp) (car xs) temp)
        )
      )
  )