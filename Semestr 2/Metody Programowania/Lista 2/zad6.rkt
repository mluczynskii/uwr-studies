#lang racket
(define (suffixes xs)
  (if (null? xs) (cons (list) (list))
      (cons xs (suffixes (cdr xs))))
  )