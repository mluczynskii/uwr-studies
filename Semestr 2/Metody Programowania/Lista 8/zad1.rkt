#lang racket

(define example-list (mcons 1 (mcons 2 (mcons 3 null))))

(define (mreverse! xs)
  (define (pom prev curr)
    (set-mcdr! curr prev)
    curr)
  (define (it curr prev)
    (cond [(null? curr)
           "XD"]
          [else
           (it (mcdr curr) (pom prev curr))]))
  (it xs null))
    
