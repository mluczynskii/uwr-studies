#lang racket

(define (negatives n)
  (build-list n (lambda (x) (* -1 (+ x 1)))))
(negatives 5)

(define (reciprocals n)
  (build-list n (lambda (x) (/ 1 (+ x 1)))))
(reciprocals 5)

(define (evens n)
  (build-list n (lambda (x) (* 2 x))))
(evens 5)

(define (identityM n)
  (build-list n (lambda (x)
                  (build-list n (lambda (y)
                                  (if (= y x)
                                      1
                                      0))))))
(identityM 5)