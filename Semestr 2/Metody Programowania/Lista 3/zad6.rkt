#lang racket

(define empty-set (lambda (x) #f))

(define (singleton a)
  (lambda (x) (equal? x a)))

(define (in a s)
  (s a))

(define (union s t)
  (lambda (x) (or (s x) (t x))))

(define (intersect s t)
  (lambda (x) (and (s x) (t x))))
