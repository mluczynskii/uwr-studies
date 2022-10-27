#lang racket

(define empty-queue (cons null null))

(define (empty? q)
  (null? (car q)))

(define (push-back x q)
  (if (null? (car q))
      (cons (cons x null) null)
      (cons (car q) (cons x (cdr q)))))

(define (front q)
  (if (empty? q)
      null
      (caar q)))

(define (reverse xs)
  (foldl cons null xs))

(define (pop q)
  (cond [(empty? q)
         empty-queue]
        [(null? (cdar q))
         (cons (reverse (cdr q)) null)]
        [else
         (cons (cdar q) (cdr q))]))

(define example-q '((1 2 3) 6 5 4))
  

