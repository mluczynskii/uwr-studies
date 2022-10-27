#lang racket

(define (list->llist xs)
  (lambda (ys) (append xs ys)))

(define (llist->list f)
  (f null))

(define llist-null (lambda (xs) xs))

(define (singleton x)
  (lambda (xs) (cons x xs)))

(define (llist-append f g)
  (lambda (xs) (f (g xs))))

(define (foldr-llist-reverse xs)
  (llist->list (foldr (lambda (y ys) (llist-append ys (singleton y)))
          llist-null
          xs)))