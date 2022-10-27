#lang racket
(provide
 dequeue?
 nonempty-dequeue/c
 (contract-out
  [dequeue-empty? (-> dequeue? boolean?)]
  [dequeue-make   (-> dequeue?)]
  [dequeue-push-front   (-> dequeue? any/c dequeue?)]
  [dequeue-push-back    (-> dequeue? any/c dequeue?)]
  [dequeue-pop-front (-> nonempty-dequeue/c any/c)]
  [dequeue-pop-back (-> nonempty-dequeue/c any/c)]
  [dequeue-peek-front   (-> nonempty-dequeue/c any/c)]
  [dequeue-peek-back   (-> nonempty-dequeue/c any/c)]
  [dequeue-join   (-> nonempty-dequeue/c nonempty-dequeue/c any/c)]))
 
(struct dequeue 
  ([front #:mutable] [back #:mutable]) #:transparent)

(struct node
  ([prev #:mutable] [elem] [next #:mutable]) #:transparent)

(define (dequeue-empty? q)
  (null? (dequeue-front q)))

(define nonempty-dequeue/c
  (and/c dequeue? (not/c dequeue-empty?)))

(define (dequeue-make)
  (dequeue null null))

(define (dequeue-push-back q x)
  (define entry (node (dequeue-back q) x null))
  (cond [(dequeue-empty? q)
         (set-dequeue-front! q entry)
         (set-dequeue-back! q entry)]
        [else
         (set-node-next! (dequeue-back q) entry)
         (set-dequeue-back! q entry)]))

(define (dequeue-push-front q x)
  (define entry (node null x (dequeue-front q)))
  (cond [(dequeue-empty? q)
         (set-dequeue-front! q entry)
         (set-dequeue-back! q entry)]
        [else
         (set-node-prev! (dequeue-front q) entry)
         (set-dequeue-front! q entry)]))

(define (dequeue-pop-front q)
  (define entry (dequeue-front q))
  (set-dequeue-front! q (node-next entry))
  (set-node-prev! (dequeue-front q) null)
  (set-node-next! entry null))

(define (dequeue-pop-back q)
  (define entry (dequeue-back q))
  (set-dequeue-back! q (node-prev entry))
  (set-node-next! (dequeue-back q) null)
  (set-node-prev! entry null))

(define (dequeue-peek-front q)
  (if (dequeue-empty? q)
      null
      (node-elem (dequeue-front q))))

(define (dequeue-peek-back q)
  (if (dequeue-empty? q)
      null
      (node-elem (dequeue-back q))))

(define (dequeue-join q1 q2)
  (set-node-next! (dequeue-back q1) (dequeue-front q2))
  (set-node-prev! (dequeue-front q2) (dequeue-back q1))
  (set-dequeue-back! q1 (dequeue-back q2))
  (set-dequeue-front! q2 null))

(define test (dequeue-make))
(dequeue-push-front test 2)
(dequeue-push-back test 1)
(dequeue-push-back test 3)
(dequeue-push-back test 7)


      
     
  
  