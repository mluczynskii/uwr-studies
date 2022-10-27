#lang racket
(require data/heap)
(provide sim? wire?
         (contract-out
          [make-sim        (-> sim?)]
          [sim-wait!       (-> sim? positive? void?)]
          [sim-time        (-> sim? real?)]
          [sim-add-action! (-> sim? positive? (-> any/c) void?)]

          [make-wire       (-> sim? wire?)]
          [wire-on-change! (-> wire? (-> any/c) void?)]
          [wire-value      (-> wire? boolean?)]
          [wire-set!       (-> wire? boolean? void?)]

          [bus-value (-> (listof wire?) natural?)]
          [bus-set!  (-> (listof wire?) natural? void?)]

          [gate-not  (-> wire? wire? void?)]
          [gate-and  (-> wire? wire? wire? void?)]
          [gate-nand (-> wire? wire? wire? void?)]
          [gate-or   (-> wire? wire? wire? void?)]
          [gate-nor  (-> wire? wire? wire? void?)]
          [gate-xor  (-> wire? wire? wire? void?)]

          [wire-not  (-> wire? wire?)]
          [wire-and  (-> wire? wire? wire?)]
          [wire-nand (-> wire? wire? wire?)]
          [wire-or   (-> wire? wire? wire?)]
          [wire-nor  (-> wire? wire? wire?)]
          [wire-xor  (-> wire? wire? wire?)]

          [flip-flop (-> wire? wire? wire? void?)]))

(struct sim ([time #:mutable] [action-q #:mutable]))
(struct wire ([value #:mutable] [actions #:mutable] [sim]))
(struct action ([function] [time]))

(define/contract (action<= a b)
  (-> action? action? boolean?)
  (<= (action-time a) (action-time b)))

(define/contract (make-sim)
  (-> sim?)
  (sim 0 (make-heap action<=)))

(define/contract (sim-wait! simulation time)
  (-> sim? positive? void?)
  (define action-q (sim-action-q simulation))
  (define current-time (sim-time simulation))
  (cond [(= (heap-count action-q) 0)
         (set-sim-time! simulation (+ current-time time))]
        [else
         (define next-action (heap-min action-q))
         (define time-to-next-action (- (action-time next-action) current-time))
         (cond [(<= time-to-next-action time)
                (define remaining-time (- time time-to-next-action))
                (set-sim-time! simulation (action-time next-action))
                ((action-function next-action))
                (heap-remove-min! action-q)
                (sim-wait! simulation remaining-time)]
               [else
                (set-sim-time! simulation (+ current-time time))])]))

(define/contract (sim-add-action! simulation time function)
  (-> sim? positive? (-> any/c) void?)
  (define current-time (sim-time simulation))
  (define new-action (action function (+ current-time time)))
  (heap-add! (sim-action-q simulation) new-action))

(define/contract (make-wire simulation)
  (-> sim? wire?)
  (wire #f null simulation))

(define/contract (wire-on-change! wire function)
  (-> wire? (-> any/c) void?)
  (set-wire-actions! wire (cons function (wire-actions wire)))
  (function))

(define/contract (wire-set! wire value)
  (-> wire? boolean? void?)
  (define (call-actions xs)
    (if (null? xs)
        (void)
        (begin
          ((car xs))
          (call-actions (cdr xs)))))
  (if (eq? (wire-value wire) value)
      (void)
      (begin
        (set-wire-value! wire value)
        (call-actions (wire-actions wire)))))

(define/contract (gate-not output input)
  (-> wire? wire? void?)
  (if (eq? (wire-sim input) (wire-sim output))
      (let ([simulation (wire-sim output)]
            [not-action (lambda () (wire-set! output (not (wire-value input))))])
        (wire-on-change! input (lambda () (sim-add-action! simulation 1 not-action))))
      (error 'gate-not "Input and output wires must be in the same simulation")))

(define/contract (gate-binary output in1 in2 f delay symbol)
  (-> wire? wire? wire? (-> boolean? boolean? boolean?) positive? symbol? void?)
  (if (eq? (wire-sim in1) (wire-sim in2))
      (let ([simulation (wire-sim in1)])
        (if (eq? (wire-sim output) simulation)
            (let ([action (lambda () (wire-set! output (f (wire-value in1) (wire-value in2))))])
              (begin
                (wire-on-change! in1 (lambda () (sim-add-action! simulation delay action)))
                (wire-on-change! in2 (lambda () (sim-add-action! simulation delay action)))))
            (error symbol "Output wire must be in the same simulation as input wires")))
      (error symbol "Input wires must be in the same simulation")))

(define/contract (gate-and output in1 in2)
  (-> wire? wire? wire? void?)
  (gate-binary output in1 in2 (lambda (x y) (and x y)) 1 'gate-and))

(define/contract (gate-nand output in1 in2)
  (-> wire? wire? wire? void?)
  (gate-binary output in1 in2 (lambda (x y) (nand x y)) 1 'gate-nand))

(define/contract (gate-or output in1 in2)
  (-> wire? wire? wire? void?)
  (gate-binary output in1 in2 (lambda (x y) (or x y)) 1 'gate-or))

(define/contract (gate-nor output in1 in2)
  (-> wire? wire? wire? void?)
  (gate-binary output in1 in2 (lambda (x y) (nor x y)) 1 'gate-nor))

(define/contract (gate-xor output in1 in2)
  (-> wire? wire? wire? void?)
  (gate-binary output in1 in2 (lambda (x y) (xor x y)) 2 'gate-xor))

(define/contract (wire-not input)
  (-> wire? wire?) 
  (let* ([simulation (wire-sim input)]
         [output (make-wire simulation)]
         [not-action (lambda () (wire-set! output (not (wire-value input))))])
    (begin
      (wire-on-change! input (lambda () (sim-add-action! simulation 1 not-action)))
      output)))

(define/contract (wire-binary in1 in2 f delay symbol)
  (-> wire? wire? (-> boolean? boolean? boolean?) positive? symbol? wire?)
  (if (eq? (wire-sim in1) (wire-sim in2))
      (let* ([simulation (wire-sim in1)]
             [output (make-wire simulation)]
             [action (lambda () (wire-set! output (f (wire-value in1) (wire-value in2))))])
        (begin
          (wire-on-change! in1 (lambda () (sim-add-action! simulation delay action)))
          (wire-on-change! in2 (lambda () (sim-add-action! simulation delay action)))
          output))
      (error symbol "Input wires must be in the same simulation")))

(define/contract (wire-and in1 in2)
  (-> wire? wire? wire?)
  (wire-binary in1 in2 (lambda (x y) (and x y)) 1 'wire-and))

(define/contract (wire-nand in1 in2)
  (-> wire? wire? wire?)
  (wire-binary in1 in2 (lambda (x y) (nand x y)) 1 'wire-nand))

(define/contract (wire-or in1 in2)
  (-> wire? wire? wire?)
  (wire-binary in1 in2 (lambda (x y) (or x y)) 1 'wire-or))

(define/contract (wire-nor in1 in2)
  (-> wire? wire? wire?)
  (wire-binary in1 in2 (lambda (x y) (nor x y)) 1 'wire-nor))

(define/contract (wire-xor in1 in2)
  (-> wire? wire? wire?)
  (wire-binary in1 in2 (lambda (x y) (xor x y)) 2 'wire-xor))

(define (bus-set! wires value)
  (match wires
    ['() (void)]
    [(cons w wires)
     (begin
       (wire-set! w (= (modulo value 2) 1))
       (bus-set! wires (quotient value 2)))]))

(define (bus-value ws)
  (foldr (lambda (w value) (+ (if (wire-value w) 1 0) (* 2 value)))
         0
         ws))

(define (flip-flop out clk data)
  (define sim (wire-sim data))
  (define w1  (make-wire sim))
  (define w2  (make-wire sim))
  (define w3  (wire-nand (wire-and w1 clk) w2))
  (gate-nand w1 clk (wire-nand w2 w1))
  (gate-nand w2 w3 data)
  (gate-nand out w1 (wire-nand out w3)))