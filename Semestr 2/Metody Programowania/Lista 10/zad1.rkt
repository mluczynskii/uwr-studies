#lang plait

(module+ test
  (print-only-errors #t))

;; abstract syntax -------------------------------

(define-type Op
  (add)
  (sub)
  (mul)
  (div))

(define-type Exp
  (numE [n : Number])
  (opE [op : Op] [cs : (Listof Exp)]))

;; parse ----------------------------------------

(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s)
     (numE (s-exp->number s))]
    [(s-exp-match? `{SYMBOL ANY ...} s)
     (opE (parse-op (s-exp->symbol (first (s-exp->list s))))
          (parse-exp-list (rest (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(define (parse-exp-list [cs : (Listof S-Exp)]) : (Listof Exp)
  (type-case (Listof S-Exp) cs
    [empty
     empty]
    [(cons c cs)
     (cons (parse c) (parse-exp-list cs))])) 

(define (parse-op [op : Symbol]) : Op
  (cond
    [(eq? op '+) (add)]
    [(eq? op '-) (sub)]
    [(eq? op '*) (mul)]
    [(eq? op '/) (div)]
    [else (error 'parse "unknown operator")]))
  
;; eval --------------------------------------

(define-type-alias Value Number)

(define (add-l [xs : (Listof Value)]) : Value
  (type-case (Listof Value) xs
    [empty
     0]
    [(cons x xs)
     (+ x (add-l xs))]))

(define (sub-l [xs : (Listof Value)]) : Value
  (type-case (Listof Value) xs
    [empty
     0]
    [(cons x xs)
     (- x (sub-l xs))]))

(define (mul-l [xs : (Listof Value)]) : Value
  (type-case (Listof Value) xs
    [empty
     1]
    [(cons x xs)
     (* x (mul-l xs))]))

(define (div-l [xs : (Listof Value)]) : Value
  (type-case (Listof Value) xs
    [empty
     1]
    [(cons x xs)
     (if (= x 0)
         (error 'eval "Division by zero")
         (/ x (div-l xs)))]))

(define (op->proc [op : Op]) : ((Listof Value) -> Value)
  (type-case Op op
    [(add) add-l]
    [(sub) sub-l]
    [(mul) mul-l]
    [(div) div-l]))

(define (eval [e : Exp]) : Value
  (type-case Exp e
    [(numE n) n]
    [(opE o cs) ((op->proc o) (eval-list cs))]))

(define (eval-list [cs : (Listof Exp)]) : (Listof Value)
  (type-case (Listof Exp) cs
    [empty
     empty]
    [(cons c cs)
     (cons (eval c) (eval-list cs))]))

(define (run [e : S-Exp]) : Value
  (eval (parse e)))

;; printer ———————————————————————————————————-

(define (print-value [v : Value]) : Void
  (display v))

(define (main [e : S-Exp]) : Void
  (print-value (eval (parse e))))