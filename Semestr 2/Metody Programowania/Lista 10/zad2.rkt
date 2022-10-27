#lang plait

(module+ test
  (print-only-errors #t))

;; abstract syntax -------------------------------

(define-type Op
  (add)
  (sub)
  (mul)
  (div)
  (eql)
  (leq))

(define-type UOp
  (neg)
  (rev))

(define-type Exp
  (numE [n : Number])
  (opE [op : Op]
       [l : Exp]
       [r : Exp])
  (uopE [op : UOp]
        [e : Exp])
  (ifE [b : Exp]
       [l : Exp]
       [r : Exp])
  (andE [cs : (Listof Exp)])
  (orE [cs : (Listof Exp)])
  (condE [cs : (Listof (Exp * Exp))]))

;; parse ----------------------------------------

(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s)
     (numE (s-exp->number s))]
    [(s-exp-match? `{if ANY ANY ANY} s)
     (ifE (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s)))
          (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `{cond ANY ...} s)
     (condE (parse-cond (rest (s-exp->list s))))]
    [(s-exp-match? `{and ANY ...} s)
     (andE (parse-and-or (rest (s-exp->list s))))]
    [(s-exp-match? `{or ANY ...} s)
     (orE (parse-and-or (rest (s-exp->list s))))]
    [(s-exp-match? `{SYMBOL ANY ANY} s)
     (opE (parse-op (s-exp->symbol (first (s-exp->list s))))
          (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s))))]
    [(s-exp-match? `{SYMBOL ANY} s)
     (uopE (parse-uop (s-exp->symbol (first (s-exp->list s))))
           (parse (second (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(define (parse-and-or [ss : (Listof S-Exp)]) : (Listof Exp)
  (type-case (Listof S-Exp) ss
    [empty
     empty]
    [(cons s ss)
     (cons (parse s) (parse-and-or ss))])) 

(define (parse-cond [ss : (Listof S-Exp)]) : (Listof (Exp * Exp))
  (type-case (Listof S-Exp) ss
    [empty
     empty]
    [(cons s ss)
     (if (s-exp-match? `{ANY ANY} s)
         (cons (pair (parse (first (s-exp->list s)))
                     (parse (second (s-exp->list s))))
               (parse-cond ss))
         (error 'parse "invalid input: cond"))]))

(define (parse-op [op : Symbol]) : Op
  (cond
    [(eq? op '+) (add)]
    [(eq? op '-) (sub)]
    [(eq? op '*) (mul)]
    [(eq? op '/) (div)]
    [(eq? op '=) (eql)]
    [(eq? op '<=) (leq)]
    [else (error 'parse "unknown operator")]))

(define (parse-uop [op : Symbol]) : UOp
  (cond
    [(eq? op '~) (neg)]
    [(eq? op '-) (rev)]
    [else (error 'parse "unknown unary operator")]))
                
(module+ test
  (test (parse `2)
        (numE 2))
  (test (parse `{+ 2 1})
        (opE (add) (numE 2) (numE 1)))
  (test (parse `{* 3 4})
        (opE (mul) (numE 3) (numE 4)))
  (test (parse `{+ {* 3 4} 8})
        (opE (add)
             (opE (mul) (numE 3) (numE 4))
             (numE 8)))
  (test (parse `{if {= 0 1} {* 3 4} 8})
        (ifE (opE (eql) (numE 0) (numE 1))
             (opE (mul) (numE 3) (numE 4))
             (numE 8)))
   (test/exn (parse `{{+ 1 2}})
            "invalid input")
  (test/exn (parse `{+ 1})
            "unknown unary operator")
  (test/exn (parse `{^ 1 2})
            "unknown operator")
  (test (parse `{cond {{= 0 1} {* 3 4}}
                      {{= 1 1} 8}})
        (condE (list (pair (opE (eql) (numE 0) (numE 1))
                           (opE (mul) (numE 3) (numE 4)))
                     (pair (opE (eql) (numE 1) (numE 1))
                           (numE 8)))))
  (test (parse `{and {= 1 2} {or {<= 3 4} {= 2 1}}})
        (andE (list (opE (eql) (numE 1) (numE 2))
                    (orE (list (opE (leq) (numE 3) (numE 4))
                               (opE (eql) (numE 2) (numE 1))))))))
  
;; eval --------------------------------------

(define-type Value
  (numV [n : Number])
  (boolV [b : Boolean]))

(define (op-num-num->proc [f : (Number Number -> Number)]) : (Value Value -> Value)
  (λ (v1 v2)
    (type-case Value v1
      [(numV n1)
       (type-case Value v2
         [(numV n2)
          (numV (f n1 n2))]
         [else
          (error 'eval "type error")])]
      [else
       (error 'eval "type error")])))

(define (uop-num->proc [f : (Number -> Number)]) : (Value -> Value)
  (lambda (v)
    (type-case Value v
      [(numV n)
       (numV (f n))]
      [else
       (error 'eval "type error")])))

(define (uop-bool->proc [f : (Boolean -> Boolean)]) : (Value -> Value)
  (lambda (v)
    (type-case Value v
      [(boolV n)
       (boolV (f n))]
      [else
       (error 'eval "type error")])))

(define (op-num-bool->proc [f : (Number Number -> Boolean)]) : (Value Value -> Value)
  (λ (v1 v2)
    (type-case Value v1
      [(numV n1)
       (type-case Value v2
         [(numV n2)
          (boolV (f n1 n2))]
         [else
          (error 'eval "type error")])]
      [else
       (error 'eval "type error")])))

(define (op->proc [op : Op]) : (Value Value -> Value)
  (type-case Op op
    [(add) (op-num-num->proc +)]
    [(sub) (op-num-num->proc -)]
    [(mul) (op-num-num->proc *)]
    [(div) (op-num-num->proc /)]
    [(eql) (op-num-bool->proc =)]
    [(leq) (op-num-bool->proc <=)]))

(define (uop->proc [op : UOp]) : (Value -> Value)
  (type-case UOp op
    [(neg) (uop-bool->proc not)]
    [(rev) (uop-num->proc (lambda (v) (* -1 v)))]))

(define (eval [e : Exp]) : Value
  (type-case Exp e
    [(numE n) (numV n)]
    [(opE o l r) ((op->proc o) (eval l) (eval r))]
    [(uopE o e) ((uop->proc o) (eval e))]
    [(ifE b l r)
     (type-case Value (eval b)
       [(boolV v)
        (if v (eval l) (eval r))]
       [else
        (error 'eval "type error")])]
    [(condE cs)
     (eval (cond->if cs))]
    [(andE cs)
     (eval-and cs)]
    [(orE cs)
     (eval-or cs)]))

(define (eval-or [cs : (Listof Exp)]) : Value
  (type-case (Listof Exp) cs
    [empty
     (boolV #f)]
    [(cons c cs)
     (type-case Value (eval c)
       [(boolV v)
        (if v (boolV #t) (eval-or cs))]
       [else
        (error 'eval-or "type error")])]))

(define (eval-and [cs : (Listof Exp)]) : Value
  (type-case (Listof Exp) cs
    [empty
     (boolV #t)]
    [(cons c cs)
     (type-case Value (eval c)
       [(boolV v)
        (if v (eval-and cs) (boolV #f))]
       [else
        (error 'eval-and "type error")])]))
       
(define (cond->if [cs : (Listof (Exp * Exp))]) : Exp
  (type-case (Listof (Exp * Exp)) cs
    [empty
     (numE 42)]
    [(cons c cs)
     (ifE (fst c)
          (snd c )
          (cond->if cs))]))

(define (run [e : S-Exp]) : Value
  (eval (parse e)))

(module+ test
  (test (run `2)
        (numV 2))
  (test (run `{+ 2 1})
        (numV 3))
  (test (run `{* 2 1})
        (numV 2))
  (test (run `{+ {* 2 3} {+ 5 8}})
        (numV 19))
  (test (run `{= 0 1})
        (boolV #f))
  (test (run `{if {= 0 1} {* 3 4} 8})
        (numV 8))
  (test (run `{cond {{= 0 1} {* 3 4}}
                    {{= 1 1} 8}})
        (numV 8)))

;; printer ———————————————————————————————————-

(define (value->string [v : Value]) : String
  (type-case Value v
    [(numV n) (to-string n)]
    [(boolV b) (if b "true" "false")]))

(define (print-value [v : Value]) : Void
  (display (value->string v)))

(define (main [e : S-Exp]) : Void
  (print-value (eval (parse e))))