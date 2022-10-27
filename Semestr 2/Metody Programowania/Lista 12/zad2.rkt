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

(define-type Exp
  (numE [n : Number])
  (opE [op : Op] [l : Exp] [r : Exp])
  (ifE [b : Exp] [l : Exp] [r : Exp])
  (varE [x : Symbol])
  (letE [x : Symbol] [e1 : Exp] [e2 : Exp])
  (lamE [x : Symbol] [e : Exp])
  (appE [e1 : Exp] [e2 : Exp]))

;; parse ----------------------------------------

(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s)
     (numE (s-exp->number s))]
    [(s-exp-match? `{lambda {SYMBOL} ANY} s)
     (lamE (s-exp->symbol (first (s-exp->list (second (s-exp->list s)))))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{SYMBOL ANY ANY} s)
     (opE (parse-op (s-exp->symbol (first (s-exp->list s))))
          (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s))))]
    [(s-exp-match? `{if ANY ANY ANY} s)
     (ifE (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s)))
          (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `SYMBOL s)
     (varE (s-exp->symbol s))]
    [(s-exp-match? `{let SYMBOL ANY ANY} s)
     (letE (s-exp->symbol (second (s-exp->list s)))
           (parse (third (s-exp->list s)))
           (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `{ANY ANY} s)
     (appE (parse (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(define (parse-op [op : Symbol]) : Op
  (cond
    [(eq? op '+) (add)]
    [(eq? op '-) (sub)]
    [(eq? op '*) (mul)]
    [(eq? op '/) (div)]
    [(eq? op '=) (eql)]
    [(eq? op '<=) (leq)]
    [else (error 'parse "unknown operator")]))

;; eval --------------------------------------

;; values

(define-type Value
  (numV [n : Number])
  (boolV [b : Boolean])
  (funV [e : ExpA] [env : (EnvA Value)]))

;; primitive operations

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

;; lexical addressing —————————————————————————————————

;; target

(define-type ExpA
  (numA [n : Number])
  (opA [op : Op] [l : ExpA] [r : ExpA])
  (ifA [b : ExpA] [l : ExpA] [r : ExpA])
  (varA [n : Number])
  (letA [a1 : ExpA] [a2 : ExpA])
  (lamA [e : ExpA])
  (appA [e1 : ExpA] [e2 : ExpA]))

;; environments (lists of entities)

(define-type-alias (EnvA 'a) (Listof 'a))

(define mt-envA empty)

(define (extend-envA [env : (EnvA 'a)] [x : 'a]) : (EnvA 'a)
  (cons x env))

(define (lookup-envA [n : Number] [env : (EnvA 'a)]) : 'a
  (list-ref env n))

;; evaluation function

(define (evalA [a : ExpA] [env : (EnvA Value)]) : Value
  (type-case ExpA a
    [(numA n) (numV n)]
    [(opA o l r) ((op->proc o) (evalA l env) (evalA r env))]
    [(ifA b l r)
     (type-case Value (evalA b env)
       [(boolV v)
        (if v (evalA l env) (evalA r env))]
       [else
        (error 'eval "type error")])]
    [(varA n)
     (lookup-envA n env)]
    [(letA e1 e2)
     (let ([v1 (evalA e1 env)])
       (evalA e2 (extend-envA env v1)))]
    [(lamA e)
     (funV e env)]
    [(appA e1 e2)
     (apply (evalA e1 env) (evalA e2 env))]))

(define (apply [e1 : Value] [e2 : Value]) : Value
  (type-case Value e1
    [(funV e env)
     (evalA e (extend-envA env e2))]
    [else
     (error 'apply "Not a function")]))

(define (runA [s : S-Exp]) : Value
  (evalA (translate (parse s) mt-envA) mt-envA))

;; translation function

(define (address-of [x : Symbol] [env : (EnvA Symbol)]) : Number
  (type-case (EnvA Symbol) env
    [empty
     (error 'address-of "unbound variable")]
    [(cons y rst-env)
     (if (eq? x y)
         0
         (+ 1 (address-of x rst-env)))]))

(define (translate [e : Exp] [env : (EnvA Symbol)]) : ExpA
  (type-case Exp e
    [(numE n)
     (numA n)]
    [(opE o l r)
     (opA o (translate l env) (translate r env))]
    [(ifE b l r)
     (ifA (translate b env)
          (translate l env)
          (translate r env))]
    [(varE x)
     (varA (address-of x env))]
    [(letE x e1 e2)
     (letA (translate e1 env)
           (translate e2 (extend-envA env x)))]
    [(lamE x e)
     (lamA (translate e (extend-envA env x)))]
    [(appE e1 e2)
     (appA (translate e1 env) (translate e2 env))]))