#lang plait
;; abstract syntax -------------------------------

(define-type Op
  (add) (sub) (mul) (div) (eql) (leq))

(define-type Exp
  (numE [n : Number])
  (opE [op : Op] [l : Exp] [r : Exp])
  (ifE [b : Exp] [l : Exp] [r : Exp])
  (varE [x : Symbol])
  (letE [xs : (Listof (Symbol * Exp))] [e2 : Exp])
  (let*E [xs : (Listof (Symbol * Exp))] [e2 : Exp]))

;; parse ----------------------------------------

(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s)
     (numE (s-exp->number s))]
    [(s-exp-match? `{let {ANY ANY ...} ANY} s)
     (letE (parse-let (s-exp->list (second (s-exp->list s))))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{let* {ANY ANY ...} ANY} s)
     (let*E (parse-let (s-exp->list (second (s-exp->list s))))
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
    [else (error 'parse "invalid input")]))

(define (parse-let [xs : (Listof S-Exp)]) : (Listof (Symbol * Exp))
  (type-case (Listof S-Exp) xs
    [empty
     empty]
    [(cons x xs)
     (cond [(s-exp-match? `{SYMBOL ANY} x)
            (cons (pair (s-exp->symbol (first (s-exp->list x)))
                        (parse (second (s-exp->list x))))
                  (parse-let xs))]
           [else
            (error 'parse-let "invalid input")])]))

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
  (boolV [b : Boolean]))

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

;; environments

(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

(define-type-alias Env (Listof Binding))

(define mt-env empty)

(define (extend-env [env : Env] [xs : (Listof (Symbol * Exp))] [env-start : Env]) : Env
  (type-case (Listof (Symbol * Exp)) xs
    [empty
     env]
    [(cons x xs)
     (extend-env (cons (bind (fst x) (eval (snd x) env-start)) env) xs env-start)]))

(define (extend-env* [env : Env] [xs : (Listof (Symbol * Exp))]) : Env
  (type-case (Listof (Symbol * Exp)) xs
    [empty
     env]
    [(cons x xs)
     (extend-env* (cons (bind (fst x) (eval (snd x) env)) env) xs)]))

(define (lookup-env [n : Symbol] [env : Env]) : Value
  (type-case (Listof Binding) env
    [empty (error 'lookup "unbound variable")]
    [(cons b rst-env) (cond
                        [(eq? n (bind-name b))
                         (bind-val b)]
                        [else (lookup-env n rst-env)])]))

;; evaluation function

(define (eval [e : Exp] [env : Env]) : Value
  (type-case Exp e
    [(numE n) (numV n)]
    [(opE o l r) ((op->proc o) (eval l env) (eval r env))]
    [(ifE b l r)
     (type-case Value (eval b env)
       [(boolV v)
        (if v (eval l env) (eval r env))]
       [else
        (error 'eval "type error")])]
    [(varE x)
     (lookup-env x env)]
    [(letE xs e)
     (eval e (extend-env env xs env))]
    [(let*E xs e)
     (eval e (extend-env* env xs))]))

(define (run [e : S-Exp]) : Value
  (eval (parse e) mt-env))

;; printer ———————————————————————————————————-

(define (value->string [v : Value]) : String
  (type-case Value v
    [(numV n) (to-string n)]
    [(boolV b) (if b "true" "false")]))

(define (print-value [v : Value]) : Void
  (display (value->string v)))

(define (main [e : S-Exp]) : Void
  (print-value (eval (parse e) mt-env)))
