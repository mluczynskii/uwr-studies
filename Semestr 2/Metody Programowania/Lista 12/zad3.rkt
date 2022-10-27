#lang plait

(module+ test
  (print-only-errors #t))

;; abstract syntax -------------------------------

(define-type Exp
  (numE [n : Number])
  (ifE [b : Exp] [l : Exp] [r : Exp])
  (varE [x : Symbol])
  (letE [x : Symbol] [e1 : Exp] [e2 : Exp])
  (lamE [xs : (Listof Symbol)] [e : Exp])
  (appE [e1 : Exp] [e2 : (Listof Exp)]))

;; parse ----------------------------------------

(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s)
     (numE (s-exp->number s))]
    [(s-exp-match? `{lambda {SYMBOL ...} ANY} s)
     (lamE (parse-lambda (s-exp->list (second (s-exp->list s))))
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
    [(s-exp-match? `{SYMBOL ANY ANY} s)
     (appE (appE (varE (parse-op (s-exp->symbol (first (s-exp->list s)))))
                 (list (parse (second (s-exp->list s)))))
           (list (parse (third (s-exp->list s)))))]
    [(s-exp-match? `{ANY ANY ...} s)
     (appE (parse (first (s-exp->list s)))
           (parse-list (rest (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(define (parse-list [xs : (Listof S-Exp)]) : (Listof Exp)
  (type-case (Listof S-Exp) xs
    [empty
     empty]
    [(cons s xs)
     (cons (parse s) (parse-list xs))]))

(define (parse-lambda [xs : (Listof S-Exp)]) : (Listof Symbol)
  (type-case (Listof S-Exp) xs
    [empty
     empty]
    [(cons x xs)
     (if (s-exp-match? `SYMBOL x)
         (cons (s-exp->symbol x) (parse-lambda xs))
         (error 'parse "invalid input"))]))

(define prim-ops '(+ - * / = <=))

(define (parse-op [op : Symbol]) : Symbol
  (if (member op prim-ops)
      op 
      (error 'parse "unknown operator")))

;; eval --------------------------------------

;; values

(define-type Value
  (numV [n : Number])
  (boolV [b : Boolean])
  (funV [xs : (Listof Symbol)] [e : Exp] [env : Env])
  (primopV [f : (Value -> Value)]))

(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

;; environments

(define-type-alias Env (Listof Binding))

(define mt-env empty)

(define (extend-env [env : Env] [xs : (Listof Symbol)] [vs : (Listof Value)]) : Env
  (type-case (Listof Symbol) xs
    [empty
     (type-case (Listof Value) vs
       [empty
        env]
       [(cons v vs)
        (error 'extend-env "arity mismatch")])]
    [(cons x xs)
     (type-case (Listof Value) vs
       [empty
        (error 'extend-env "arity mismatch")]
       [(cons v vs)
        (cons (bind x v) (extend-env env xs vs))])]))

(define (lookup-env [n : Symbol] [env : Env]) : Value
  (type-case (Listof Binding) env
    [empty (error 'lookup "unbound variable")]
    [(cons b rst-env) (cond
                        [(eq? n (bind-name b))
                         (bind-val b)]
                        [else (lookup-env n rst-env)])]))

;; primitive operations and the initial environment

(define (op-num-num->value [f : (Number Number -> Number)]) : Value 
  (primopV
   (λ (v1)
     (type-case Value v1
       [(numV n1)
        (primopV
         (λ (v2)
           (type-case Value v2
             [(numV n2)
              (numV (f n1 n2))]
             [else
              (error 'eval "type error")])))]
       [else
        (error 'eval "type error")]))))

(define (op-num-bool->value [f : (Number Number -> Boolean)]) : Value 
  (primopV
   (λ (v1)
     (type-case Value v1
       [(numV n1)
        (primopV
         (λ (v2)
           (type-case Value v2
             [(numV n2)
              (boolV (f n1 n2))]
             [else
              (error 'eval "type error")])))]
       [else
        (error 'eval "type error")]))))

(define init-env 
  (foldr (λ (b env) (extend-env env (list (fst b)) (list (snd b))))
         mt-env 
         (list (pair '+ (op-num-num->value +))
               (pair '- (op-num-num->value -))
               (pair '* (op-num-num->value *))
               (pair '/ (op-num-num->value /))
               (pair '= (op-num-bool->value =))
               (pair '<= (op-num-bool->value <=)))))

;; evaluation function (eval/apply)

(define (eval [e : Exp] [env : Env]) : Value
  (type-case Exp e
    [(numE n) (numV n)]
    [(ifE b l r)
     (type-case Value (eval b env)
       [(boolV v)
        (if v (eval l env) (eval r env))]
       [else
        (error 'eval "type error")])]
    [(varE x)
     (lookup-env x env)]
    [(letE x e1 e2)
     (let ([v1 (eval e1 env)])
       (eval e2 (extend-env env (list x) (list v1))))]
    [(lamE xs b)
     (funV xs b env)]
    [(appE e1 e2)
     (apply (eval e1 env) (eval-list e2 env))]))

(define (eval-list [xs : (Listof Exp)] [env : Env]) : (Listof Value)
  (type-case (Listof Exp) xs
    [empty
     empty]
    [(cons e xs)
     (cons (eval e env) (eval-list xs env))]))

(define (apply [v1 : Value] [v2 : (Listof Value)]) : Value
  (type-case Value v1
    [(funV xs b env)
     (eval b (extend-env env xs v2))]
    [(primopV f)
     (if (> (length v2) 1)
         (error 'apply "arity mismatch")
         (f (first v2)))]
    [else (error 'apply "not a function")]))

(define (run [e : S-Exp]) : Value
  (eval (parse e) init-env))

;; printer ———————————————————————————————————-

(define (value->string [v : Value]) : String
  (type-case Value v
    [(numV n) (to-string n)]
    [(boolV b) (if b "true" "false")]
    [(funV x e env) "#<procedure>"]
    [(primopV f) "#<primop>"]))

(define (print-value [v : Value]) : Void
  (display (value->string v)))

(define (main [e : S-Exp]) : Void
  (print-value (eval (parse e) init-env)))
