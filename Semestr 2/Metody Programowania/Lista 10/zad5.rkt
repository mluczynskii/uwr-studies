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
  (leq)
  (_cons))

(define-type Unary-op
  (_car)
  (_cdr)
  (_null?))
  
(define-type Exp
  (numE [n : Number])
  (opE [op : Op]
       [l : Exp]
       [r : Exp])
  (ifE [b : Exp]
       [l : Exp]
       [r : Exp])
  (condE [cs : (Listof (Exp * Exp))])
  (listE [cs : (Listof Exp)])
  (unary-opE [op : Unary-op]
             [e : Exp]))

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
    [(s-exp-match? `{list ANY ...} s)
     (listE (parse-list (rest (s-exp->list s))))]
    [(s-exp-match? `null s)
     (listE empty)]
    [(s-exp-match? `{SYMBOL ANY} s)
     (unary-opE (parse-unary-op (s-exp->symbol (first (s-exp->list s))))
                (parse (second (s-exp->list s))))]
    [(s-exp-match? `{SYMBOL ANY ANY} s)
     (opE (parse-op (s-exp->symbol (first (s-exp->list s))))
          (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(define (parse-unary-op [op : Symbol]) : Unary-op
  (cond [(eq? op 'car) (_car)]
        [(eq? op 'cdr) (_cdr)]
        [(eq? op 'null?) (_null?)]
        [else (error 'parse "unknown unary operator")]))
        
(define (parse-list [ss : (Listof S-Exp)]) : (Listof Exp)
  (type-case (Listof S-Exp) ss
    [empty
     empty]
    [(cons s ss)
     (cons (parse s) (parse-list ss))]))

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
    [(eq? op 'cons) (_cons)]
    [else (error 'parse "unknown operator")]))
                
;; eval --------------------------------------

(define-type Value
  (numV [n : Number])
  (boolV [b : Boolean])
  (listV [xs : (Listof Value)]))

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
    [(leq) (op-num-bool->proc <=)]
    [(_cons)
     (lambda (v1 v2)
       (type-case Value v2
         [(listV xs)
          (listV (cons v1 xs))]
         [else
          (listV (list v1 v2))]))]))
         
(define (unary-op->proc [op : Unary-op]) : (Value -> Value)
  (type-case Unary-op op
    [(_car)
     (lambda (v)
       (type-case Value v
         [(listV xs)
          (type-case (Listof Value) xs
            [empty
             (error 'car "expected non-empty list")]
            [else
             (first xs)])]
         [else
          (error 'eval "type error")]))]
    [(_cdr)
     (lambda (v)
       (type-case Value v
         [(listV xs)
          (type-case (Listof Value) xs
            [empty
             (error 'cdr "expected non-empty list")]
            [else
             (listV (rest xs))])]
         [else
          (error 'eval "type error")]))]
    [(_null?)
     (lambda (v)
       (type-case Value v
         [(listV xs)
          (boolV (empty? xs))]
         [else
          (error 'eval "type error")]))]))

(define (eval [e : Exp]) : Value
  (type-case Exp e
    [(numE n) (numV n)]
    [(opE o l r) ((op->proc o) (eval l) (eval r))]
    [(ifE b l r)
     (type-case Value (eval b)
       [(boolV v)
        (if v (eval l) (eval r))]
       [else
        (error 'eval "type error")])]
    [(condE cs)
     (eval (cond->if cs))]
    [(listE cs)
     (listV (eval-list cs))]
    [(unary-opE o e) ((unary-op->proc o) (eval e))]))

(define (eval-list [cs : (Listof Exp)]) : (Listof Value)
   (type-case (Listof Exp) cs
     [empty
      empty]
     [(cons c cs)
      (cons (eval c) (eval-list cs))]))

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

;; printer ———————————————————————————————————-

(define (value->string [v : Value]) : String
  (type-case Value v
    [(numV n) (to-string n)]
    [(boolV b) (if b "true" "false")]
    [(listV xs) (string-append "'(" (string-append (listV->string xs) ")"))]))

(define (listV->string [xs : (Listof Value)]) : String
  (type-case (Listof Value) xs
    [empty
     ""]
    [(cons x xs)
     (type-case (Listof Value) xs
       [empty
        (string-append (value->string x) (listV->string xs))]
       [(cons y ys)
        (string-append (value->string x) (string-append " " (listV->string xs)))])]))

(define (print-value [v : Value]) : Void
  (display (value->string v)))

(define (main [e : S-Exp]) : Void
  (print-value (eval (parse e))))

