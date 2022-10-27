#lang plait

(define-type Prop
  (var [v : String])
  (conj [l : Prop] [r : Prop])
  (disj [l : Prop] [r : Prop])
  (neg [f : Prop]))

; obliczanie wartosci formuly dla danego wartosciowania
(define (eval [sigma : (Hashof String Boolean)]
              [phi : Prop])
  (cond [(var? phi)
         (let ([val (hash-ref sigma (var-v phi))])
           (if (none? val)
               (error 'beta_male "Podano zle wartosciowanie")
               (some-v val)))]
        [(conj? phi)
         (and (eval sigma (conj-l phi)) (eval sigma (conj-r phi)))]
        [(disj? phi)
         (or (eval sigma (disj-l phi)) (eval sigma (disj-r phi)))]
        [(neg? phi)
         (not (eval sigma (neg-f phi)))]))

; usuwanie duplikatow z listy
(define (remove-duplicates [xs : (Listof 'a)])
  (foldr
   (lambda (x ys) (cons x (filter (lambda (y) (not (equal? x y))) ys)))
   empty
   xs))

; znajdywanie listy zmiennych wystepujacych w formule
(define (free-vars [formula : Prop])
  (local
    [(define (it [f : Prop]
                 [acc : (Listof String)])
       (cond [(var? f)
              (cons (var-v f) acc)]
             [(neg? f)
              (it (neg-f f) acc)]
             [(conj? f)
              (append (it (conj-l f) acc) (it (conj-r f) acc))]
             [(disj? f)
              (append (it (disj-l f) acc) (it (disj-r f) acc))]))]
    (remove-duplicates (it formula empty))))

; generowanie listy wartosciowan do zpermutowania
(define (generate-sigma [n : Number]
                  [len : Number])
  (append (build-list n (lambda (x) #t)) (build-list (- len n) (lambda (x) #f))))

; usuwanie x z listy xs
(define (remove [x : 'a]
                [xs : (Listof 'a)])
  (cond [(empty? xs)
         xs]
        [(equal? x (first xs))
         (rest xs)]
        [else
         (cons (first xs) (remove x (rest xs)))]))

; generowanie permutacji listy
(define (pom [x : 'a]
             [xs : (Listof 'a)])
  (map (lambda (ys) (cons x ys)) (perms xs)))
                            
(define (perms [xs : (Listof 'a)])
  (local
    [(define (it [ys : (Listof 'a)]
                 [acc : (Listof (Listof 'a))]
                 [xs : (Listof 'a)])
       (if (empty? ys)
           acc
           (it (rest ys)
               (append (pom (first ys) (remove (first ys) xs)) acc)
               xs)))]
    (if (= (length xs) 1)
        (list xs)
        (remove-duplicates (it xs empty xs)))))

; generowanie wszystkich wartosciowan
(define (generate-sigma-perms [n : Number]) ; n - ilosc zmiennych w formule
  (local
    [(define (it [n : Number]
                 [acc : Number])
       (if (= acc -1)
           empty
           (append (perms (generate-sigma acc n)) (it n (- acc 1)))))]
    (it n n)))

; tworzenie sigmy dla listy zmiennych i listy wartosciowan
(define (create-hash [var-list : (Listof String)]
                      [b-list : (Listof Boolean)])
  (local
    [(define (it [xs : (Listof String)]
                 [ys : (Listof Boolean)]
                 [acc : (Listof (String * Boolean))])
       (if (or (empty? xs) (empty? ys))
           acc
           (it (rest xs) (rest ys) (cons (pair (first xs) (first ys)) acc))))]
    (hash (it var-list b-list empty))))

; sprawdzanie czy formula jest tautologia
(define (tautology? [phi : Prop])
  (local
    [(define var-list (free-vars phi))
     (define sigma-list (generate-sigma-perms (length var-list)))
     (define (it [xs : (Listof String)] ; lista zmiennych
                 [ys : (Listof (Listof Boolean))] ; lista wartosciowan
                 [acc : Boolean]) ; wynik    
       (if (empty? ys)
           acc
           (it xs
               (rest ys)
               (and acc (eval (create-hash xs (first ys)) phi)))))]
    (it var-list sigma-list #t)))
            
(define tautology (disj (var "x") (neg (var "x"))))
(define not-tautology (conj (var "x") (neg (var "x"))))