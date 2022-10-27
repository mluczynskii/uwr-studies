#lang plait

(define-type Prop
  (var [v : String])
  (conj [l : Prop] [r : Prop])
  (disj [l : Prop] [r : Prop])
  (neg [f : Prop]))

(define example-prop
  (conj
   (disj (var "x") (neg (var "y")))  
   (neg (var "x"))))

(define example-sigma
  (hash (list (pair "x" #f) (pair "y" #t))))

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

(eval example-sigma example-prop)