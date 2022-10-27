#lang racket

(require rackunit)
(require racket/match)

(provide (struct-out column-info)
         (struct-out table)
         (struct-out and-f)
         (struct-out or-f)
         (struct-out not-f)
         (struct-out eq-f)
         (struct-out eq2-f)
         (struct-out lt-f)
         table-insert
         table-project
         table-sort
         table-select
         table-rename
         table-cross-join
         table-natural-join)

(define-struct table (schema rows) #:transparent)
(define-struct column-info (name type) #:transparent)

(define cities
   (table
    (list (column-info 'city 'string)
          (column-info 'country 'string)
          (column-info 'area 'number)
          (column-info 'capital 'boolean))
    (list (list "Wroclaw" "Poland" 293 #f)
          (list "Warsaw" "Poland" 517 #t)
          (list "Poznan" "Poland" 262 #f)
          (list "Berlin" "Germany" 892 #t)
          (list "Munich" "Germany" 310 #f)
          (list "Paris" "France" 105 #t)
          (list "Rennes" "France" 50 #f))))

(define countries
  (table
   (list (column-info 'country 'string)
         (column-info 'population 'number)) ; schemat   
   (list (list "Poland" 38)
         (list "Germany" 83)
         (list "France" 67)
         (list "Spain" 47)))) ; rows

(define (empty-table columns) (table columns '()))

; użyteczne metody
(define (reverse xs)
  (foldl cons null xs))

(define (find-position col-name col-list)
  (define (it col-list n)
    (cond [(null? col-list)
           (error "Nie ma kolumny o takiej nazwie")]
          [(equal? (column-info-name (car col-list)) col-name)
           n]
          [else
           (it (cdr col-list) (+ n 1))]))
  (it col-list 0))

(define (check-data new-row col-list)
  (cond [(null? col-list)
         #t]
        [(match (column-info-type (car col-list))
           ['string (string? (car new-row))]
           ['number (number? (car new-row))]
           ['boolean (boolean? (car new-row))]
           ['symbol (symbol? (car new-row))])
         (check-data (cdr new-row) (cdr col-list))]
        [else
         #f]))

(define (determine-type-comparator col-name col-list)
      (cond [(null? col-list)
             (error "Nie ma kolumny o takiej nazwie")]
            [(equal? (column-info-name (car col-list)) col-name)
             (cond [(equal? (column-info-type (car col-list)) 'string)
                    string<?]
                   [(equal? (column-info-type (car col-list)) 'number)
                    <] 
                   [(equal? (column-info-type (car col-list)) 'boolean)
                    (lambda (x y) (if (equal? y #t)
                                      (if (equal? x #f)
                                          #t
                                          #f)
                                      #f))]
                   [(equal? (column-info-type (car col-list)) 'symbol)
                    (lambda (x y) (string<? (symbol->string x) (symbol->string y)))])]
            [else
             (determine-type-comparator col-name (cdr col-list))]))

(define (get-col-names col-list)
  (define (it xs acc)
    (if (null? xs)
        acc
        (it (cdr xs) (cons (column-info-name (car xs)) acc))))
  (it col-list null))

(define (quotient xs ys)
  (define (it xs acc)
    (cond [(null? xs)
           acc]
          [(member (car xs) ys)
           (it (cdr xs) (cons (car xs) acc))]
          [else
           (it (cdr xs) acc)]))
  (it xs null))

(define (union xs ys)
  (define (it xs acc)
    (cond [(null? xs)
           acc]
          [(member (car xs) ys)
           (it (cdr xs) acc)]
          [else
           (it (cdr xs) (cons (car xs) acc))]))
  (it xs ys))

(define (check-types test-row position val)
  (define field (list-ref test-row position))
  (cond [(string? field)
         (string? val)]
        [(number? field)
         (number? val)]
        [(symbol? field)
         (symbol? val)]
        [(boolean? field)
         (boolean? val)]))

; table-insert
(define (table-insert row tab)
  (cond [(not (= (length row) (length (table-schema tab))))
         (error "Dlugosc danych sie nie zgadza")]
        [(not (check-data row (table-schema tab)))
         (error "Typy danych sie nie zgadzaja")]
        [else
         (table (table-schema tab) (cons row (table-rows tab)))]))

; table-project
(define (table-project cols tab)
  (define (it columns rows columns-acc rows-acc)
    (cond [(null? columns)
           (table (reverse columns-acc) (map reverse rows-acc))]
          [(member (column-info-name (car columns)) cols)
           (it (cdr columns)
               (map cdr rows)
               (cons (car columns) columns-acc)
               (map (lambda (x y) (cons (car x) y)) rows rows-acc))]
          [else
           (it (cdr columns)
               (map cdr rows)
               columns-acc
               rows-acc)]))
  (it (table-schema tab)
      (table-rows tab)
      null
      (map (lambda (x) null) (table-rows tab))))
  
          
; table-rename
(define (table-rename col ncol tab)
  (define new-schema
    (map (lambda (column) (if (equal? (column-info-name column) col)
                              (column-info ncol (column-info-type column))
                              column))
         (table-schema tab)))
  (table new-schema (table-rows tab)))

; table-sort           
(define (table-sort cols tab)
  (define col-list (table-schema tab))
  (define (it cols rows)
    (if (null? cols)
        (table (table-schema tab) rows)
        (it (cdr cols)
            (sort rows
                  #:key (lambda (xs) (list-ref xs (find-position (car cols) col-list)))
                  (determine-type-comparator (car cols) col-list)))))
  (it (reverse cols) (table-rows tab)))
  
; table-select
(define-struct and-f (l r )) ; koniunkcja formuł
(define-struct or-f (l r)) ; dysjunkcja formuł
(define-struct not-f (e)) ; negacja formuły
(define-struct eq-f (name val)) ; wartość kolumny name równa val
(define-struct eq2-f  (name name2)) ; wartości kolumn name i name2 równe
(define-struct lt-f (name val)) ; wartość kolumny name mniejsza niż val
         
(define (table-select form tab)
  (define col-list (table-schema tab))
  (define rows (table-rows tab))
  (define (generate-filtering-function form)
    (cond [(eq-f? form)
           (define col-name (eq-f-name form))
           (define val (eq-f-val form))
           (lambda (xs) (equal? (list-ref xs (find-position col-name col-list)) val))] 
          [(eq2-f? form)
           (define name-1 (eq2-f-name form))
           (define name-2 (eq2-f-name2 form))
           (lambda (xs) (equal? (list-ref xs (find-position name-1 col-list))
                                (list-ref xs (find-position name-2 col-list))))]
          [(lt-f? form)
           (define col-name (lt-f-name form))
           (define val (lt-f-val form))
           (define compare (determine-type-comparator col-name col-list))
           (define position (find-position col-name col-list))
           (if (check-types (car rows) position val)
               (lambda (xs) (compare (list-ref xs position)
                                     val))
               (error "Porownywane wartosci maja rozne typy"))]
          [(and-f? form)
           (define left (and-f-l form))
           (define right (and-f-r form))
           (lambda (xs) (and
                         ((generate-filtering-function left) xs)
                         ((generate-filtering-function right) xs)))]
          [(or-f? form)
           (define left (or-f-l form))
           (define right (or-f-r form))
           (lambda (xs) (or
                         ((generate-filtering-function left) xs)
                         ((generate-filtering-function right) xs)))]
          [(not-f? form)
           (define expr (not-f-e form))
           (lambda (xs) (not ((generate-filtering-function expr) xs)))]
          [else
           (error "Nieznana formula")]))
  (table col-list (filter (generate-filtering-function form) rows)))

; table-cross-join
(define (table-cross-join tab1 tab2)
  (define rows-1 (table-rows tab1))
  (define rows-2 (table-rows tab2))
  (define (it rows-1 new-rows)
    (if (null? rows-1)
        new-rows
        (it (cdr rows-1) (append new-rows (map (lambda (xs) (append (car rows-1) xs)) rows-2)))))
  (table (append (table-schema tab1) (table-schema tab2)) (it rows-1 null)))

; table-natural-join
(define (table-natural-join tab1 tab2)
  (define col-names-1 (get-col-names (table-schema tab1)))
  (define col-names-2 (get-col-names (table-schema tab2)))
  
  (define same-names (quotient col-names-1 col-names-2))

  (define (generate-new-names xs acc)
    (if (null? xs)
        acc
        (generate-new-names (cdr xs) (cons (string->uninterned-symbol (symbol->string (car xs))) acc))))
  (define new-names (generate-new-names (reverse same-names) null))
  
  (define (rename xs ys acc)
    (if (null? xs)
        acc
        (rename (cdr xs) (cdr ys) (table-rename (car xs) (car ys) acc))))
  
  (define step1 (rename same-names new-names tab2))
  (define step2 (table-cross-join tab1 step1))

  (define (generate-formula xs ys)
    (if (null? (cdr xs))
        (eq2-f (car xs) (car ys))
        (and-f (eq2-f (car xs) (car ys)) (generate-formula (cdr xs) (cdr ys)))))

  (if (null? same-names)
      step2
      (table-project (union col-names-1 col-names-2)
                     (table-select (generate-formula same-names new-names) step2))))
  


      
      
      
         

  
    
    


  

  
  
         







         
         
  