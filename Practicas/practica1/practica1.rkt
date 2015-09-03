#lang plai

;; Practica 1.- lenguajes de programacion
;;     SECCION I


;; ejercicio 1
(define (pow base exp)
  (if (= exp 0)
      1
      (* base (pow base (- exp  1)))))

;; test pow
(test (pow 2 0) 1)
(test (pow 2 0) 0)
(test (pow 2 10) 1024)
(test (pow 2 10) 1025)
(test (pow 2 9) 512)


;; ejercicio 2
(define (average list)
  (/ (sumaLista list) (length list)))

(define (sumaLista list)
  (if (null? list)
      0
      (+ (car list) (sumaLista (cdr list)))))

;; test average
(test (average '(1)) 1)
(test (average '(1)) 0)
(test (average '(1 2 0)) 1)
(test (average '(1 2 0)) 2)



;; ejercicio 4
(define (zip l1 l2)
  (if (or (empty? l1) (empty? l2))'()
      (cons (list (car l1) (car l2)) (zip  (cdr l1) (cdr l2)))))

;; test zip
(test (zip '(1 2 3) '(a b))'((1 a) (2 b)))
(test (zip '(1 2 3) '(a b))'((1 a) (2 a)))
(test (zip '(9 2 3) '(a b))'((9 a) (2 b)))
(test (zip '(9 8 7) '(c d))'((9 c) (8 d)))
(test (zip '(9 8 7) '(c d))'((9 c) (8 c)))
 

;; ejercicio 5
(define (reduce funcion l)
  (if (= 2 (length l)) 
  (funcion (first l)(second l)) 
  (reduce funcion (cons (funcion (first l) (second l)) (cdr(cdr l))))))

;; test reduce
(test (reduce + '(1 2)) 3)
(test (reduce - '(1 1)) 0)
(test (reduce * '(1 2)) 2)
(test (reduce / '(2 2)) 1)
(test (reduce * '(2 2)) 2)


;;SECCION II

;; ejercicio 1
(define (mconcat l1 l2)
  (cond((empty? l1) l2)
       (else (cons (car l1) (mconcat (cdr l1) l2) ))))

;; test mconcat
(test (mconcat '(1 2) '(3 4)) '(1 2 3 4))
(test (mconcat '(1 2) '(3 4)) '(1 2 3))
(test (mconcat '(3 4) '(3 4)) '(3 4 3 4))
(test (mconcat '(1 2) '(3 4 5 6)) '(1 2 3 4 5 6))
(test (mconcat '(1 2 3 4) '(3 4 1 2)) '(1 2 3 4 3 4 1 2))

;; ejercicio 2
(define (mmap f l)
  (cond ((empty? l) l)
        (else (cons (f (car l))
                    (mmap f (cdr l))))))

;; test mmap
(test (mmap add1 '(1 2 3 4)) '(2 3 4 5))
(test (mmap add1 '(1 2 3 4)) '(2 3 4 5 6))
(test (mmap car '((1 2 3 4))) '(1))
(test (mmap cdr '((1 2 3 4))) '((2 3 4)))
(test (mmap cdr '((1 2 3 4))) '((2 3 4 5)))



;; ejercicio 3
(define mfilter
  (lambda (pred l)
      (cond ((empty? l) '())
            ((pred (car l)) (cons (car l) (mfilter pred (cdr l))))
            (else (mfilter pred (cdr l))))))

;; test mfilter
(test (mfilter (lambda (x) (not (zero? x))) '(2 0 1 4 0)) '(2 1 4))
(test (mfilter (lambda (l) (not (empty? l))) '((1 4 2) () (2 4) ())) '((1 4 2) (2 4)))
(test (mfilter (lambda (n) (= (modulo n 2) 0)) '(1 2 3 4 5 6)) '(2 4 6))
(test (mfilter (lambda (n) (= (modulo n 2) 0)) '(1 2 3 4 5 6)) '(2 4 3))
(test (mfilter (lambda (l) (not (empty? l))) '((1 4 2) () (2 4) ())) '((1 3 2) (2 4)))

;; ejercicio 4
(define (any? funcion l)
  (if (empty? l)
     #f
     (if (funcion (car l))
         #t
         (any? funcion (cdr l)))))

;; test any?
(test (any? number? '()) #f)
(test (any? number? '(a b c 1)) #t)
(test (any? symbol? '(a b c 1)) #t)
(test (any? number? '(a b c 1)) #f)
(test (any? number? '(a b c d)) #t)

;; ejercicio 5
(define (every? funcion l)
  (if (empty? l)
     #t
     (if (equal? (funcion (car l)) #f)
         #f
         (every? funcion (cdr l)))))

;; test every?
(test (every? number? '()) #t)
(test (every? number? '(a b c 1)) #f)
(test (every? symbol? '(a b c d)) #t)
(test (every? number? '(1 2 3 4)) #t)
(test (every? number? '(a b c d)) #f)
(test (every? number? '(a b c d)) #t)


;; ejercicio 6

;; mpowerset a ejecutar
(define (mpowerset lista)
  (sort (mpowerset-aux lista) (lambda (x y)  (lista-menor? x y))))



;; Funcion auxiliar que regresa el conjunto potencia de un conjunto en desorden
(define
  (mpowerset-aux aL)
  (cond
    [(empty? aL) (list empty)]
    [else
       (mpset2 (mpowerset-aux (rest aL))
                 (first aL))]))
(define
 (mpset2 r a)                 
  (cond
    [(empty? r)  empty]       
    [else
      (cons (cons a (first r))    
          (cons (first r)      
                  (mpset2       
                      (rest r)   
                      a )))]))


;; Funcion auxiliar que regresa true si dos listas estan ordenadas y regresa false en caso contrario.
(define (lista-menor? lst1 lst2)
  (define (aux l1 l2)
    (if (null? l1)
      #f
      (if (> (car l2) (car l1))
        #t
        (or (< (car l1) (car l2)) (aux (cdr l1) (cdr l2))))))
  (let ((len1 (length lst1)) (len2 (length lst2)))
    (if (> len1 len2)
      #f
      (or (< len1 len2) (aux lst1 lst2)))))

;; test mpowerset
(test (mpowerset '()) '(()))
(test (mpowerset '(1)) '(() (1)))
(test (mpowerset '(1 2)) '(() (1) (2) (1 2)))
(test (mpowerset '(1 2 3)) '(() (1) (2) (3) (1 2) (1 3) (2 3) (1 2 3)))
(test (mpowerset '()) '((1)))



