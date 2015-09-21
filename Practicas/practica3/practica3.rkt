#lang plai

(require "practica3-base.rkt")


;; ejercicio 1
(define zones
  (lambda (min max)
    (zone-n min max min max 4 empty) ))

;; funcion auxiliar
 (define range 
   (lambda (min max) 
     (- max min)))

;; funcion auxiliar
(define zone-n
  (lambda (resthr  maxhr resthr-local maxhr-local n rest)
    (if (= n 4)
    (let* ([local-range (range resthr maxhr-local)]
          [maximum-high maxhr-local]
          [maximum-low (+ (* local-range 0.9) resthr-local)])
   (zone-n resthr maxhr maximum-low maximum-high (- n 1) (maximum maximum-low maximum-high)) )
    (if (= n 3)
       (let* ([local-range (range resthr maxhr)]
          [anaerobic-high (- resthr-local 1)]
          [anaerobic-low (+ (* local-range 0.8) resthr)])
    (zone-n resthr maxhr anaerobic-low anaerobic-high (- n 1)(list (anaerobic anaerobic-low anaerobic-high) rest)))
        (if (= n 2)
       (let* ([local-range (range resthr maxhr)]
          [aerobic-high (- resthr-local 1)]
          [aerobic-low (+ (* local-range 0.7) resthr)])
   (zone-n resthr maxhr aerobic-low aerobic-high (- n 1) (list* (aerobic aerobic-low aerobic-high) rest)))
       (if (= n 1)
       (let* ([local-range (range resthr maxhr)]
          [fat-burning-high (- resthr-local 1)]
          [fat-burning-low (+ (* local-range 0.6) resthr)])
   (zone-n resthr maxhr fat-burning-low fat-burning-high (- n 1) (list* (fat-burning fat-burning-low fat-burning-high) rest))) 
       (if (= n 0)
       (let* ([local-range (range resthr maxhr)]
          [warm-up-high (- resthr-local 1)]
          [warm-up-low (+ (* local-range 0.5) resthr)])
   (zone-n resthr maxhr warm-up-low warm-up-high (- n 1) (list* (warm-up warm-up-low warm-up-high) rest))) 
      (let* ([local-range (range resthr maxhr)]
          [resting-high (- resthr-local 1)]
          [resting-low resthr])
    (cons (resting resting-low resting-high) rest)) )
       ))))))


;; ejercicio 2
(define get-zone 
 (lambda (zone my-zones)
   (case zone 
     ['resting (if (resting? (car my-zones)) (car my-zones) (get-zone zone (cdr my-zones)))]   
     ['warm-up (if (warm-up? (car my-zones)) (car my-zones) (get-zone zone (cdr my-zones)))]
     ['fat-burning (if (fat-burning? (car my-zones)) (car my-zones) (get-zone zone (cdr my-zones)))]
     ['aerobic (if (aerobic? (car my-zones)) (car my-zones) (get-zone zone (cdr my-zones)))]
     ['anaerobic (if (anaerobic? (car my-zones)) (car my-zones) (get-zone zone (cdr my-zones)))]
     ['maximum (if (maximum? (car my-zones)) (car my-zones) (get-zone zone (cdr my-zones)))]
     [else error "No exite esa zona"]
     )))


;; ejercicio 3
(define bpm->zone
  (lambda (lista my-zones)
    (if (empty? lista) 
        empty
    (cons (buscar-zone (car lista)) (bpm->zone (cdr lista) my-zones)))))
    
   


;; auxiliar,dado una frecuencia cardiaca regresa la zona en que se encuentra
(define buscar-zone 
  (lambda (n) 
    (if (and (>= n (resting-low (car my-zones)) ) (<= n (resting-high (car my-zones))))  
             (car my-zones)
             (if (and (>= n (warm-up-low (car (cdr my-zones))) ) (<= n (warm-up-high (car (cdr my-zones)))))  
             (car (cdr my-zones))
             (if (and (>= n (fat-burning-low (car (cdr (cdr my-zones)))) ) (<= n (fat-burning-high (car (cdr (cdr my-zones))))))  
             (car (cdr (cdr my-zones)))
             (if (and (>= n (aerobic-low (car (cdr (cdr (cdr my-zones))))) ) (<= n (aerobic-high (car (cdr (cdr (cdr my-zones)))))))  
             (car (cdr (cdr (cdr my-zones))))
             (if (and (>= n (anaerobic-low (car (cdr (cdr (cdr (cdr my-zones)))))) ) (<= n (anaerobic-high (car (cdr (cdr (cdr (cdr my-zones))))))))  
             (car (cdr (cdr (cdr (cdr my-zones)))))
             (if (and (>= n (maximum-low (car (cdr (cdr (cdr (cdr (cdr my-zones))))))) ) (<= n (maximum-high (car (cdr (cdr (cdr (cdr (cdr my-zones)))))))))  
             (car (cdr (cdr (cdr (cdr (cdr my-zones))))))
             (car my-zones))))))
    )))


;; auxiliar
(define create-trackpoints-aux 
  (lambda (row my-zones row-original index)
    (if  (empty? row)
        empty           
        (cons (trackpoint (GPS (car (car (cdr (car row)))) (car (cdr (car (cdr (car row)))))) 
                          (car (cdr (cdr (car row)))) 
                          (buscar-zone (car (cdr (cdr(list-ref row-original index)))))
                          (car (car row))) (create-trackpoints-aux  (cdr row) my-zones row-original (+ 1 index)))
        )    
    ))


;; ejercicio 4
(define create-trackpoints
  (lambda (row my-zones)
    (create-trackpoints-aux row my-zones row 0)))


;; auxiliar, obitnene la frecuencia cardiaca de un trackpoint
(define get-hrz
  (lambda (trackpoint)
    (let ([x (trackpoint-hr trackpoint)]) x)))


;; auxiliar
(define hacer-lista-hrz 
  (lambda (lista-trackpoint)
    (if (empty? lista-trackpoint)
        empty
          (cons (get-hrz (car lista-trackpoint)) (hacer-lista-hrz (cdr lista-trackpoint))))      
            ))

;; ejercicio 6
(define average-hr
  (lambda (lista)
    (truncate (/  (foldr + 0 (hacer-lista-hrz lista)) (length lista)))
    ))



;; auxiliar
(define (maximo-lista lst)
  (if (null? lst)
      #f
      (foldl (lambda (e r) (if (> e r) e r))
             (car lst)
             (cdr lst))))

;; ejercicio 7
(define max-hr 
  (lambda (lista)
    (maximo-lista (hacer-lista-hrz lista))))





;; SECCION 2

;; ejercicio 1
(define ninBT 
  (lambda (tree)
    (type-case BTree 
      tree 
      [EmptyBT () 0]
      [BNode (c l e r)      
      (if  (and (equal? (EmptyBT) l) (equal? (EmptyBT) r)) 
           0  
           (+ (ninBT l) (ninBT r) 1 ))
      ]
      )))

;; ejercicio 2
(define nlBT 
  (lambda (tree)
    (type-case BTree 
      tree 
      [EmptyBT () 0]
      [BNode (c l e r)      
      (if  (and (equal? (EmptyBT) l) (equal? (EmptyBT) r)) 
           1  
           (+ (nlBT l) (nlBT r)))
      ]
      )))

;; ejercicio 3
(define nnBT
  (lambda (tree)
    (type-case BTree 
      tree 
      [EmptyBT () 0]
      [BNode (c l e r)      
      (+ (nnBT l) (nnBT r) 1)
      ]
      )))

;; ejercicio 4
(define (mapBT  proc tree)
  (match tree
    [(EmptyBT)  tree]
    [(BNode c l e r)  (BNode c (mapBT proc l) (proc e) (mapBT proc r) )]))


;; ejercicio 5
(define (preorderBT tree)
  (if (equal? (EmptyBT) tree ) '() (cons (BNode-e tree) (append (preorderBT (BNode-l tree)) (preorderBT (BNode-r tree) )))
  ))


;; ejercicio 6
(define posorderBT
  (lambda (arbol)
    (type-case BTree arbol
      [EmptyBT () '()]
      [BNode (c l e r) (append (posorderBT l) (append (posorderBT r) (list e))) ]
      )))

;; ejercicio 7
(define inorderBT
  (lambda (arbol)
    (type-case BTree arbol
      [EmptyBT () '()]
      [BNode (c l e r) (append (inorderBT l) (cons e (inorderBT r))) ]
      )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Definiciones de funciones auxiliares ;;;;;;;;;;;;;;

(define my-zones (zones 50 180))

;;Definicion de las diferentes zonas 
(define mrest ( get-zone 'resting my-zones))
(define mfat ( get-zone 'fat-burning my-zones))
(define mwarm ( get-zone 'warm-up my-zones))
(define maero ( get-zone 'aerobic my-zones))
(define manae ( get-zone 'anaerobic my-zones))
(define mmax ( get-zone 'maximum my-zones))

;;
(define sample (create-trackpoints (take raw-data 100) my-zones))
(define trackpoints (create-trackpoints raw-data my-zones))


;;
