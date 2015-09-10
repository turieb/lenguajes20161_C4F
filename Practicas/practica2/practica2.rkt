#lang plai

;; ejercicio 1
(define-type Array
  (MVacio)
  (MArray (n number?)
            (lista list?)))
;; test MArray
(test (Array? (MArray 1 '(0))) #t)
(test (Array? (MArray 1 '(0))) #f)
(test (Array? (MArray 1 '(0 2 3))) #t)
(test (Array? (MVacio)) #t)
(test (Array? (MVacio)) #f)

;; ejercicio 2

(define-type MList
  (MEmpty)
  (MCons(v any/c)
        (rest MList?)))

;; test MList
(test (MEmpty) (MEmpty))
(test (MCons 1 (MCons 2 (MCons 3 (MEmpty)))) (MCons 1 (MCons 2 (MCons 3 (MEmpty)))))
(test (MCons 7 (MCons 4 (MCons 10 (MEmpty))))  (MCons 7 (MCons 4 (MCons 10 (MEmpty)))))
(test (MEmpty) (MCons 2))
(test (MCons 1 (MEmpty)) (MCons 1 (MEmpty)))


;; ejercicio 3
(define-type ArbolN
  (TLEmpty)
  (NodeN (n number?)
         (l list?)))

;; test NTree
(test (TLEmpty) (TLEmpty))
(test (NodeN 1 (list (TLEmpty) (TLEmpty) (TLEmpty))) (NodeN 1 (list (TLEmpty) (TLEmpty) (TLEmpty))))
(test (NodeN 1 (list (NodeN 2 (list (TLEmpty)))(NodeN 3 (list (TLEmpty)))(NodeN 4 (list (TLEmpty) (TLEmpty) (TLEmpty))))) (NodeN 1 (list (NodeN 2 (list (TLEmpty)))(NodeN 3 (list (TLEmpty)))(NodeN 4 (list (TLEmpty) (TLEmpty) (TLEmpty))))))
(test (TLEmpty) 1)
(test (TLEmpty) (NodeN 1 (list (TLEmpty))))



;; ejercicio 4
(define-type Position
  (Pvacio)
  (2D-Point(x number?)
           (y number?)))

;; test position
(test (Pvacio) (Pvacio))
(test (Pvacio) 1)
(test  (2D-Point 0 0)  (2D-Point 0 0))
(test  (2D-Point 0 0)  (Pvacio))
(test  (2D-Point 1 (sqrt 9))  (2D-Point 1 3))


;; ejercicio 5
(define-type Figure
  (FVacio)
  (Circle(x 2D-Point?) (radio number?))
  (Square(x 2D-Point?) (longitud number?))
  (Rectangle(x 2D-Point?) (ancho number?) (largo number?)))

;; test figure
(test  (Circle (2D-Point 2 2) 2)  (Circle (2D-Point 2 2) 2))
(test  (Square (2D-Point 0 3) 3)  (Square (2D-Point 0 3) 3))
(test (Rectangle (2D-Point 0 2) 2 3) (Rectangle (2D-Point 0 2) 2 3))
(test (Rectangle (2D-Point 0 2) 2 3) (Square (2D-Point 0 2) 2))
(test  (Square (2D-Point 0 3) 3)  (Circle (2D-Point 0 3) 3))


;; SECCION 2

;; ejercicio 1
(define (setvalueA array pos val)
  (if (> pos (MArray-n array))
      (error "setvfalueA: Out of bounds")
      (cond        
        (< (length(cdr(MArray-lista array))) pos) (cons (car (MArray-lista array)) (setvalueA (cdr(MArray-lista array)) pos val))
        (= (length(cdr(MArray-lista array))) pos) (cons val (cdr (MArray-lista array)))
        (= pos (length(cdr(MArray-lista array)))empty))))

;; ejercicio 2
(define (MArray2MList arr)
  (if (empty? (MArray-lista arr))
      (MEmpty)
      (MCons (car (MArray-lista arr)) (MArray2MList (MArray (- (MArray-n arr) 1) (cdr (MArray-lista arr)))))))

;; test MArray2List
(test (MArray2MList (MArray 0 '())) (MEmpty))
(test (MArray2MList (MArray 5 '("a" "b"))) (MCons "a" (MCons "b" (MEmpty))))
(test  (MArray2MList (MArray 3 '(1 2 3))) (MCons 1 (MCons 2 (MCons 3 (MEmpty)))))
(test (MArray2MList (MArray 0 '())) (MCons "a" (MCons "b" (MEmpty))))
(test (MArray2MList (MArray 5 '("a" "b"))) (MEmpty))

;; ejercicio 3
(define (printML lista)
  (if (MEmpty? lista)
      "[]"
      ((string-append "[" (MCons-v lista)) (string-append "]" ",") (printML (MCons-rest lista)))))

;; ejercicio 4
(define (concatML listaA listaB)
  (cond
    [(MEmpty? listaA) listaB]
    [(MEmpty? listaB) listaA]
    [else (MCons (MCons-v listaA) (concatML (MCons-rest listaA) listaB))]))

;; test concatML
(test (concatML (MEmpty) (MEmpty)) (MEmpty))
(test (concatML (MCons 7 (MCons 4 (MEmpty))) (MCons 1 (MEmpty))) (MCons 7 (MCons 4 (MCons 1 (MEmpty)))))
(test (concatML (MCons 7 (MCons 4 (MEmpty))) (MCons 1 (MCons 10 (MEmpty)))) (MCons 7 (MCons 4 (MCons 1 (MCons 10 (MEmpty))))))
(test (concatML (MEmpty) (MEmpty)) (MCons 1 (MEmpty)))
(test (concatML (MEmpty) (MEmpty)) 1)

;; ejercicio 5
(define (lengthML lista)
  (if (MEmpty? lista)
      0
      (+ 1 (lengthML (MCons-rest lista)))))

;; test lengthML
(test  (lengthML (MEmpty)) 0)
(test  (lengthML (MEmpty)) 1)
(test (lengthML (MCons 7 (MCons 4 (MEmpty)))) 2)
(test (lengthML (MCons 7 (MCons 4 (MEmpty)))) 3)
(test (lengthML (MCons 7 (MCons 4 (MEmpty)))) 0)


    