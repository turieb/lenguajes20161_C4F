#lang plai

(require "practica3-base.rkt")
(require "practica3.rkt")

;;test zones

(test (zones 50 180) (list(resting 50 114.0)
                          (warm-up 115.0 127.0)
                          (fat-burning 128.0 140.0)
                          (aerobic 141.0 153.0)
                          (anaerobic 154.0 166.0)
                          (maximum 167.0 180)))

(test (zones 40 170) (list (resting 40 104.0) 
                           (warm-up 105.0 117.0) 
                           (fat-burning 118.0 130.0) 
                           (aerobic 131.0 143.0) 
                           (anaerobic 144.0 156.0) 
                           (maximum 157.0 170)))

(test (zones 30 140) (list (resting 30 84.0) 
                           (warm-up 85.0 95.0) 
                           (fat-burning 96.0 106.0) 
                           (aerobic 107.0 117.0)
                           (anaerobic 118.0 128.0) 
                           (maximum 129.0 140)))

(test (zones 60 200) (list (resting 60 129.0) 
                           (warm-up 130.0 143.0) 
                           (fat-burning 144.0 157.0)
                           (aerobic 158.0 171.0) 
                           (anaerobic 172.0 185.0)
                           (maximum 186.0 200)))

(test (zones 70 190) (list (resting 70 129.0) 
                                 (warm-up 130.0 141.0) 
                                 (fat-burning 142.0 153.0) 
                                 (aerobic 154.0 165.0)
                                 (anaerobic 166.0 177.0) 
                                 (maximum 178.0 190)))

;;test get-zone

(test (get-zone 'anaerobic my-zones)  (anaerobic 154.0 166.0))
(test (get-zone 'resting my-zones)  (resting 50 114.0))
(test (get-zone 'warm-up my-zones)  (warm-up 115.0 127.0))
(test (get-zone 'aerobic my-zones)  (aerobic 141.0 153.0))
(test (get-zone 'maximum my-zones)  (maximum 167.0 180))



;;test ninBT

(test (ninBT (EmptyBT)) 0)
(test (ninBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) 1)
(test (ninBT (BNode < (EmptyBT) 2 (EmptyBT))) 0)
(test (ninBT (BNode < (BNode < (BNode < (BNode < (EmptyBT) 2 (EmptyBT)) 2 (EmptyBT)) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (BNode < (EmptyBT) 2 (EmptyBT))))) 4)
(test (ninBT (BNode < (BNode < (BNode < (BNode < (EmptyBT) 2 (EmptyBT)) 2 (EmptyBT)) 3  (BNode < (BNode < (EmptyBT) 2 (EmptyBT)) 2 (EmptyBT))) 1 (BNode < (EmptyBT) 2 (BNode < (EmptyBT) 2 (EmptyBT))))) 5)

;;test nlBT 

(test (nlBT (EmptyBT)) 0)
(test (nlBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) 2)
(test (nlBT (BNode < (EmptyBT) 2 (EmptyBT))) 1)
(test (nlBT (BNode < (BNode < (BNode < (BNode < (EmptyBT) 2 (EmptyBT)) 2 (EmptyBT)) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (BNode < (EmptyBT) 2 (EmptyBT))))) 2)
(test (nlBT (BNode < (BNode < (BNode < (BNode < (EmptyBT) 2 (EmptyBT)) 2 (EmptyBT)) 3  (BNode < (BNode < (EmptyBT) 2 (EmptyBT)) 2 (EmptyBT))) 1 (BNode < (EmptyBT) 2 (BNode < (EmptyBT) 2 (EmptyBT))))) 3)

;;test nnBT

(test (nnBT (EmptyBT)) 0)
(test (nnBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) 3)
(test (nnBT (BNode < (EmptyBT) 2 (EmptyBT))) 1)
(test (nnBT (BNode < (BNode < (BNode < (BNode < (EmptyBT) 2 (EmptyBT)) 2 (EmptyBT)) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (BNode < (EmptyBT) 2 (EmptyBT))))) 6)
(test (nnBT (BNode < (BNode < (BNode < (BNode < (EmptyBT) 2 (EmptyBT)) 2 (EmptyBT)) 3  (BNode < (BNode < (EmptyBT) 2 (EmptyBT)) 2 (EmptyBT))) 1 (BNode < (EmptyBT) 2 (BNode < (EmptyBT) 2 (EmptyBT))))) 8)

;;test mapBT

(test (mapBT add1 (EmptyBT)) (EmptyBT))

(test (mapBT add1 (BNode < (EmptyBT) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) (BNode < (EmptyBT) 2 (BNode < (EmptyBT) 3 (EmptyBT))))

(test (mapBT (lambda (x) (* x x)) (BNode < (EmptyBT) 3 (BNode < (EmptyBT) 2 (EmptyBT)))) (BNode < (EmptyBT) 9 (BNode < (EmptyBT) 4 (EmptyBT))))

(test (mapBT (lambda (x) (- x 1)) (BNode < (EmptyBT) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) (BNode < (EmptyBT) 0 (BNode < (EmptyBT) 1 (EmptyBT))) )

(test (mapBT (lambda (x) (+ x 7)) (BNode < (EmptyBT) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) (BNode < (EmptyBT) 8 (BNode < (EmptyBT) 9 (EmptyBT))))

