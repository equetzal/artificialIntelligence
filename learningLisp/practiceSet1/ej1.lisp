#!/usr/bin/sbcl --script
; Construya una sola expresión LISP para calcular lo que se indica en cada uno de los siguientes incisos: 

	; El quinto elemento de la lista (((1 2) 3) 4 (5 (6)) A (B C) D (E (F G ))), sin usar la función F IF TH.

(defvar exampleList)
(defvar temp)
(setq exampleList '(((1 2) 3) 4 (5 (6)) A (B C) D (E (F G ))))
(print exampleList)
(print (list "Quinto elemento:" (first (second(second (rest exampleList))))))

	; El número de segundos que tiene el año bisiesto 2004.
(print (list "Segundos en 366 días:" (* 366 24 60 60)))

	; Si el valor numérico asociado a la variable x es diferente de cero y además menor o igual que el valor asociado a la variable y.
(defvar x) (defvar y)
(setq x 15) (setq y 10)
(print (and (/= x 0) (<= x y)))

	; Una lista con las dos soluciones reales de la ecuación 2x²+7x+5=0
(defvar a) (setq a 2.0)
(defvar b) (setq b 7.0)
(defvar c) (setq c 5.0)
(print (/ (+ (* -1 b) (* -1 (sqrt (+ (* b b) (* -1 4 a c)))) (* 4 a))))
(print (/ (+ (* -1 b) (sqrt (+ (* b b) (* -1 4 a c)))) (* 4 a)))