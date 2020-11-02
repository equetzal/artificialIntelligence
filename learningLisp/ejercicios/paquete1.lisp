#!/usr/bin/sbcl --script

; Paquete de ejercicios 1
; Alumna: Enya Quetzalli Gómez Rodríguez (Eduardo Gómez Rodríguez)
; Profesor: Dr. Salvador Godoy
; Escuela: Centro de Investigación de Cómputo - IPN
; Semestre: 2021/1

(defvar input)
(defvar output)


(print "Inciso 1")(fresh-line)
;-------- Inciso 1 --------
; Instrucciones: Construya una sola expresión LISP para calcular lo que se indica en cada uno de los siguientes incisos
;	a) El quinto elemento de la lista (((1 2) 3) 4 (5 (6)) A (B C) D (E (F G ))), sin usar la función F IF TH.
	(setq input '(((1 2) 3) 4 (5 (6)) A (B C) D (E (F G ))))
	(print (first (second(second (rest input)))))
	(fresh-line)
;	b) El número de segundos que tiene el año bisiesto 2004.
	(print (* 366 24 60 60))
	(fresh-line)
;	c) Si el valor numérico asociado a la variable x es diferente de cero y además menor o igual que el valor asociado a la variable y.
	(defvar x) (defvar y)
	(setq x 15) (setq y 10)
	(print (and (/= x 0) (<= x y)))
	(fresh-line)
;	d) Una lista con las dos soluciones reales de la ecuación 2x²+7x+5=0
	(defvar a) (setq a 2.0)
	(defvar b) (setq b 7.0)
	(defvar c) (setq c 5.0)
	(print (/ (+ (* -1 b) (* -1 (sqrt (+ (* b b) (* -1 4 a c)))) (* 4 a))))
	(print (/ (+ (* -1 b) (sqrt (+ (* b b) (* -1 4 a c)))) (* 4 a)))
	(fresh-line)


(print "Inciso 2")(fresh-line)
;-------- Inciso 2 --------
; Escriba, en notación prefija y evalúe las siguientes expresiones aritméticas:
;	a)
	(print (+ (* 2 4)(- 6 8))) (fresh-line)
;	b)
	(print (/ (+ 5 (+ -3 4)) (+ 6 (/ 2 5)))) (fresh-line)
;	c)
	(print (sqrt (/ (+ 1.4502 (* -1 (- -4 (/ 3 8)))) (expt -1 (expt (- 3 5) (/ 1 3)))))) (fresh-line)
;	d)
	(print (expt (/ (expt (/ 65.402 (sqrt -1)) (/ 5)) 0.17) (/ 7))) (fresh-line)


(print "Inciso 3")(fresh-line)
;-------- Inciso 3 --------
; Indique el resultado de evaluar cada una de las siguientes expresiones
;	a) (cdar '((one two) three four)))
	(print (cdar '((one two) three four))) (fresh-line)
;	b) (append (cons '(eva lisa) '(karl sven)) '(eva lisa) '(karl sven))
	(print (append (cons '(eva lisa) '(karl sven)) '(eva lisa) '(karl sven))) (fresh-line)
;	c) (subst 'gitan 'birgitta '(eva birgitta lisa birgitta karin))
	(print (subst 'gitan 'birgitta '(eva birgitta lisa birgitta karin))) (fresh-line)
;	d) (remove 'sven '(eva sven lisa sven anna))
	(print (remove 'sven '(eva sven lisa sven anna))) (fresh-line)
;	e) (butlast '(karl adam nilsson gregg alisson vilma) 3)
	(print (butlast '(karl adam nilsson gregg alisson vilma) 3)) (fresh-line)
;	f) (nth 2 '(a b c d e))
	(print (nth 2 '(a b c d e))) (fresh-line)
;	g) (nthcdr 2 '(a b c d e))
	(print (nthcdr 2 '(a b c d e))) (fresh-line)
;	h) (intersection '(a b c) '(x b z c))
	(print (intersection '(a b c) '(x b z c))) (fresh-line)
;	i) (cdadar '(((((1 2 3) z) y)(x 4)) 7 8 (a b c (5 (6 7 8)))))
	(print (cdadar '(((((1 2 3) z) y)(x 4)) 7 8 (a b c (5 (6 7 8)))))) (fresh-line)


(print "Inciso 4")(fresh-line)
;-------- Inciso 4 --------
; Defina una función Recombina que reciba como argumento una lista de la forma ((A . x) (B . y) (C . z)), donde A , B y C son átomos simbólicos, mientras que x, y y z son números. Como respuesta, la función debe entregar otra lista con la siguiente estructura: ( ((x y) . A ) ((y z) . C ) ((z y x) . B ) )
	(setq input '((A . 1) (B . 5) (C . 9)) )
	(defun recombina (lista) 
		(list 
			(cons (list (rest (first lista)) (rest (second lista))) 
				(first (first lista)) 
			)
			(cons (list (rest (second lista)) (rest (third lista))) 
				(first (third lista)) 
			)
			(cons (list (rest (third lista)) (rest (second lista)) (rest (first lista))) 
				(first (second lista)) 
			)
		)
	)
	(print (recombina input)) (fresh-line)


(print "Inciso 5")(fresh-line)
;-------- Inciso 5 --------
;  Defina un predicado RealNoCero? que reciba un argumento N y responda si su argumento es o no un número real diferente de cero.
	(defun realNoCero (n) (
			if (realp n) (/= n 0) NIL
		)
	)
	(print (realNoCero 0.0002))
	(fresh-line)
	

(print "Inciso 6")(fresh-line)
;-------- Inciso 6 --------
; Construya una función Analiza, con argumento X, que responda una lista con los valores de verdad correspondientes a las respuestas a las siguientes preguntas: ¿es X un átomo?, ¿es X un n úmero?, ¿es X una lista? , ¿es X una celda de construcción? y ¿es X una lista vacía?
	(defun analiza (X) ( list
			(atom x)
			(numberp x)
			(listp x)
			(consp x)
			(null x)
		)
	)
	(print (analiza (cons 'A 'B))) (fresh-line)


(print "Inciso 7")(fresh-line)
;-------- Inciso 7 --------
; Defina una función Intercala que reciba como argumentos dos listas cualesquiera y, como resultado entregue otra lista en la que se encuentran intercalados los elementos de las listas originales; siempre en el mismo orden: un elemento de la primera lista y otro de la segunda lista. Si las listas no tienen la misma longitud, todos los elementos restantes de la lista más grande se colocan seguidos en la respuesta
	(setq input '( (A B C D E F) (1 2 3 4 5 6 7 8) ))
	(defun intercala (l1 l2) 
		(let (
			(mixed '())
			) 
			(do (
				(i 0 (+ i 1))
				)((= i (min (list-length l1) (list-length l2))) mixed)
				(progn 
					(setq mixed (append mixed (list (nth i l1))))
					(setq mixed (append mixed (list (nth i l2))))
				)
			)
			(do (
				(i (min (list-length l1) (list-length l2)) (+ i 1))
				) ((= i (list-length l1)) mixed)
				(setq mixed (append mixed (list (nth i l1))))
			)
			(do (
				(i (min (list-length l1) (list-length l2)) (+ i 1))
				) ((= i (list-length l2)) mixed)
				(setq mixed (append mixed (list (nth i l2))))
			)
		)
	)
	(print (intercala (first input) (first (rest input)))) (fresh-line)


(print "Inciso 8")(fresh-line)
;-------- Inciso 8 --------
; Programe un predicado MismoTipo que reciba como argumento dos listas de la misma longitud y como respuesta, devuelva T si ambas listas tienen elementos del mismo tipo y en las mismas posiciones, NIL en caso contrario. Observe que los elementos no requieren ser iguales, sólo del mismo tipo de datos.
	(setq input '( (A B C (1 2) D E F) (A B C D E F G) ))
	(defun mismoTipo (l1 l2) 
		(let (
			(mixed '())
			) 
			(do (
				(i 0 (+ i 1))
				)((= i (list-length l1)) mixed)
				(progn 
					(setq mixed (append mixed (list 
						(if (or 
								(and (atom (nth i l1)) (atom (nth i l2))) 
								(and (listp (nth i l1)) (listp (nth i l2))) 
							) T NIL)
					)))
				)
			)
		)
	)
	(print (mismoTipo (first input) (first (rest input)))) (fresh-line)


(print "Inciso 9")(fresh-line)
;-------- Inciso 9 --------
; Defina una función APalíndromo, sensible a mayúsculas y minúsculas, que reciba como argumento una cadena y, como respuesta entrega otra cadena que es el palíndromo de la original. Ejemplo: APalíndromo("Hola") = "HolaaloH". 
	(setq input "Hola")
	(defun apalindromo (txt) 
		(let (
				(pal txt)
			)(do (
					(i (- (length txt) 1) (- i 1))
				) ((< i 0) pal)
				(setq pal (concatenate 'string pal (list (char txt i))))
			)
		)
	)
	(print (apalindromo input)) (fresh-line)


(print "Inciso 10")(fresh-line)
;-------- Inciso 10 --------
; Defina un predicado Bisiesto que reciba como entrada un número entero representando un año y, como respuesta, indique si se trata de un año bisiesto o no.
	(setq input 2020)
	(defun bisiesto (año) 
		(if (= 0 (mod año 4)) T NIL)
	)
	(print (bisiesto input)) (fresh-line)
