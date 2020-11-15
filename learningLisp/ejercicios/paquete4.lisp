#!/usr/bin/sbcl --script

; Paquete de ejercicios 4
; Alumna: Enya Quetzalli Gómez Rodríguez (Eduardo Gómez Rodríguez)
; Profesor: Dr. Salvador Godoy
; Escuela: Centro de Investigación de Cómputo - IPN
; Semestre: 2021/1

(defvar input)
(defvar output)


(print "Inciso 1")
; [Collect, argumentos: un predicado y una lista. Recursiva] Devuelve una lista en la cual se encuentran todos los elementos del argumento original para los cuales se cumple el predicado del primer argumento.
	(setq input '(2 4 A B 2 (9 3) 6 C 9 10))
	(defun collect (predicado lista) 
		(if (null lista)
			'()
			(if (funcall predicado (first lista))
				(cons (first lista) (collect predicado (rest lista)) )
				(collect predicado (rest lista))
			)
		)
	)
	(print input)
	(print (collect #'numberp input))(fresh-line)
	

(print "Inciso 2")
; [Palíndromo, argumento: una lista, Recursiva. Predicado] Si la lista recibida es un palíndromo, regresa T; de lo contrario regresa NIL.
	(setq input '(a n i t a l a v a l a t i n a))
	(defun palindromo (lista) 
		(if (null lista) T
			(let (
				(fi (first lista))
				(la (first (last lista)))
				)
				(if (equal fi la)
					(palindromo (rest (butlast lista)))
					NIL
				)
			)
		)
	)
	(print input)
	(print (palindromo input))(fresh-line)


(print "Inciso 3")
; [2Palindrome, argumento: una cadena, Recursiva, No destructiva] Entrega como respuesta una cadena como la original, pero convertida en palíndromo (duplicándola en orden inverso al final de sí misma).
	(defun rev (txt n) 
		(if (= n 0)
			(list (char txt n))
			(concatenate 'string 
				(list (char txt n))
				(rev txt (- n 1)) 
			)
		)
	)
	(defun palindromo (cadena)
		(concatenate 'string cadena (rev cadena (- (length cadena) 1)) )
	)
	(print (palindromo "Hola"))(fresh-line)