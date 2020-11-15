#!/usr/bin/sbcl --script

; Paquete de ejercicios 3
; Alumna: Enya Quetzalli Gómez Rodríguez (Eduardo Gómez Rodríguez)
; Profesor: Dr. Salvador Godoy
; Escuela: Centro de Investigación de Cómputo - IPN
; Semestre: 2021/1

(defvar input)
(defvar output)

;
;	Inciso 1 -> Rehacer todos los ejerccicios del paquete 2, pero ahora recursivos. 
;

(print "Inciso 1")
;-------- Inciso 1 --------
; [sin usar ELT ni POSITION] Defina una función ElemInPos que reciba tres argumentos: elem, lista y pos. La función debe devolver T si elem está en la posición pos de lista, NIL si no lo está.
	(setq input '(A B C D E))
	(defun elemInPos (elemento lista posición) 
		(if (= posición 0) 
			(equal elemento (first lista))
			(elemInPos elemento (rest lista) (- posición 1))
		)
	)
	(print (elemInPos 'C input 3)) (fresh-line)


(print "Inciso 2")
;-------- Inciso 2 --------
; Escriba la función Inicio-en que recibe como argumentos una lista y un elemento cualquiera. La función debe entregar como respuesta una copia de la lista original pero comenzando con la primera ocurrencia del elemento dado en la lista original.
	(setq input '(A B C D E))
	(defun inicioEn (elemento lista) 
		(push elemento lista)
	)
	(print (inicioEn 'Z input)) (fresh-line)


(print "Inciso 3")
;-------- Inciso 3 --------
; Modifique la función del ejercicio anterior para que se llame Termina-en y entregue como respuesta una copia de la lista original pero que termina en la última ocurrencia del elemento dado 
	(setq input '(A B C D E))
	(defun terminaEn (elemento lista) 
		(setq lista (append lista (list elemento)))
	)
	(print (terminaEn 'Z input)) (fresh-line)


(print "Inciso 4")
; Construya una función Primer-impar que reciba como argumento una lista y como respuesta entregue otra lista conteniendo el primer elemento de la lista original que sea un número impar y la posición (índice) donde se encuentra. Observe que los elementos de la lista pueden ser de cualquier tipo de datos
	(setq input '(A () 2 8 D 3 P 4 7))
	(defun primerImpar (lista indice) 
		(if (equal NIL lista) 
			NIL
			(if (and (numberp (first lista)) (= 1 (mod (first lista) 2)))
				(list (first lista) indice)
				(primerImpar (rest lista) (+ indice 1))
			)
		)
	)
	(print (primerImpar input 0)) (fresh-line)


(print "Inciso 5")
; Modifique la función del inciso anterior para que entregue en la lista de respuesta el último elemento de la lista que sea un número real mayor o igual que cero y el número de veces que dicho elemento se repite en toda la lista.
	(setq input '(A () 2.0 8.2 D 7.1 P 4 7.1))
	(defun freqUltimoReal (lista) 
		(let (
			(retVal '())
			)
			(if (equal NIL lista)
				retVal
				(progn
					(setq retVal (freqUltimoReal (rest lista)))
					(if (equal NIL retVal) 
						(if (and (realp (first lista)) (>= (first lista) 0.0))
							(list (first lista) 1)
							retVal
						)
						(if (and (realp (first lista)) (>= (first lista) 0.0) (= (first lista) (first retVal)))
							(list (first retVal) (+ 1 (second retVal)))
							retVal
						)
					)
				)
			)
		)
	)
	(print (freqUltimoReal input)) (fresh-line)


(print "Inciso 6")
; Escriba la función Conteo que recibe como argumento una lista cualquiera y, como respuesta, entregue una celda de construcción cuya primera parte contiene el conteo de elementos numéricos de la lista original y cuya segunda parte contiene el conteo de sublistas contenidas en la lista original.
	(setq input '(2 (3 4) (5 4) 5 9.0 5))
	(defun conteo (lista num lis) 
		(if (equal lista NIL)
			(cons num lis)
			(if (numberp (first lista))
				(conteo (rest lista) (+ 1 num) lis)
				(if (listp (first lista))
					(conteo (rest lista) num (+ 1 lis))
					(conteo (rest lista) num lis)
				)
			)
		)
	)
	(print (conteo input 0 0)) (fresh-line)


(print "Inciso 7")
; Defina una función Aplana que reciba como argumento una lista con elementos anidados a cualquier nivel de profundidad y, como respuesta, entregue una lista conteniendo los mismos elementos pero todos ellos al nivel principal de profundidad.
	(setq input '(2 (3 4 (5 6 7)) (5 4) 5 9.0 5))
	(defun aplana (lista)
		(let (
			(nuevaLista '())
			(hayListaAnidada 0)
			)
			(loop 
				for i in lista
			do
				(if (listp i)
					(progn
						(loop 
							for j in i
						do
							(setq nuevaLista (append nuevaLista (list j)))
						)
						(setq hayListaAnidada 1)
					)
					(setq nuevaLista (append nuevaLista (list i)))
				)
			)
			(if (= 1 hayListaAnidada) (aplana nuevaLista) nuevaLista)
		)
	)
	(print input)
	(print (aplana input)) (fresh-line)


(print "Inciso 8")
; Escriba la función Diagonal que recibe como argumento una lista conteniendo m sub-listas de m elementos cada una de ellas y que representa una matriz de m x m elementos. Como respuesta, esta función debe devolver una lista conteniendo los elementos en la diagonal principal de dicha matriz. Observe que los elementos de la matriz pueden ser de cualquier tipo de datos, no forzosamente numéricos.
	(setq input '(
		(A B C D)
		(E F G H)
		(I J K L)
		(M N Ñ O)
	))
	(defun diagonal (matriz idx)
		(if (null matriz) 
			'()
			(cons (nth idx (first matriz)) (diagonal (rest matriz) (+ 1 idx)))
		)
	)
	(loop for el in input do (print el)) (fresh-line)
	(print (diagonal input 0)) (fresh-line)


(print "Inciso 9")
; Construya una función que reciba como argumento una lista cualquiera y, como respuesta, entregue una lista, con el mismo número de elementos de primer nivel, pero que contiene un símbolo A si el elemento en la posición correspondiente es un átomo, un símbolo L si el elemento correspondiente es una lista y un símbolo N si el elemento en la posición correspondiente es una lista vacía.
	(setq input '(2 (3 4 (5 6 7)) (5 4) 5 9.0 5 ()))
	(defun isAtomOrList (lista) 
		(if (null lista)
			'()
			(cons 
				(if (atom (first lista)) (if (null (first lista)) 'N 'A) 'L)
				(isAtomOrList (rest lista))
			)
		)
	)
	(print (isAtomOrList input)) (fresh-line)


(print "Inciso 10")
; Defina la función Suma-numérica que recibe como argumento una lista cualquiera (no anidada), y como respuesta entrega la suma de exclusivamente aquellos elementos de la lista que son numéricos.
	(setq input '(2 (3 4 (5 6 7)) (5 4) 5 9.0 5 ()))
	(defun sumaNumerica (lista) 
		(if (null lista)
			0
			(+ (sumaNumerica (rest lista)) 
				(if (numberp (first lista))
					(first lista)
					0
				)
			)
		)
	)
	(print (sumaNumerica input)) (fresh-line)


(print "Inciso 11")
; Escriba una función Filtra-vocales que reciba como argumento una lista (con elementos de cualquier tipo y anidada a cualquier nivel de profundidad) y, como respuesta entregue una copia de la lista argumento en la cual se han removido las letras vocales (tanto mayúsculas como minúsculas).
	(setq input '(M A (b c (d e f)) (g h) i j k (o z)))
	(defun removeVowels (lista) 
		(let (
			(answer '())
			)
			(loop 
				for i in lista
			do
				(if (atom i)
					(unless (or 
							(equal 'a i)
							(equal 'e i)
							(equal 'i i)
							(equal 'o i)
							(equal 'u i)
						)
						(setq answer (append answer (list i)))
					)
					(setq answer (append answer (list (removeVowels i))))
				)
			)
			answer
		)
	)
	(print (removeVowels input)) (fresh-line)


(print "Inciso 12")
; Construya una función Filtra-múltiplos que reciba como argumentos una lista y un número entero. Como respuesta debe entregar una copia de la lista argumento en la cual se han removido todos los múltiplos del entero recibido. 
	(setq input '(2 4 6 7 8 9 10 11 13 15 17 18 20))
	(defun removeMultiples (lista x)
		(if (null lista)
			'()
			(if (= 0 (mod (first lista) x)) 
				(cons (first lista) (removeMultiples (rest lista) x))
				(removeMultiples (rest lista) x)
			)
		) 
	)
	(print (removeMultiples input 2)) (fresh-line)


(print "Inciso 13")
; Defina la función Celdas que recibe como argumento una lista (con elementos de cualquier tipo y anidada a cualquier nivel de profundidad) y, como respuesta entrega el número de celdas de construcción que contiene la representación interna de la lista argumento.
	(setq input '(M A (b c (d e f)) (g h) i j k (o . z)))
	(defun celdas (lista) 
		(let (
			(numCeldas 0)
			)
			(loop 
				for i in lista
			do
				(setq numCeldas (+ numCeldas 1))
				(if (consp i) 
					(unless (atom (rest i))
						(setq numCeldas (+ numCeldas (celdas i)))
					)
				)
			)
			numCeldas
		)
	)
	(print input)
	(print (celdas input)) (fresh-line)



(print "Inciso 14")
; Construya una función Implica con aridad indeterminada, que implemente el operador lógico de la implicación.

;
; Pendiente, ¿Qué es implica? XD
;
(fresh-line)


(print "Inciso 15")
; Escriba una función Mult que recibe como argumento dos listas, conteniendo sublistas numéricas, representando matrices. La función debe regresar la multiplicación de las dos matrices si es que éstas son compatibles, en caso de no serlo debe regresar NIL.

;
;	Profe, no supe como hacerla recursiva :(
;
(fresh-line)


(print "Inciso 16")
; Defina una función recursiva Find que reciba dos argumentos: elem y lista. La función debe devolver NIL si elem no es un elemento de lista, de lo contrario, deberá devolver la sublista que comienza con la primera instancia de elem.
	(setq input '(a b c d e f g))
	(defun encuentra (elem lista flag) 
		(if (listp elem) NIL 
			(if (null lista)
				'()
				(if (equal (first lista) elem) 
					(cons (first lista) (encuentra elem (rest lista) 1))
					(if (= flag 1) 
						(cons (first lista) (encuentra elem (rest lista) 1))
						(encuentra elem (rest lista) 0)
					)
				)
			)
		)
	)
	(print input)
	(print (encuentra 'c input 0)) (fresh-line)


(print "Inciso 17")
; Defina una función recursiva Cambia que reciba como argumento una lista y dos elementos elem1, elem2. Como respuesta la función debe entregar otra lista parecida a la original, pero donde todas las ocurrencias de elem1 se substituyeron por elem2.
	(setq input '(a b c a a b c a b b c c))
	(defun cambia (lista elem1 elem2) 
		(if (null lista) 
			'()
			(if (equal (first lista) elem1) 
				(cons elem2 (cambia (rest lista) elem1 elem2))
				(cons (first lista) (cambia (rest lista) elem1 elem2))
			)
		)
	)
	(print input)
	(print (cambia input 'a 'z))(fresh-line)


(print "Inciso 18")
; En el URL http://www.cliki.net/fibonacci se presentan diversas implementaciones para los números de Fibonacci. Implemente TODAS las opciones que ahí se presentan y compare su desempeño con time para el argumento 50
	(fresh-line)(print "Tail-recursive computation of the nth element of the Fibonacci sequence")
	(defun fib (n)
	(check-type n (integer 0 *))
	(labels ((fib-aux (n f1 f2)
						(if (zerop n) f1
						(fib-aux (1- n) f2 (+ f1 f2)))))
			(fib-aux n 0 1)))

	(time (fib 50))

	(fresh-line)(print "loop-based iterative computation of the nth element of the Fibonacci sequence")
	(defun fib (n)
	(check-type n (integer 0 *))
	(loop for f1 = 0 then f2
			and f2 = 1 then (+ f1 f2)
			repeat n finally (return f1)))

	(time (fib 50))

	(fresh-line)(print "do-based iterative computation of the nth element of the Fibonacci sequence")
	(defun fib (n)
	(check-type n (integer 0 *))
	(do ((i n (1- i))
		(f1 0 f2)
		(f2 1 (+ f1 f2)))
		((= i 0) f1)))

	(time (fib 50))

	(fresh-line)(print "CPS computation of the nth element of the Fibonacci sequence")
	(defun fib (n)
	(check-type n (integer 0 *))
	(labels ((fib-aux (n k)
						(if (zerop n)
							(funcall k 0 1)
						(fib-aux (1- n) (lambda (x y)
											(funcall k y (+ x y)))))))
			(fib-aux n #'(lambda (a b) a))))

	(time (fib 50))

	(fresh-line)(print "CPS version, but with the continuation left implicit")
	(defun fib (n)
	(labels ((fib2 (n)
					(cond ((= n 0)
							(values 1 0))
						(t
							(multiple-value-bind (val prev-val)
												(fib2 (- n 1))
							(values (+ val prev-val)
									val))))))
		(nth-value 0 (fib2 n))))

	(time (fib 50))

	(fresh-line)(print "Successive squaring method from SICP")
	(defun fib (n)
	(check-type n (integer 0 *))
	(labels ((fib-aux (a b p q count)
						(cond ((= count 0) b)
							((evenp count)
							(fib-aux a
										b
										(+ (* p p) (* q q))
										(+ (* q q) (* 2 p q))
										(/ count 2)))
							(t (fib-aux (+ (* b q) (* a q) (* a p))
										(+ (* b p) (* a q))
										p
										q
										(- count 1))))))
			(fib-aux 1 0 0 1 n)))

	(time (fib 50))


	(fresh-line)(print "F(2n-1)=F(n)^2+F(n-1)^2, F(2n)=(2F(n-1)+F(n))*F(n) based")
	(defun fib (n)
		(if (< n 2) n
			(if (oddp n)
			(let ((k (/ (1+ n) 2)))
				(+ (expt (fib k) 2) (expt (fib (1- k)) 2)))
			(let* ((k (/ n 2)) (fk (fib k)))
				(* (+ (* 2 (fib (1- k))) fk) fk)))))

	(time (fib 50))

	(fresh-line)(print "Taken from Winston's Lisp, 3rd edition, this is a tail-recursive version, w/o an auxiliary function")
	(defun fib (n &optional (i 1) (previous-month 0) (this-month 1))
		(if (<= n i)
			this-month
			(fib n (+ 1 i) this-month (+ this-month previous-month))))

	(time (fib 50))

	(fresh-line)(print "Fibonacci - Binet's Formula 1")
	(defun fib (n)
		(* (/ 1 (sqrt 5))
			(- (expt (/ (+ 1 (sqrt 5)) 2) n)
			(expt (/ (- 1 (sqrt 5)) 2) n))))

	(time (fib 50))

	(fresh-line)(print "Fibonacci - Binet's Formula 2")
	(defun fib (n)
		(/ (- (expt (/ (+ 1 (sqrt 5)) 2) n)
				(expt (/ (- 1 (sqrt 5)) 2) n))
			(sqrt 5)))

	(time (fib 50))


(print "Inciso 19")
; Defina una función recursiva Mapea que opere exactamente igual que la función mapear de Common Lisp
	(defvar org)(defvar orgMap)
	(setq org '(ce ome elli nahui mahquilli chiquace chiqome chiqelli chiqnahui))
	(setq orgMap '(1 2 3 4 5 6 7 8 9))
	(defun mapeo (fun l1 l2) 
		(if (null l1)
			'()
			(cons 
				(funcall fun (first l1) (first l2))
				(mapeo fun (rest l1) (rest l2))
			)
		)
	)
	(print org)
	(print orgMap)
	(loop for i in (mapeo #'(lambda (x y) (cons x y)) org orgMap) do (print i))(fresh-line)


(print "Inciso 20")
; Defina una función recursiva Aplana que reciba como argumento una lista con elementos anidados a cualquier nivel de profundidad y, como respuesta, entregue una lista conteniendo los mismo elementos pero todos ellos al nivel principal de profundidad
	(setq input '(2 (3 4 (5 6 7)) (5 4) 5 9.0 5))
	(defun aplana (lista)
		(let (
			(nuevaLista '())
			(hayListaAnidada 0)
			)
			(loop 
				for i in lista
			do
				(if (listp i)
					(progn
						(loop 
							for j in i
						do
							(setq nuevaLista (append nuevaLista (list j)))
						)
						(setq hayListaAnidada 1)
					)
					(setq nuevaLista (append nuevaLista (list i)))
				)
			)
			(if (= 1 hayListaAnidada) (aplana nuevaLista) nuevaLista)
		)
	)
	(print input)
	(print (aplana input)) (fresh-line)


(print "Inciso 21")
; Defina una función Elimina que reciba como argumento una lista y un número real n. La función debe entregar como resultado una copia de la lista original, con la cual se hayan eliminado todos los elementos que no sean numéricos, así como todos aquellos elementos numéricos que sean menores o iguales que n
	(setq input '(A 2.0 3 3.1416 8 8.91 9.2 F (a b) 6))
	(defun elimina (lista n)
		(if (null lista)
			'()
			(if (or 
					(not (numberp (first lista))) 
					(and 
						(numberp (first lista)) 
						(<= (first lista) n)
					)
				)
				(elimina (rest lista) n)
				(cons 
					(first lista) 
					(elimina (rest lista) n)
				)
			)
		)
	)
	(print input)
	(print (elimina input 5.0)) (fresh-line)


(print "Inciso 22")
; Defina una función recursiva PegaYCambia que reciba como argumento dos listas lista1, lista2 y dos elementos elem1, elem2. Como respuesta, la función debe entregar una lista donde concatene las dos listas originales, pero substituyendo todas las ocurrencias (en ambas listas) de elem1 por elem2
	(defun pegaYCambia (lista1 lista2 elem1 elem2)
		(if (null lista1)
			(if (null lista2)
				'()
				(cons 
					(if (equal (first lista2) elem1) elem2 (first lista2))
					(pegaYCambia lista1 (rest lista2) elem1 elem2)
				)
			)
			(cons
				(if (equal (first lista1) elem1) elem2 (first lista1))
				(pegaYCambia (rest lista1) lista2 elem1 elem2)
			)
		)
	)
	(print (pegaYCambia '(A B C D E F) '(F E D C B A) 'A 'Z))


(print "Inciso 23")
; Defina una función QSort que reciba como argumento único una lista e implemente con ellos el algoritmo de ordenamiento Quick Sort, ignorando por completo aquellos elementos de la lista original que no sean numéricos. La respuesta de la función debe ser una lista con los elementos numéricos de la original ordenados de forma ascendente.
