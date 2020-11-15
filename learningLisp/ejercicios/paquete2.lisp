#!/usr/bin/sbcl --script

; Paquete de ejercicios 2
; Alumna: Enya Quetzalli Gómez Rodríguez (Eduardo Gómez Rodríguez)
; Profesor: Dr. Salvador Godoy
; Escuela: Centro de Investigación de Cómputo - IPN
; Semestre: 2021/1

(defvar input)
(defvar output)


(print "Inciso 1")
;-------- Inciso 1 --------
; [sin usar ELT ni POSITION] Defina una función ElemInPos que reciba tres argumentos: elem, lista y pos. La función debe devolver T si elem está en la posición pos de lista, NIL si no lo está.
	(setq input '(A B C D E))
	(defun elemInPos (elemento lista posición) 
		(equal
			elemento
			(nth posición lista)
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
	(defun primerImpar (lista) 
		(let (
			(flag -1)
			(idx -1)
			(elem -1)
			)
			(loop 
				for i in lista
			do
				(setq idx (+ idx 1))
				(if (= -1 flag) 
					(if (numberp i) 
						(if (= 1 (mod i 2)) 
							(progn 
								(setq elem i) 
								(setq flag idx)
							)
						)
					)
				) 
			)
			(list elem flag)
		)
	)
	(print (primerImpar input)) (fresh-line)


(print "Inciso 5")
; Modifique la función del inciso anterior para que entregue en la lista de respuesta el último elemento de la lista que sea un número real mayor o igual que cero y el número de veces que dicho elemento se repite en toda la lista.
	(setq input '(A () 2.0 8.2 D 7.1 P 4 7.1))
	(defun freqUltimoReal (lista) 
		(let (
			(contador 0)
			(elem -1)
			)
			(loop 
				for i in lista
			do
				(if (and (realp i) (>= i 0.0)) 
					(setq elem i) 
				) 
			)
			(loop 
				for i in lista
			do
				(if (and (realp i) (= i elem))
					(setq contador (+ contador 1)) 
				) 
			)
			(list elem contador)
		)
	)
	(print (freqUltimoReal input)) (fresh-line)


(print "Inciso 6")
; Escriba la función Conteo que recibe como argumento una lista cualquiera y, como respuesta, entregue una celda de construcción cuya primera parte contiene el conteo de elementos numéricos de la lista original y cuya segunda parte contiene el conteo de sublistas contenidas en la lista original.
	(setq input '(2 (3 4) (5 4) 5 9.0 5))
	(defun conteo (lista) 
		(let (
			(num 0)
			(lis 0)
			)
			(loop 
				for i in lista
			do 
				(if (numberp i) (setq num (+ num 1)))
				(if (listp i) (setq lis (+ lis 1)))
			)
			(cons num lis)
		)
	)
	(print (conteo input)) (fresh-line)


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
	(defun diagonal (matriz) 
		(let ( 
			(m (list-length matriz))
			(lista '())
			)
			(do (
				(i 0 (+ 1 i))
			)((= m i) lista)
				(setq lista 
					(append lista 
						(list (nth i (nth i matriz)))
					)
				)
			)
		)
	)
	(loop for el in input do (print el)) (fresh-line)
	(print (diagonal input)) (fresh-line)


(print "Inciso 9")
; Construya una función que reciba como argumento una lista cualquiera y, como respuesta, entregue una lista, con el mismo número de elementos de primer nivel, pero que contiene un símbolo A si el elemento en la posición correspondiente es un átomo, un símbolo L si el elemento correspondiente es una lista y un símbolo N si el elemento en la posición correspondiente es una lista vacía.
	(setq input '(2 (3 4 (5 6 7)) (5 4) 5 9.0 5 ()))
	(defun isAtomOrList (lista) 
		(let (
			(answer '())
			)
			(loop 
				for i in lista
			do
				(setq answer 
					(append answer (list 
							(if (atom i) 
								(if (equal NIL i) 'N 'A)
								'L
							)
						)
					)
				)
			)
			answer
		)
	)
	(print (isAtomOrList input)) (fresh-line)


(print "Inciso 10")
; Defina la función Suma-numérica que recibe como argumento una lista cualquiera (no anidada), y como respuesta entrega la suma de exclusivamente aquellos elementos de la lista que son numéricos.
	(setq input '(2 (3 4 (5 6 7)) (5 4) 5 9.0 5 ()))
	(defun sumaNumerica (lista) 
		(let (
			(suma 0)
			)
			(loop 
				for i in lista
			do
				(if (numberp i)
					(setq suma (+ suma i))
				)
			)
			suma
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
		(let (
			(answer '())
			)
			(loop 
				for i in lista
			do
				(unless (= 0 (mod i x))
					(setq answer (append answer (list i)))
				)
			)
			answer
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


; Pendiente, ¿Qué es implica? XD


(print "Inciso 15")
; Escriba una función Mult que recibe como argumento dos listas, conteniendo sublistas numéricas, representando matrices. La función debe regresar la multiplicación de las dos matrices si es que éstas son compatibles, en caso de no serlo debe regresar NIL.
	(defvar mtx1)
	(defvar mtx2)
	(setq mtx1 '(
			(3 2 1)
			(1 1 3)
			(0 2 1)
		)	
	)
	(setq mtx2 '(
			(2 1)
			(1 0)
			(3 2)
		)
	)
	(defun esMatrizValida (matriz) 
		(let (
			(n (list-length matriz))
			(m (list-length (first matriz)))
			(isValid T)
			)

			(loop 
				for i in matriz
			do
				(unless (= m (list-length i)) 
					(setq isValid NIL)
				)
				(loop 
					for j in i
				do
					(unless (numberp j) 
						(setq isValid NIL)
					)
				)
			)
			isValid
		)
	)
	(defun tamañosValidos (matriz1 matriz2)
		(let (
			(a (list-length matriz1))
			(b (list-length (first matriz1)))
			(c (list-length matriz1))
			(d (list-length (first matriz1)))
			)
			(if (= b c) T NIL)
		)
	)
	(defun obtenerFila (matriz n)
		(let (
			(fila '())
			)
			(setq fila (nth n matriz))
		)
	)
	(defun obtenerColumna (matriz n)
		(let (
			(columna '())
			)
			(loop 
				for i in matriz
			do
				(setq columna (append columna (list (nth n i))))
			)
			columna
		)
	)
	(defun obtenerValor (l1 l2)
		(let (
			(val 0)
			(n (list-length l1))
			)
			(do (
				(i 0 (+ i 1))
			)((= n i) val)
				(setq val (+ val 
					(* (nth i l1) (nth i l2))
				))
			)
		)
	)
	(defun multiplicaMatrices (matriz1 matriz2) 
		(if (and (esMatrizValida matriz1) (esMatrizValida matriz2) (tamañosValidos matriz1 matriz2))
			(progn 
				(let (
						(nuevaMatriz '())
						(listaTemp '())
						(a (list-length matriz1))
						(b (list-length (first matriz1)))
						(c (list-length (first matriz2)))
					)
					(do (
							(i 0 (+ i 1))
						)((= i a) nuevaMatriz)
						(setq listaTemp '())
						(do (
							(j 0 (+ j 1))
						)((= j c) listaTemp)
							(setq listaTemp 
								(append listaTemp (list 
									(obtenerValor (obtenerFila matriz1 i) (obtenerColumna matriz2 j)) 
								))
							)
						)
						(setq nuevaMatriz (append nuevaMatriz (list listaTemp)))					
					)
					nuevaMatriz
				)
			)
			NIL
		)
	)
	(print "   Matriz1:")
	(loop for i in mtx1 do (print i)) (fresh-line)
	(print "   Matriz2:")
	(loop for i in mtx2 do (print i)) (fresh-line)
	(print "   Resultado:")
	(loop for i in (multiplicaMatrices mtx1 mtx2) do (print i)) (fresh-line)