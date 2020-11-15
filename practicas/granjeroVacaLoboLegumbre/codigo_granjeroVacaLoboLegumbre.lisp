#!/usr/bin/sbcl --script
;; ===============================
; Ejercicio "Granjero Vaca Lobo y Legumbre"
; Alumna: Enya Quetzalli Gómez Rodríguez (Eduardo Gómez Rodríguez)
; Boleta: 2014030529
; Profesor: Dr. Salvador Godoy
; Escuela: Centro de Investigación de Cómputo - IPN
; Semestre: 2021/1
;; ===============================
;
; Instrucciones: 
;	Siguiendo la misma metodología y estructura del programa de Misioneros y Caníbales,  programe la solución a los problemas de Ranas y estanque nocturno,  así como  Granjero, lobo, oveja y legumbres. Ambos programas deben usar el algoritmo generalizado visto en clase y calcular los 5 indicadores de desempeño también vistos en clase. Deberán los programas estar "adecuadamente" comentados según el ejemplo que se entregó. 
;
; Representación:
; 	Decidí reprensetar con 2 listas el estado de cada lado del rio:
;	(1 1 1 1) (0 0 0 0)
;	 E O A G   E O A G  -> E = legumbrE, O = lobO, A = vacA, G = Granjero
;
;; =============================== 

;; To run just type "./granjeroVacaLoboLegumbre.lisp" on the terminal

; Definición y operaciones de la frontera de búsqueda
(defparameter nextNode '()) ;frontera
(defparameter solutionPath '()) ;camino de la solución
(defparameter globalNodeId -1) ;conteo de nodos creados
(defparameter globalCurrentNodeId nil) ;nodo que se procesa al momento

(defun push-front (node)
	(push node nextNode)
)
(defun pop-front ()
	(pop nextNode)
)
(defun push-back (node)
	(setq nextNode (append nextNode (list node)))
)
(defun pop-back (deque)
	(setq nextNode (butlast nextNode))
)
(defun newId ()
	(setq globalNodeId (+ 1 globalNodeId))
)
(defun newNode (state operation method) ;crea un nodo para la frontera
	(list (newId) state globalCurrentNodeId operation method)
)

; Definición y operaciones de los estados
(defun canEatEachOther (side) ;reglas lógicas del problema
	(let (
		(legumbre (equal 1 (nth 0 side)))
		(lobo (equal 1 (nth 1 side)))
		(vaca (equal 1 (nth 2 side)))
		)
		(or 
			(and legumbre vaca)
			(and lobo vaca)
		)
	)
)
(defun isStateValid (state) ;valida si un estado es válido dadas las reglas
	(let (
		(izq (first state))
		(der (second state))
		(aloneSide NIL)
		)
		(if (equal 0 (nth 3 izq)) ;El lado solo es donde no pueden estar juntos
			(setq aloneSide izq)
			(setq aloneSide der)
		)
		(not (canEatEachOther aloneSide))
	)
)

; Definición y operaciones con las transisiones
(defparameter ops '(
	(:viaja-vaca 		A)
	(:viaja-lobo 		O)
	(:viaja-legumbre 	E)
	(:viaja-solo		S)
	)
)
(defun canTravel (state op); regresa T o NIL dependiendo si puede o no viajar
	(let (
		(izq (first state))
		(der (second state))
		(animal -1)
		)
		(cond 
			((equal 'E (second op)) (setq animal 0))
			((equal 'O (second op)) (setq animal 1))
			((equal 'A (second op)) (setq animal 2))
			((equal 'S (second op)) (setq animal 3))
		)
		(let (
			(isIzq (equal 1 (nth animal izq)))
			(isDer (equal 1 (nth animal der)))
			)
			(or
				(and (equal 1 (nth 3 izq)) isIzq)
				(and (equal 1 (nth 3 der)) isDer)
			)
		)
	)
)
(defun travel (state op) ; realiza el cambio de estado para viajar
	(let (
		(izq (copy-list (first state)))
		(der (copy-list (second state)))
		(animal -1)
		)
		(cond 
			((equal 'E (second op)) (setq animal 0))
			((equal 'O (second op)) (setq animal 1))
			((equal 'A (second op)) (setq animal 2))
			((equal 'S (second op)) (setq animal 3))
		)
		(let (
			(isIzq (equal 1 (nth animal izq)))
			)
			(if isIzq
				(progn 
					(setf (nth animal izq) 0)
					(setf (nth 3 izq) 0)
					(setf (nth animal der) 1)
					(setf (nth 3 der) 1)
				)
				(progn 
					(setf (nth animal izq) 1)
					(setf (nth 3 izq) 1)
					(setf (nth animal der) 0)
					(setf (nth 3 der) 0)
				)
			)
		)
		(list izq der)
	)
)

; Definición y operaciones de la memoria
; (node result)
(defparameter memory '())
(defun remember (node) ; agrega un nodo a la memoria
	(push (copy-list node) memory)
)
(defun canRemember (state) ;regresa T si un estado esta en la memoria
	(let (
		(isAnImage NIL)
		(resultOfImage NIL)
		)
		(loop for image in memory do
			(if (equal (second image) state)
				(progn 
					(setq isAnImage T)
				)
			)
		)
		isAnImage
	)
)
(defun rememberPath (stateId) ;regresa el camino usando la memoria
	(unless (equal NIL stateId)
		(loop for image in memory do
			(if (equal stateId (first image))
				(progn 
					(push image solutionPath)
					(rememberPath (third image))
				)
			)
		)
	)
)

; Definición y operaciones de la busqueda ciega
(defun blindSearch (initialState finalState method) ; búsqueda ciega
	(let (
		(isDfs (equal :deep-first method))
		(isBfs (equal :breadth-first method))
		(isFinalState NIL)
		(currentNode NIL)
		(currentState NIL)
		(nextState NIL)
		(tempNode NIL)
		(finalStateId NIL)
		(stepCount 0)
		)
		(cond ;inicializa la frontera de búsqueda con el estado inicial
			(isDfs (push-front (newNode initialState NIL method)))
			(isBfs (push-back (newNode initialState NIL method)))
		)
		(loop until (or isFinalState (null nextNode)) do
			(setq currentNode (first nextNode))
			(setq globalCurrentNodeId (first currentNode))
			(setq currentState (second currentNode))
			(remember currentNode) ;agrega a la memoria y saca de la frontera
			(pop-front)
			
			(if (equal currentState finalState)
				(progn ;si es estado final, alzamos la bandera
					(setq isFinalState T)
					(setq finalStateId globalCurrentNodeId)
				)
				(progn
					;Aplicamos las operaciones
					(loop for op in ops do
						(if (canTravel currentState op) ;Si puede realizar el viaje
							(progn 
								(setq nextState (travel currentState op)) ;Calcula el siguiente estado
								(unless (canRemember nextState) ;verifica que no lo haya visitado
									(if (isStateValid nextState) ;y valida que sea valido hacer eso
										(progn
											(setq tempNode (newNode nextState op method))
											(cond ;lo agrega a la frontera de busqueda
												(isDfs (push-front tempNode))
												(isBfs (push-back tempNode))
											)
										)
									)
								)
							)
						)
					)
				)
			)
		)

		(if isFinalState 
			(progn ;si hay solución
				(rememberPath finalStateId) ;recrea el camino con los recuerdos
				(format t "Solución de ~A pasos encontrada en ~A intentos ~%" (list-length solutionPath) finalStateId)
				(loop for step in solutionPath do ;y los imprime
					(format t "Paso # ~A aplicando ~A obtenemos -> ~A ~%" stepCount (nth 3 step) (nth 1 step) )
					(setq stepCount (+ 1 stepCount))
				)
			)
			(format t "Luego de ~A intentos, no se encontró solución~%" finalStateId) ;si no hay solución
		)
	)
)

; Mandamos a llamar la búsqueda ciega
(blindSearch '((1 1 1 1) (0 0 0 0)) '((0 0 0 0) (1 1 1 1)) :deep-first)
(fresh-line)
