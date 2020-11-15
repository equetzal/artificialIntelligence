#!/usr/bin/sbcl --script
;; ===============================
; Ejercicio "El estanque y las ranas"
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
; 	Decidí reprensetar con 1 lista el estado del estanque
;	(V V V E C C C) -> (C C C E V V V)
;	 V = Rana Verde, C = Rana Café, E = Espacio
;
;; =============================== 

;; To run just type "./codigo_estanqueRanas.lisp"


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
(defun isStateValid (state) ;valida si un estado es válido dadas las reglas
	(let (
		(cafes 0)
		(verdes 0)
		)
		(loop for part in state do
			(if (equal 'V part) (setq verdes (+ 1 verdes)))
			(if (equal 'C part) (setq cafes (+ 1 cafes)))
		)
		(and (equal 3 verdes) (equal 3 cafes))
	)
)

; Definición y operaciones con las transisiones
(defparameter ops '(
	(:salta-adelante-1	W)
	(:salta-adelante-2 	X)
	(:salta-atras-1 	Y)
	(:salta-atras-2		Z)
	)
)
(defun canTravel (state frog op); regresa T o NIL dependiendo si puede o no viajar a otro estado
	(let (
		(mov 0)
		)
		(cond 
			((equal 'W (second op)) (setq mov 1))
			((equal 'X (second op)) (setq mov 2))
			((equal 'Y (second op)) (setq mov -1))
			((equal 'Z (second op)) (setq mov -2))
		)
		(let (
			(pos (+ frog mov))
			)
			(and
				(>= pos 0)
				(< pos (list-length state))
				(equal 'E (nth pos state))
			)
		)
	)
)
(defun travel (state frog op) ; realiza el cambio de estado del viaje
	(let (
		(newState (copy-list state))
		(mov 0)
		)
		(cond 
			((equal 'W (second op)) (setq mov 1))
			((equal 'X (second op)) (setq mov 2))
			((equal 'Y (second op)) (setq mov -1))
			((equal 'Z (second op)) (setq mov -2))
		)
		(let (
			(pos (+ frog mov))
			)
			(setf (nth pos newState) (nth frog newState))
			(setf (nth frog newState) 'E)
		)
		newState
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
			(isDfs (push-front (newNode initialState '(NIL NIL) method)))
			(isBfs (push-back (newNode initialState '(NIL NIL) method)))
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
						(loop for frog in '(0 1 2 3 4 5 6) do
							(if (canTravel currentState frog op) ;Si puede realizar el viaje
								(progn 
									(setq nextState (travel currentState frog op)) ;Calcula el siguiente estado
									(unless (canRemember nextState) ;verifica que no lo haya visitado
										(if (isStateValid nextState) ;y valida que sea valido hacer eso
											(progn
												(setq tempNode (newNode nextState (list (+ 1 frog) op) method))
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
		)

		(if isFinalState 
			(progn ;si hay solución
				(rememberPath finalStateId) ;recrea el camino con los recuerdos
				(format t "Solución de ~A pasos encontrada en ~A intentos ~%" (list-length solutionPath) finalStateId)
				(loop for step in solutionPath do ;y los imprime
					(format t "Paso # ~S con la rana ~A aplicando ~20A obtenemos -> ~20A ~%" stepCount (first (nth 3 step)) (second (nth 3 step)) (nth 1 step))
					(setq stepCount (+ 1 stepCount))
				)
			)
			(format t "Luego de ~A intentos, no se encontró solución~%" finalStateId) ;si no hay solución
		)
	)
)

; Mandamos a llamar la búsqueda ciega
(blindSearch '(V V V E C C C) '(C C C E V V V) :deep-first)
(fresh-line)
