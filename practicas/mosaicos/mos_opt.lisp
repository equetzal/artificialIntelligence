#!/usr/bin/sbcl --script
;; ===============================
; Ejercicio "Mosaicos"
; Alumna: Enya Quetzalli Gómez Rodríguez (Eduardo Gómez Rodríguez)
; Boleta: 2014030529
; Profesor: Dr. Salvador Godoy
; Escuela: Centro de Investigación de Cómputo - IPN
; Semestre: 2021/1
;; ===============================
;
; Representación:
;	Mapa:
;	(	(1	(NIL	2	4	3	NIL	NIL	))  
;   	(2	(NIL	NIL	5	4	1	NIL	))
;       (3	(1		4	NIL	NIL	NIL	NIL	))
;       (4	(2		5	NIL	NIL	3	1	))
;       (5	(NIL	NIL	NIL	NIL	4	2	)))
;
;	Hexagonos:
;		(A  (7 10 6 9 12 8) )
;		Identificador de Hexagono, Lista de colores en sus caras
;
;	Solución: 
;		( (1 A 30) (2 B 60))
;		Numero de Hexagono en el mapa, Identificaor de Hexagono, Rotacion en grados del Hexagono
;
;	Estado:
;		(NIL NIL (A 0) NIL NIL NIL)
;		Lista del tamaño de hexagonos en el mapa, con NIL si no hay ninguna pieza, o una lista si la hay, 
;		cada pieza contiene el identificador de hexagono y su rotación en forma de número 0-5
;		El número indica el numero de veces que será rotada la lista de adyacencias
;
;	Operacion:
;		(A 0 1)
;		Etiqueta del hexagono, Numero de rotaciones, Slot en donde colocarlo
;	
;; =============================== 

;Primero debe cargarse la biblioteca de la siguiente forma:
(load "mosaic-lib.lisp")

; Para añadir un algoritmo al menú de la página, es necesario usar la función
; "add-algrithm"como se muestra a continuación. No importa en qué lugar
; del código se use pero, de preferencia, usar al inicio del código.
; Puede omitirse al realizar pruebas en su computadora.

(add-algorithm 'depth-first)
(add-algorithm 'aStar)

;Funcion de muestra. Regresa el resultado de un algoritmo de búsqueda a lo
;ancho. Esta función no debe llevar argumentos.
(defparameter mosBoard NIL)
(defparameter mosPieces NIL)

(defparameter nextNode '()) ;frontera
(defparameter solutionPath '()) ;camino de la solución
(defparameter globalNodeId -1) ;conteo de nodos creados
(defparameter globalCurrentNodeId NIL) ;nodo que se procesa al momento
(defparameter solutionScore NIL)

;Time O(1)
(defun push-front (node)
	(push node nextNode)
)
;Time O(1)
(defun pop-front ()
	(pop nextNode)
)
;Time O(n)
(defun push-back (node)
	(setq nextNode (append nextNode (list node)))
)
(defun pop-back (deque)
	(setq nextNode (butlast nextNode))
)
(defun newId ()
	(setq globalNodeId (+ 1 globalNodeId))
)
(defun newNode (state operation method score deep parentState) ;crea un nodo para la frontera
	(list (newId) state parentState operation method score deep)
)
(defparameter priorityHash (make-hash-table :test #'equal))
(defparameter maxPriority 0)
(defun insertWithPriority (node)
	;(format t "List of priority #~A = ~100A ~%" (nth 5 node) (gethash (nth 5 node) priorityHash))
	(setq maxPriority (max maxPriority (nth 5 node)))
	(push node (gethash (nth 5 node) priorityHash))
	;(format t "List of priority #~A => ~100A ~%" (nth 5 node) (gethash (nth 5 node) priorityHash))
)
(defun getWithPriority ()
	(let (
		(nextNode '())
		)
		(loop for i from 0 to maxPriority by 1 do
			;(format t "List of priority #~A = ~100A ~%" i (gethash i priorityHash))
			(unless (null (gethash i priorityHash))
				(setq nextNode (first (gethash i priorityHash)))
				(pop (gethash i priorityHash))
				;(format t "List of priority #~A updated to => ~100A ~%" i (gethash i priorityHash))
				(return-from getWithPriority nextNode)
			)
		)
	)
)
(defun isPriorityEmpty ()
	(loop for i from 0 to maxPriority by 1 do
		(unless (null (gethash i priorityHash))
			(return-from isPriorityEmpty NIL)		
		)
	)
	T
)

(defun calculateSolutionScore ()
	(let (
		(uniquePairs '())
		(temp '())
		)
		(loop for slot across mosBoard do
			(loop for neighbour in (second slot) do
				(unless (null neighbour)
					(setq temp (list (min (first slot) neighbour) (max (first slot) neighbour)))
					(setq uniquePairs (adjoin temp uniquePairs))
				)
			)
		)
		(setq solutionScore (list-length uniquePairs))
	)
)

(defun calculateStateScore (state deep)
	(if (null solutionScore) (calculateSolutionScore))
	(let (
		(uniquePairs '())
		(temp '())
		)
		(loop for slot across mosBoard do
			(loop for neighbour in (second slot) do
				(unless (null neighbour)
					(setq temp (list (min (first slot) neighbour) (max (first slot) neighbour)))
					(unless 
						(or 
							(null (nth (- (first temp) 1) state)) 
							(null (nth (- (second temp) 1) state)))
						(setq uniquePairs (adjoin temp uniquePairs))			
					)
				)
			)
		)
		(+ deep (- solutionScore (list-length uniquePairs)))
	)
)

;Operaciones útiles para las piezas
(defun rotateColors (piece rotations)
	;(format t "Piece ~A with colors " piece)
	(let (
		(adj (copy-list (get-piece-info piece)))
		)
		;(format t " and applying ~A right rotations result in " rotations)
		(do (
			(i 0 (+ i 1))
		)((= i rotations) adj)
			(setq adj (cons (first (last adj)) (butlast adj)))
		)
		;(format t "~20A ~%" adj)
		adj
	)
)

; Definición y operaciones de los estados
(defun isStateValid (state) ;valida si un estado es válido dadas las reglas
	(let (
		(usedHexagons '())
		(slot NIL)
		(slotNeighbours NIL)
		(piece NIL)
		(rotation NIL)
		(pieceColors NIL)
		(color NIL)
		(neighbour NIL)
		(neighbourNeighbours NIL)
		(neighbourTag NIL)
		(neighbourRotation NIL)
		(neighbourColors NIL)
		(isValid T)
		)
		;Primero validaremos si ninguna pieza esta repetida
		(loop for el in state do
			(unless (null el)
				;(format t "No es null, es ~A ~%" el)
				;(format t "Used Hexagons = ~A ~%" usedHexagons)
				;(format t "Member? ~A ~%" (member (first el) usedHexagons))
				(unless (null (member (first el) usedHexagons))
					(setq isValid NIL)
				)
				(setq usedHexagons (adjoin (first el) usedHexagons))
			)
		)
		(if (equal isValid NIL)
			(return-from isStateValid isValid)
		)
		;(format t "Valido en anteriores, verificando emparejamientos~%")
		;Luego validaremos que todos los colores de las piezas ya colocadas coincidan en sus colores
		(do ( ;Iteramos sobre el estado para ver qué piezas ya colocamos en el tablero
			(i 0 (+ i 1)) ;El indice nos dice cual slot del tablero estamos revisando
			)((= i (list-length state)) isValid)
			(setq slot (nth i state)) ;asignamos el slot a currentElement para mayor facilidad
			(unless (null slot) ;Si no es NIL, entonces ya lo asignamos
				;(format t "Slot: ~A ~%" slot)
				(setq piece (first slot))
				(setq rotation (second slot))
				;(format t "Piece ~A with ~A rotations on the map at slot ~A ~%" piece rotation (+ 1 i))
				;(format t "Slot ~A has neighbours " (+ 1 i)) 
				(setq slotNeighbours (get-adjacent (+ 1 i))) 
				;(format t "~%")
				(setq pieceColors (rotateColors piece rotation)) ;Obtenemos los colores rotados de la pieza en el mismo orden que la lista de adyacencia en el tablero
				
				(do ( ;Iteramos sobre los lados de adyacencia de la pieza para verificar que coincidan con el vecino
					(side 0 (+ side 1))
					)((= side (list-length pieceColors)) NIL)
					(unless (equal NIL (nth side slotNeighbours)) ;Sabemos que el side tiene un vecino
						(setq color (nth side pieceColors))
						(setq neighbour (nth side slotNeighbours))
						;(format t "  Color ~A has slot ~A as neighbour ~%" color neighbour)
						(unless (null (nth (- neighbour 1) state)) ;Solo revisamos el vecino si su slot esta ocupado por una pieza
							(setq neighbourTag (first (nth (- neighbour 1) state)))
							(setq neighbourRotation (second (nth (- neighbour 1) state)))
							;(format t "  Neighbour ~A has adjacecies " neighbour) 
							(setq neighbourNeighbours (get-adjacent neighbour)) 
							;(format t "~%")
							;(format t "  ")
							(setq neighbourColors (rotateColors neighbourTag neighbourRotation))

							(do ( ;Iteramos sobre los lados del vecino
								(neighbourSide 0 (+ neighbourSide 1))
							)((= neighbourSide (list-length neighbourColors)) NIL)
								(if (equal (+ 1 i) (nth neighbourSide neighbourNeighbours)) ;Revisamos el que coincide con nuestro lado
									(progn 
										;(format t "    Anchor color ~A vs ~A of neighbour ~%" color (nth neighbourSide neighbourColors))
										(unless (equal color (nth neighbourSide neighbourColors)) ;Si el color es diferente al nuestro entonces el estado no es valido
											(setq isValid NIL) 
											;(format t "    Los colores no coinciden, estado invalido ~%")
										)
									)
								)
							)

						)

					)
				)
			)
		)
		isValid
	)
)
(defun isFull (state) 
	(let (
		(full T)
		)
		(loop for slot in state do
			(if (null slot)
				(setq full NIL)
			)
		)
		full
	)
)
(defun isSolution (state)
	(and (isFull state) (isStateValid state))
)
(defun stateToRequireSolution (state)
	(let (
		(requiredSolution '())
		)
		(do (
			(slot 0 (+ slot 1))
		)((= slot (list-length state)) requiredSolution)
			(unless (null (nth slot state))
				(push 
					(list (+ 1 slot) (first (nth slot state)) 
						(* 60 (second (nth slot state)))
					)
				requiredSolution)
			)
		)
	)
)

;(print (isStateValid someState))
;(print (rotateColors 'A 3))
;(print (stateToRequireSolution someState))
;(fresh-line)

; Definición y operaciones con las transisiones
(defparameter ops '())
(defun addOps ()
	(setq ops '())
	(let (
		(match (make-hash-table))
		(order '())
		(counter 0)
		)

		(loop for slot across mosBoard do
			(setq counter 0)
			(loop for side in (second slot) do
				(unless (null side)
					(setq counter (+ 1 counter))
				)
			)
			(push slot (gethash counter match))
		)

		(loop for i from 0 upto 6 do 
			;(format t "Slots with ~A adjacencies -> ~100A ~%" i (gethash i match))
			(loop for slot in (gethash i match) do
				(push slot order)
			)
		)

		;(print order)

		(loop for slot in order do
			(loop for hexagon in mosPieces do
				(loop for rotation from 0 to 5 by 1 do
					(push (list (first hexagon) rotation (first slot)) ops)
				)
			)
		)
	)
)
(defun canTravel (state op); regresa T o NIL dependiendo si puede o no viajar a otro estado
	(null (nth (- (third op) 1) state))
)
(defun travel (state op) ; realiza el cambio de estado del viaje
	(let (
		(newState (copy-list state))
		(pos (- (third op) 1))
		)
		(setf (nth pos newState) (butlast op))
		newState
	)
)

; Definición y operaciones de la memoria
(defparameter memory (make-hash-table :test #'equal))
(defun remember (node) ; agrega un nodo a la memoria
	(setf (gethash (second node) memory) (copy-list node))
)
(defun canRemember (state) ;regresa T si un estado esta en la memoria
	(if (null (gethash state memory))
		NIL
		T
	)
)
(defun rememberPath (state) ;regresa el camino usando la memoria
	(unless (null (gethash state memory))
		(unless (null (third (gethash state memory)))
			(push (gethash state memory) solutionPath)
			(rememberPath (third (gethash state memory)))
		)
	)
)

; Definición y operaciones de la busqueda ciega
(defun blindSearch (initialState method maxSolutions) ; búsqueda ciega
	(let (
		(isDfs (equal :deep-first method))
		(isBfs (equal :breadth-first method))
		(isAStar (equal :a-star method))
		(numberOfSols 0)
		(currentNode NIL)
		(currentState NIL)
		(nextState NIL)
		(tempNode NIL)
		(finalStateId NIL)
		(deep 0)
		(stepCount 0)
		(flag NIL)
		)
		(cond ;inicializa la frontera de búsqueda con el estado inicial
			(isDfs (push-front (newNode initialState '(NIL NIL) method 0 0 NIL)))
			(isBfs (push-back (newNode initialState '(NIL NIL) method 0 0 NIL)))
			(isAStar 
				(insertWithPriority 
					(newNode initialState '(NIL NIL) method (calculateStateScore initialState 0) 0 NIL)
			))
		)
		;(remember (first nextNode)) 
		;(format t "Initial memory ~50A" memory)
		(fresh-line)
		(loop until (or 
			(equal numberOfSols maxSolutions) 
			(if isAStar (isPriorityEmpty) (null nextNode))
			flag) do
			(setq currentNode 
				(if isAStar (getWithPriority) (first nextNode))
			)
			(setq globalCurrentNodeId (first currentNode))
			(setq currentState (second currentNode))
			(setq deep (nth 6 currentNode))
			(pop-front)
			;(if (equal deep 2) (setq flag T))
			;(format t "Current State -> ")
			;(format t "~%")
			(unless (canRemember currentState)
				(remember currentNode)
				;(format t "~70A ~%" currentNode)
				(if (isSolution currentState)
					(progn ;si es estado final, alzamos la bandera
						(setq numberOfSols (+ 1 numberOfSols))
						(setq finalStateId globalCurrentNodeId)
						(push (stateToRequireSolution currentState) *solution*)
						;(print (stateToRequireSolution currentState))
					)
					(progn
						;Aplicamos las operaciones
						(loop for op in ops do
							;(format t "Can travel from ~A with ~20A? ~A ~%" currentState op (canTravel currentState op))
							(if (canTravel currentState op) ;Si puede realizar el viaje
								(progn 
									(setq nextState (travel currentState op)) ;Calcula el siguiente estado
									;(format t "   Travel ~A ~%" nextState)
									;(format t "Next State ~A" nextState)
									(unless (canRemember nextState) ;verifica que no lo haya visitado
										(if (isStateValid nextState) ;y valida que sea valido hacer eso
											(progn
												(cond ;lo agrega a la frontera de busqueda
													(isDfs 
														(setq tempNode (newNode nextState op method 0 (+ 1 deep) currentState))
														(push-front tempNode)
													)
													(isBfs 
														(setq tempNode (newNode nextState op method 0 (+ 1 deep) currentState))
														(push-back tempNode)
													)
													(isAStar 
														(setq tempNode
															(newNode nextState op method
																(calculateStateScore nextState (+ 1 deep)) 
																(+ 1 deep)
																currentState
															)
														)
														(insertWithPriority tempNode)
													)
												)
												;(remember tempNode)
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
	*solution*
)


(defun depth-first ()
	(let (
		(initialState '())
		)
		(setq mosBoard (get-board))
		(setq mosPieces (get-pieces))
		(addOps)
		(loop for slot across mosBoard do
			(push NIL initialState)
		)
		(setq *solution* '())
		(setq *solution* (blindSearch initialState :deep-first 1))
		;(print *solution*)
	)
)

(defun aStar ()
	(let (
		(initialState '())
		)
		(setq mosBoard (get-board))
		(setq mosPieces (get-pieces))
		(addOps)
		(loop for slot across mosBoard do
			(push NIL initialState)
		)
		;(format t "Initial State = ~A ~%" initialState)
		(setq *solution* '())
		(setq *solution* (blindSearch initialState :a-star 2))
		;(format t "Solution = ~A ~%" *solution*)
	)
)

(print (aStar))

;(defvar someState '(NIL (B 3) (A 5) NIL NIL NIL NIL NIL NIL NIL))
;(print (isStateValid someState))

;(start-table)
