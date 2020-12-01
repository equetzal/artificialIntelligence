#!/usr/bin/sbcl --script

; ==== Libreria del problema ====
(load "mosaic-lib.lisp")

; ==== Definiciones Importantes ====
(add-algorithm 'depth-first-search)
(add-algorithm 'breadth-first-search)
(add-algorithm 'best-first-search)
(add-algorithm 'a-star)

(defparameter searchMethod NIL)
(defparameter mosBoard NIL)
(defparameter mosPieces NIL)


; ==== Bloque de la frontera de búsqueda ===
(defparameter nodeId 0)
(defparameter queue '())
(defparameter priorityQueue (make-hash-table :test #'equal))
(defparameter priorityFinder (make-hash-table :test #'equal))
(defparameter maximalPriority 0)

;Se crea un nodo para la frontera de búsqueda
(defun createNode (state score operation deep parent)
	(let (
		(newNode (make-array '(6)))
		)
		(setf (aref newNode 0) nodeId) ;id
		(setf (aref newNode 1) state);state
		(setf (aref newNode 2) score);score
		(setf (aref newNode 3) operation);operation
		(setf (aref newNode 4) deep);deep
		(setf (aref newNode 5) parent);parent

		(setq nodeId (+ 1 nodeId))
		newNode
	)
)

;Regresa verdadero o falso dependiendo de si la frontera de busqueda tiene o no ese estado
(defun isOnQueue (node)
	(cond 
		((or (equal searchMethod :breadth-first-search) (equal searchMethod :depth-first-search))
			(loop for element in queue do
				(if (equal (aref node 1) (aref element 1))
					(return-from isOnQueue T)
				)
			)
			(return-from isOnQueue NIL)
		)

		((or (equal searchMethod :best-first-search) (equal searchMethod :a-star))
			(if (null (gethash (aref node 1) priorityFinder))
				(return-from isOnQueue NIL)
				(return-from isOnQueue T)
			)
		)
	)
)

;Borra cualquier nodo en la frontera de busqueda cuyo estado sea el mismo que node
(defun deleteFromQueue (node)
	(cond 
		((or (equal searchMethod :breadth-first-search) (equal searchMethod :depth-first-search))
			(setq queue (remove-if #'(lambda (x) (equal (aref x 1) (aref node 1))) queue))
		)

		((or (equal searchMethod :best-first-search) (equal searchMethod :a-star))
			(setf 
				(gethash (gethash (aref node 1) priorityFinder) priorityQueue) 
				(remove-if #'(lambda (x) (equal (aref x 1) (aref node 1))) 
					(gethash (gethash (aref node 1) priorityFinder) priorityQueue)))
			(remhash (aref node 1) priorityFinder)
		)
	)
)

;Agrega un nodo a la frontera de búsqueda, en a-star y best-fs sustituye un previo si el nuevo es mejor
(defun addToQueue (node)
	(cond 
		((equal searchMethod :breadth-first-search)
			(unless (isOnQueue node) (setq queue (append queue (list node))))
		)
		((equal searchMethod :depth-first-search)
			(unless (isOnQueue node) (push node queue))
		)
		((or (equal searchMethod :best-first-search) (equal searchMethod :a-star))
			(if (isOnQueue node)
				(progn ;Is on the queue
					(unless (>= (aref node 2) (gethash (aref node 1) priorityFinder))
						(deleteFromQueue node)
						(push node (gethash (aref node 2) priorityQueue))
						(setf (gethash (aref node 1) priorityFinder) (aref node 2))
						(setq maximalPriority (max maximalPriority (aref node 2)))
					)
				)
				(progn ;Is not on the queue
					(push node (gethash (aref node 2) priorityQueue))
					(setf (gethash (aref node 1) priorityFinder) (aref node 2))
					(setq maximalPriority (max maximalPriority (aref node 2)))
				)
			)
		)
	)
)

;Obtiene el siguiente nodo a procesar en la frontera de busqueda
(defun getNextFromQueue ()
	(cond 
		((or (equal searchMethod :breadth-first-search) (equal searchMethod :depth-first-search))
			(return-from getNextFromQueue (pop queue))
		)
		((or (equal searchMethod :best-first-search) (equal searchMethod :a-star))
			(loop for score from 0 to maximalPriority by 1 do
				(unless (null (gethash score priorityQueue))
					(remhash (aref (first (gethash score priorityQueue)) 1) priorityFinder)
					(return-from getNextFromQueue (pop (gethash score priorityQueue)))
				)
			)
		)
	)
	NIL
)

;Regresa T/NIL si la frontera de busqueda esta vacía o no
(defun isQueueEmpty ()
	(cond 
		((or (equal searchMethod :breadth-first-search) (equal searchMethod :depth-first-search))
			(return-from isQueueEmpty (null queue))
		)
		((or (equal searchMethod :best-first-search) (equal searchMethod :a-star))
			(loop for score from 0 to maximalPriority by 1 do
				(unless (null (gethash score priorityQueue))
					(return-from isQueueEmpty NIL)
				)
			)
			(return-from isQueueEmpty T)
		)
	)
)

;Imprime toda la frontera de busqueda
(defun printQueue ()
	(cond 
		((or (equal searchMethod :breadth-first-search) (equal searchMethod :depth-first-search))
			(format t "Queue -> ~200A ~%" queue)
		)
		((or (equal searchMethod :best-first-search) (equal searchMethod :a-star))
			(loop for score from 0 to maximalPriority by 1 do
				(unless (null (gethash score priorityQueue))
					(format t "For score ~A the queue is -> ~100A ~%" score (gethash score priorityQueue))
				)
			)
		)
	)
)



; ==== Bloque de la memoria ===
(defparameter memory (make-hash-table :test #'equal))
(defparameter solutionPath '())

;Agrega un nodo a la memoria
(defun remember (node)
	(setf (gethash (aref node 1) memory) node)
)

;Regresa T/NIL si el nodo se encuentra en la memoria
(defun canRemember (state)
	(not (null (gethash state memory)))
)

;Regresa una lista de nodos por los que pasó para llegar a la solución
(defun rememberPath (state &optional (clear T))
	(if clear (setq solutionPath '()))
	(unless (null (gethash state memory))
		(push (gethash state memory) solutionPath)
		(unless (null (gethash (aref (gethash state memory) 5) memory))
			(rememberPath (aref (gethash state memory) 5) NIL)
		)
	)
	solutionPath
)



; ==== Bloque de las operaciones ===
(defparameter possibleOps '())

;Si la operaciones son dinámicas, usar esta función para generarlas
(defun addOps ()
	(setq possibleOps '())
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

		(loop for slot across mosBoard do
			(loop for hexagon in mosPieces do
				(loop for rotation from 0 to 5 by 1 do
					(push (list (first hexagon) rotation (first slot)) possibleOps)
				)
			)
		)
	)
)

;Dado un estado y una operación, determina si es válido aplicarla o no
(defun isOpValid (state op)
	(null (nth (- (third op) 1) state))
)

;Dado un estado regresa las operaciones puede (o le conviene) aplicar
(defun getOps (state)
	(let (
		(stateOps (copy-list possibleOps))
		)
		(loop for op in possibleOps do
			(setq stateOps 
				(remove-if #'(lambda (x) (not (isOpValid state x))) stateOps)
			)
		)
		stateOps
	)
)

;Dado un estado y una operación, crea un nuevo estado con la operación aplicada
(defun doOp (state op)
	(let (
		(newState (copy-list state))
		(pos (- (third op) 1))
		)
		(setf (nth pos newState) (butlast op))
		newState
	)
)



; ==== Bloque de los estados ===
(defparameter goalScore 0)
(defparameter goalState NIL)

;Dado un estado, determina si es válido bajo las reglas del problema
(defun isStateValid (state)
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

;Dado un estado, determina si es el estado meta
(defun isGoalState (state)
	(and (isFull state) (isStateValid state))
)

;Si el goalScore es dinámico o se requiere para cacular el stateScore, esta función lo genera
(defun getGoalScore ()
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
		(setq goalScore (list-length uniquePairs))
	)
)

;Determina la puntuación de un estado respecto al origen y la meta
(defun getStateScore (state cost)
	(cond
		((or (equal searchMethod :breadth-first-search) (equal searchMethod :depth-first-search))
			(return-from getStateScore 0)
		)
		((equal searchMethod :best-first-search)
			(setq cost 0)
		)
	)
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
		(+ cost (- goalScore (list-length uniquePairs)))
	)
)

;Dado un estado meta, genera la solucion como es requerida en el problema
(defun getRequiredSolution (state)
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



; ==== Bloque de funciones adicionales ===

;To-Do, Aqui se deben colocar todas las funciones adicionales que sean útiles
;		para llenar las funciones previas respecto al problema resuelto
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



; ==== Bloque de la búsqueda ===
(defun doSearch (initialState numberOfSolutions &optional (printSearch NIL))
	(let (
		(listOfSolutions '())
		(currentNumberOfSolutions 0)
		(currentNode NIL)
		(currentState NIL)
		(currentDeep 0)
		(nextNode NIL)
		(nextState NIL)
		(nextDeep 0)
		(flagToStop NIL)
		)

		;Insertamos el nodo inicial en la frontera de búsqueda
		;                       (state score operation deep parent)
		(addToQueue (createNode initialState 0 NIL currentDeep NIL)) 

		;Comenzamos con la búsqueda hasta que se vacíe la frontera de búsqueda, 
		;lleguemos al numero de soluciones o la bandera para parar esté arriba
		(loop until 
			(or (equal currentNumberOfSolutions numberOfSolutions)
				(isQueueEmpty) flagToStop) do

			;Asignamos los valores current
			(setq currentNode (getNextFromQueue))
			(setq currentState (aref currentNode 1))
			(setq currentDeep (aref currentNode 4))

			;Agregamos el nodo que vamos a procesar a la memoria
			(remember currentNode)

			;Si debemos imprimir, lo hacemos
			(if printSearch (format t "currentNode -> ~100A ~%" currentNode))

			(cond 
				;Si el estado actual es una solución, la contamos
				((isGoalState currentState)
					(if printSearch (format t "Solution on node -> ~100A ~%" currentNode))
					(setq numberOfSolutions (+ 1 numberOfSolutions))
					(push (getRequiredSolution currentState) listOfSolutions)
				)
				
				;Si el estado actual no es solución, entonces seguimos buscando
				(t 
					;(if printSearch (format t "Applying ops -> ~100A ~%" (getOps currentState)))
					(loop for op in (getOps currentState) do
						(setq nextState (doOp currentState op))
						;(if printSearch (format t "   nextState -> ~100A" nextState))
						(unless (not (and (not (canRemember nextState)) (isStateValid nextState)))
							;(if printSearch (format t " isValid!!"))
							(setq nextDeep (+ 1 currentDeep))
							(setq nextNode (createNode nextState (getStateScore nextState nextDeep) op nextDeep currentState))
							(addToQueue nextNode)
						)
						;(if printSearch (format t "~%"))
					)
				)

			)
		)
		listOfSolutions
	)
)

; === Operaciones Previas a la Búsqueda ===
(defparameter start NIL)
(defparameter end NIL)

(defun cleanData ()
	(setq searchMethod NIL)
	(setq nodeId 0)
	(setq queue NIL)
	(setq priorityQueue (make-hash-table :test #'equal))
	(setq maximalPriority 0)
	(setq memory (make-hash-table :test #'equal))
	(setq goalScore 0)
	(setq goalState NIL)
)

(defun callSearch (methodToUse initialState finalState)
	(cleanData)
	(setq searchMethod methodToUse)
	(setq mosBoard (get-board))
	(setq mosPieces (get-pieces))
	(addOps)
	(getGoalScore)
	(loop for slot across mosBoard do
		(push NIL initialState)
	)
	(setq goalState finalState)
	(doSearch initialState 1 T)
)

(defun depth-first-search ()
	(callSearch :depth-first-search start end)
)

(defun breadth-first-search ()
	(callSearch :breadth-first-search start end)
)

(defun best-first-search ()
	(callSearch :best-first-search start end)
)

(defun a-star ()
	(callSearch :a-star start end)
)

(print (a-star))