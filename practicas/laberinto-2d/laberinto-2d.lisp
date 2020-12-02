#!/usr/bin/sbcl --script

;; ===============================
; Ejercicio "Laberintos 2D"
; Alumna: Enya Quetzalli Gómez Rodríguez (Eduardo Gómez Rodríguez)
; Boleta: 2014030529
; Profesor: Dr. Salvador Godoy
; Escuela: Centro de Investigación de Cómputo - IPN
; Semestre: 2021/1
;; ===============================

(load "maze_lib.lisp")

; ==== Definiciones Importantes ====
(add-algorithm 'depth-first-search)
(add-algorithm 'breadth-first-search)
(add-algorithm 'best-first-search)
(add-algorithm 'a-star)
(defparameter searchMethod NIL)


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
				(if (and 
					(equal (aref (aref node 1) 0) (aref (aref element 1) 0))
					(equal (aref (aref node 1) 1) (aref (aref element 1) 1)))
					(return-from isOnQueue T)
				)
			)
			(return-from isOnQueue NIL)
		)

		((or (equal searchMethod :best-first-search) (equal searchMethod :a-star))
			(if (null (gethash (getStateId (aref node 1)) priorityFinder))
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
				(gethash (gethash (getStateId (aref node 1)) priorityFinder) priorityQueue) 
				(remove-if #'(lambda (x) (and (equal (aref (aref x 1) 0) (aref (aref node 1) 0)) (equal (aref (aref x 1) 1) (aref (aref node 1) 1)))) 
					(gethash (gethash (getStateId (aref node 1)) priorityFinder) priorityQueue)))
			(remhash (getStateId (aref node 1)) priorityFinder)
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
		((equal searchMethod :best-first-search)
			(unless (isOnQueue node)
				(push node (gethash (aref node 2) priorityQueue))
				(setf (gethash (getStateId (aref node 1)) priorityFinder) (aref node 2))
				(setq maximalPriority (max maximalPriority (aref node 2)))
			)
		)
		((equal searchMethod :a-star)
			(if (isOnQueue node)
				(progn ;Is on the queue
					(unless (>= (aref node 2) (gethash (getStateId (aref node 1)) priorityFinder))
						(deleteFromQueue node)
						(push node (gethash (aref node 2) priorityQueue))
						(setf (gethash (getStateId (aref node 1)) priorityFinder) (aref node 2))
						(setq maximalPriority (max maximalPriority (aref node 2)))
					)
				)
				(progn ;Is not on the queue
					(push node (gethash (aref node 2) priorityQueue))
					(setf (gethash (getStateId (aref node 1)) priorityFinder) (aref node 2))
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
					(remhash (getStateId (aref (first (gethash score priorityQueue)) 1)) priorityFinder)
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
	(setf (gethash (getStateId (aref node 1)) memory) node)
)

;Regresa T/NIL si el nodo se encuentra en la memoria
(defun canRemember (state)
	(not (null (gethash (getStateId state) memory)))
)

;Regresa una lista de nodos por los que pasó para llegar a la solución
(defun rememberPath (state &optional (clear T))
	(if clear (setq solutionPath '()))
	(unless (null state)
		(unless (null (gethash (getStateId state) memory))
			(push (gethash (getStateId state) memory) solutionPath)
			(unless (null (aref (gethash (getStateId state) memory) 5))
				(unless (null (gethash (getStateId (aref (gethash (getStateId state) memory) 5)) memory))
					(rememberPath (aref (gethash (getStateId state) memory) 5) NIL)
				)
			)
		)
	)
	solutionPath
)



; ==== Bloque de las operaciones ===
(defparameter possibleOps '((:arriba           0 )
							(:arriba-derecha   1 )
							(:derecha          2 )
							(:abajo-derecha    3 )
							(:abajo            4 )
							(:abajo-izquierda  5 )
							(:izquierda        6 )
							(:arriba-izquierda 7 )))

;Si la operaciones son dinámicas, usar esta función para generarlas
(defun addOps ()
	;To-Do
)

;Dado un estado y una operación, determina si es válido aplicarla o no
(defun isOpValid (state op)
	(let (
		(pos state)
		(newPos (make-array '(2)))
		(interA (make-array '(2)))
		(interB (make-array '(2)))
		(wall (get-cell-walls (aref state 1) (aref state 0)))
		(newWall NIL)
		(interWallA NIL)
		(interWallB NIL)
		(arriba-bloqueado '(1 3 5 7 9 11 13 15))
		(derecha-bloqueado '(2 3 6 7 10 11 14 15))
		(abajo-bloqueado '(4 5 6 7 12 13 14 15))
		(izquierda-bloqueado '(8 9 10 11 12 13 14 15))
		)
		;(format t "On state ~A the wall is ~A ~%" state wall)
		(case (first op)
			(:arriba
				(setf (aref newPos 0) (+ 0 (aref pos 0)))
				(setf (aref newPos 1) (+ -1 (aref pos 1)))
				(setq newWall (if (isStateValid newPos) (get-cell-walls (aref newPos 1) (aref newPos 0)) 15))
				
				(cond 
					((not (isStateValid newPos))
						NIL
						)
					((member wall arriba-bloqueado)
						NIL
						)
					((member newWall abajo-bloqueado)
						NIL
						)
					(t T)
				)
			)
			(:arriba-derecha
				(setf (aref interA 0) (+ 0 (aref pos 0)))
				(setf (aref interA 1) (+ -1 (aref pos 1)))
				(setq interWallA (if (isStateValid interA) (get-cell-walls (aref interA 1) (aref interA 0)) 15))

				(setf (aref interB 0) (+ 1 (aref pos 0)))
				(setf (aref interB 1) (+ 0 (aref pos 1)))
				(setq interWallB (if (isStateValid interB) (get-cell-walls (aref interB 1) (aref interB 0)) 15))
			
				(setf (aref newPos 0) (+ 1 (aref pos 0)))
				(setf (aref newPos 1) (+ -1 (aref pos 1)))
				(setq newWall (if (isStateValid newPos) (get-cell-walls (aref newPos 1) (aref newPos 0)) 15))
				

				(cond 
					((not (isStateValid newPos))
						NIL
						)
					((cond 
						((member wall arriba-bloqueado)
							NIL
							)
						((or (member interWallA abajo-bloqueado) (member interWallA derecha-bloqueado))
							NIL
							)
						((member newWall izquierda-bloqueado)
							NIL
							)
						(t T))
						T
					)
					((cond 
						((member wall derecha-bloqueado)
							NIL
							)
						((or (member interWallB izquierda-bloqueado) (member interWallB arriba-bloqueado))
							NIL
							)
						((member newWall abajo-bloqueado)
							NIL
							)
						(t T))
						T
					)
					(t NIL)
				)
				
			)

			(:derecha
				(setf (aref newPos 0) (+ 1 (aref pos 0)))
				(setf (aref newPos 1) (+ 0 (aref pos 1)))
				(setq newWall (if (isStateValid newPos) (get-cell-walls (aref newPos 1) (aref newPos 0)) 15))
			
				(cond 
					((not (isStateValid newPos))
						NIL
						)
					((member wall derecha-bloqueado)
						NIL
						)
					((member newWall izquierda-bloqueado)
						NIL
						)
					(t T)
				)
			)

			(:abajo-derecha
				(setf (aref interA 0) (+ 1 (aref pos 0)))
				(setf (aref interA 1) (+ 0 (aref pos 1)))
				(setq interWallA (if (isStateValid interA) (get-cell-walls (aref interA 1) (aref interA 0)) 15))

				(setf (aref interB 0) (+ 0 (aref pos 0)))
				(setf (aref interB 1) (+ 1 (aref pos 1)))
				(setq interWallB (if (isStateValid interB) (get-cell-walls (aref interB 1) (aref interB 0)) 15))
			
				(setf (aref newPos 0) (+ 1 (aref pos 0)))
				(setf (aref newPos 1) (+ 1 (aref pos 1)))
				(setq newWall (if (isStateValid newPos) (get-cell-walls (aref newPos 1) (aref newPos 0)) 15))
				

				(cond 
					((not (isStateValid newPos))
						NIL
						)
					((cond 
						((member wall derecha-bloqueado)
							NIL
							)
						((or (member interWallA izquierda-bloqueado) (member interWallA abajo-bloqueado))
							NIL
							)
						((member newWall arriba-bloqueado)
							NIL
							)
						(t T))
						T
					)
					((cond 
						((member wall abajo-bloqueado)
							NIL
							)
						((or (member interWallB arriba-bloqueado) (member interWallB derecha-bloqueado))
							NIL
							)
						((member newWall izquierda-bloqueado)
							NIL
							)
						(t T))
						T
					)
					(t NIL)
				)
			)

			(:abajo
				(setf (aref newPos 0) (+ 0 (aref pos 0)))
				(setf (aref newPos 1) (+ 1 (aref pos 1)))
				(setq newWall (if (isStateValid newPos) (get-cell-walls (aref newPos 1) (aref newPos 0)) 15))
			
				(cond 
					((not (isStateValid newPos))
						NIL
						)
					((member wall abajo-bloqueado)
						NIL
						)
					((member newWall arriba-bloqueado)
						NIL
						)
					(t T)
				)
			)

			(:abajo-izquierda
				(setf (aref interA 0) (+ -1 (aref pos 0)))
				(setf (aref interA 1) (+ 0 (aref pos 1)))
				(setq interWallA (if (isStateValid interA) (get-cell-walls (aref interA 1) (aref interA 0)) 15))

				(setf (aref interB 0) (+ 0 (aref pos 0)))
				(setf (aref interB 1) (+ 1 (aref pos 1)))
				(setq interWallB (if (isStateValid interB) (get-cell-walls (aref interB 1) (aref interB 0)) 15))
			
				(setf (aref newPos 0) (+ -1 (aref pos 0)))
				(setf (aref newPos 1) (+ 1 (aref pos 1)))
				(setq newWall (if (isStateValid newPos) (get-cell-walls (aref newPos 1) (aref newPos 0)) 15))
				

				(cond 
					((not (isStateValid newPos))
						NIL
						)
					((cond 
						((member wall izquierda-bloqueado)
							NIL
							)
						((or (member interWallA derecha-bloqueado) (member interWallA abajo-bloqueado))
							NIL
							)
						((member newWall arriba-bloqueado)
							NIL
							)
						(t T))
						T
					)
					((cond 
						((member wall abajo-bloqueado)
							NIL
							)
						((or (member interWallB arriba-bloqueado) (member interWallB izquierda-bloqueado))
							NIL
							)
						((member newWall derecha-bloqueado)
							NIL
							)
						(t T))
						T
					)
					(t NIL)
				)
			)

			(:izquierda
				(setf (aref newPos 0) (+ -1 (aref pos 0)))
				(setf (aref newPos 1) (+ 0 (aref pos 1)))
				(setq newWall (if (isStateValid newPos) (get-cell-walls (aref newPos 1) (aref newPos 0)) 15))
			
				(cond 
					((not (isStateValid newPos))
						NIL
						)
					((member wall izquierda-bloqueado)
						NIL
						)
					((member newWall derecha-bloqueado)
						NIL
						)
					(t T)
				)
			)

			(:arriba-izquierda
				(setf (aref interA 0) (+ 0 (aref pos 0)))
				(setf (aref interA 1) (+ -1 (aref pos 1)))
				(setq interWallA (if (isStateValid interA) (get-cell-walls (aref interA 1) (aref interA 0)) 15))

				(setf (aref interB 0) (+ -1 (aref pos 0)))
				(setf (aref interB 1) (+ 0 (aref pos 1)))
				(setq interWallB (if (isStateValid interB) (get-cell-walls (aref interB 1) (aref interB 0)) 15))
			
				(setf (aref newPos 0) (+ -1 (aref pos 0)))
				(setf (aref newPos 1) (+ -1 (aref pos 1)))
				(setq newWall (if (isStateValid newPos) (get-cell-walls (aref newPos 1) (aref newPos 0)) 15))
				

				(cond 
					((not (isStateValid newPos))
						NIL
						)
					((cond 
						((member wall arriba-bloqueado)
							NIL
							)
						((or (member interWallA abajo-bloqueado) (member interWallA izquierda-bloqueado))
							NIL
							)
						((member newWall derecha-bloqueado)
							NIL
							)
						(t T))
						T
					)
					((cond 
						((member wall izquierda-bloqueado)
							NIL
							)
						((or (member interWallB derecha-bloqueado) (member interWallB arriba-bloqueado))
							NIL
							)
						((member newWall abajo-bloqueado)
							NIL
							)
						(t T))
						T
					)
					(t NIL)
				)
			)

		)
	)
)

;Dado un estado regresa las operaciones puede (o le conviene) aplicar
(defun getOps (state)
	(let (
		(ops '())
		)
		(loop for op in possibleOps do
			(if (isOpValid state op)
				(push op ops)
			)
		)
		ops
	)
)

;Dado un estado y una operación, crea un nuevo estado con la operación aplicada
(defun doOp (state op)
	(let (
		(pos state)
		(newPos (make-array '(2)))
		)
		(case (first op)
			(:arriba
				(setf (aref newPos 0) (+ 0 (aref pos 0)))
				(setf (aref newPos 1) (+ -1 (aref pos 1)))
			)
			(:arriba-derecha		
				(setf (aref newPos 0) (+ 1 (aref pos 0)))
				(setf (aref newPos 1) (+ -1 (aref pos 1)))
			)
			(:derecha
				(setf (aref newPos 0) (+ 1 (aref pos 0)))
				(setf (aref newPos 1) (+ 0 (aref pos 1)))
			)
			(:abajo-derecha
				(setf (aref newPos 0) (+ 1 (aref pos 0)))
				(setf (aref newPos 1) (+ 1 (aref pos 1)))
			)
			(:abajo
				(setf (aref newPos 0) (+ 0 (aref pos 0)))
				(setf (aref newPos 1) (+ 1 (aref pos 1)))
			)
			(:abajo-izquierda
				(setf (aref newPos 0) (+ -1 (aref pos 0)))
				(setf (aref newPos 1) (+ 1 (aref pos 1)))
			)
			(:izquierda
				(setf (aref newPos 0) (+ -1 (aref pos 0)))
				(setf (aref newPos 1) (+ 0 (aref pos 1)))
			)
			(:arriba-izquierda
				(setf (aref newPos 0) (+ -1 (aref pos 0)))
				(setf (aref newPos 1) (+ -1 (aref pos 1)))
			)
		)
		newPos
	)
)



; ==== Bloque de los estados ===
(defparameter goalScore 0)
(defparameter goalState NIL)

;Dado un estado, determina si es válido bajo las reglas del problema
(defun isStateValid (state)
	(let (
		(cols (get-maze-cols))
		(rows (get-maze-rows))
		)
		(and (>= (aref state 0) 0) (< (aref state 0) cols)
			 (>= (aref state 1) 0) (< (aref state 1) rows)
		)
	)
)

;Dado un estado, determina si es el estado meta
(defun isGoalState (state)
	(and (equal (aref state 0) (aref goalState 0)) (equal (aref state 1) (aref goalState 1)))
)

;Si el goalScore es dinámico o se requiere para cacular el stateScore, esta función lo genera
(defun getGoalScore ()
	;To-Do
)

;Determina la puntuación de un estado respecto al origen y la meta
(defun getStateScore (state cost)
	(cond 
		((or (equal searchMethod :breadth-first-search) (equal searchMethod :depth-first-search))
			0.0
		)
		((equal searchMethod :best-first-search)
			(euclidean state)
		)
		((equal searchMethod :a-star)
			(+ cost (euclidean state))
		)
	)
)

;Dado un estado meta, genera la solucion como es requerida en el problema
(defun getRequiredSolution (state)
	(let (
		(requiredPath '())
		(solution (rememberPath state))
		)

		(loop for node in solution do 
			(unless (null (aref node 3))
				(push (second (aref node 3)) requiredPath)
			)
		)
		(nreverse requiredPath)
	)
)



; ==== Bloque de funciones adicionales ===

;To-Do, Aqui se deben colocar todas las funciones adicionales que sean útiles
;		para llenar las funciones previas respecto al problema resuelto
(defun manhattan (state)
	(+ (abs (- (aref state 0) (aref goalState 0))) (abs (- (aref state 1) (aref goalState 1))))
)

(defun euclidean (state)
	(floor (sqrt (+
		(* (- (aref state 0) (aref goalState 0)) (- (aref state 0) (aref goalState 0)))
		(* (- (aref state 1) (aref goalState 1)) (- (aref state 1) (aref goalState 1)))
		)))
)

(defun  getStateId (state)
  (+ (aref state 1) (* (aref state 0) (+ 1 (get-maze-rows)))))


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
		(solutionState NIL)
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
			;(printQueue)

			(cond 
				;Si el estado actual es una solución, la contamos
				((isGoalState currentState)
					(if printSearch (format t "Solution on node -> ~100A ~%" currentNode))
					(setq currentNumberOfSolutions (+ 1 currentNumberOfSolutions))
					(setq solutionState currentState)
					(push (getRequiredSolution currentState) listOfSolutions)
				)
				
				;Si el estado actual no es solución, entonces seguimos buscando
				(t 
					;(if printSearch (format t "Applying ops -> ~100A ~%" (getOps currentState)))
					(loop for op in (getOps currentState) do
						(setq nextState (doOp currentState op))
						(if printSearch (format t "   nextState -> ~10A, Can remember? ~A " nextState (canRemember nextState)))
						(unless (not (and (not (canRemember nextState)) (isStateValid nextState)))
							(if printSearch (format t " isValid!!"))
							(setq nextDeep (+ 1 currentDeep))
							(setq nextNode (createNode nextState (getStateScore nextState nextDeep) op nextDeep currentState))
							(addToQueue nextNode)
						)
						(if printSearch (format t "~%"))
					)
				)

			)
		)
		(if printSearch (format t "Search ended, reason? ~%  Number Of Solutions ~A, required ~A ~%  isQueueEmpty? ~A ~% flagToStop? ~A ~%"
			currentNumberOfSolutions numberOfSolutions (isQueueEmpty) flagToStop))
		(if printSearch (printQueue))
		listOfSolutions
	)
)

; === Operaciones Previas a la Búsqueda ===
;(defparameter start NIL)
;(defparameter end NIL)

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
	(addOps)
	(getGoalScore)
	(rotatef (aref initialState 0) (aref initialState 1))
	(rotatef (aref finalState 0) (aref finalState 1))
	(format t "Start = ~A, Goal = ~A ~%" initialState finalState)
	(format t "Cols = ~A, Rows = ~A ~%" (get-maze-cols) (get-maze-rows))
	(format t "Method = ~A ~%" methodToUse)
	(format t "Method = ~A ~%" searchMethod)
	(setq goalState finalState)
	(setq *solution* (first (doSearch initialState 1)))
)

(defun depth-first-search ()
	(callSearch :depth-first-search *start* *goal*)
)

(defun breadth-first-search ()
	(callSearch :breadth-first-search *start* *goal*)
)

(defun best-first-search ()
	(callSearch :best-first-search *start* *goal*)
)

(defun a-star ()
	(callSearch :a-star *start* *goal*)
)

;(print (depth-first-search))
(start-maze)