#!/usr/bin/sbcl --script

; ==== Definiciones Importantes ====
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
					(return-from isQueueEmpty T)
				)
			)
			(return-from isQueueEmpty NIL)
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
	(null (gethash state memory))
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
	;To-Do
)

;Dado un estado y una operación, determina si es válido aplicarla o no
(defun isOpValid (state op)
	;To-Do
)

;Dado un estado regresa las operaciones puede (o le conviene) aplicar
(defun getOps (state)
	;To-Do
)

;Dado un estado y una operación, crea un nuevo estado con la operación aplicada
(defun doOp (state op)
	;To-Do
)



; ==== Bloque de los estados ===
(defparameter goalScore 0)
(defparameter goalState NIL)

;Dado un estado, determina si es válido bajo las reglas del problema
(defun isStateValid (state)
	;To-Do
)

;Dado un estado, determina si es el estado meta
(defun isGoalState (state)
	;To-Do
)

;Si el goalScore es dinámico o se requiere para cacular el stateScore, esta función lo genera
(defun getGoalScore ()
	;To-Do
)

;Determina la puntuación de un estado respecto al origen y la meta
(defun getStateScore (state cost)
	;To-Do
)

;Dado un estado meta, genera la solucion como es requerida en el problema
(defun getRequiredSolution (state)
	;To-Do
)



; ==== Bloque de funciones adicionales ===

;To-Do, Aqui se deben colocar todas las funciones adicionales que sean útiles
;		para llenar las funciones previas respecto al problema resuelto




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
					(setq numberOfSolutions (+ 1 numberOfSolutions))
					(push (getRequiredSolution currentState) listOfSolutions)
				)
				
				;Si el estado actual no es solución, entonces seguimos buscando
				(t 
					(loop for op in (getOps currentState) do
						(setq nextState (doOp currentState op))
						(unless (or (canRemember nextState) (not (isStateValid nextState)))
							(setq nextDeep (+ 1 currentDeep))
							(setq nextNode (createNode nextState (getStateScore nextState nextDeep) op nextDeep currentState))
							(addToQueue nextNode)
						)
					)
				)

			)
		)
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
	(setq possibleOps NIL)
	(setq goalScore 0)
	(setq goalState NIL)
)

(defun callSearch (methodToUse initialState)
	(cleanData)
	(setq searchMethod methodToUse)
	(addOps)
	(getGoalScore)
	(doSearch initialState 1)
)

(defun depth-first-search ()
	(callSearch :depth-first-search start)
)

(defun breadth-first-search ()
	(callSearch :breadth-first-search start)
)

(defun best-first-search ()
	(callSearch :best-first-search start)
)

(defun a-star ()
	(callSearch :a-star start)
)
