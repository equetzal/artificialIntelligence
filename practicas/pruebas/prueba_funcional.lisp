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
(defparameter possibleOps '(
	(:viaja-vaca 		A)
	(:viaja-lobo 		O)
	(:viaja-legumbre 	E)
	(:viaja-solo		S)
	)
)

;Si la operaciones son dinámicas, usar esta función para generarlas
(defun addOps ()
)

;Dado un estado y una operación, determina si es válido aplicarla o no
(defun isOpValid (state op)
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



; ==== Bloque de los estados ===
(defparameter goalScore 0)
(defparameter goalState NIL)

;Dado un estado, determina si es válido bajo las reglas del problema
(defun isStateValid (state)
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

;Dado un estado, determina si es el estado meta
(defun isGoalState (state)
	(equal state goalState)
)

;Si el goalScore es dinámico o se requiere para cacular el stateScore, esta función lo genera
(defun getGoalScore ()
	;To-Do
)

;Determina la puntuación de un estado respecto al origen y la meta
(defun getStateScore (state cost)
	0
)

;Dado un estado meta, genera la solucion como es requerida en el problema
(defun getRequiredSolution (state)
	(rememberPath state)
)



; ==== Bloque de funciones adicionales ===

;To-Do, Aqui se deben colocar todas las funciones adicionales que sean útiles
;		para llenar las funciones previas respecto al problema resuelto

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
					(if printSearch (format t "Applying ops -> ~100A ~%" (getOps currentState)))
					(loop for op in (getOps currentState) do
						(setq nextState (doOp currentState op))
						(if printSearch (format t "   nextState -> ~100A" nextState))
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
	(addOps)
	(getGoalScore)
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

(setq start '((1 1 1 1) (0 0 0 0)))
(setq end '((0 0 0 0) (1 1 1 1)))
(print (a-star))
(printQueue)
(print (isQueueEmpty))