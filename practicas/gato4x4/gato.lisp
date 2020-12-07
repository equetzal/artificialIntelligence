; #!/usr/bin/sbcl --script

;; ===============================
; Ejercicio "Laberintos 2D"
; Alumna: Enya Quetzalli Gómez Rodríguez (Eduardo Gómez Rodríguez)
; Boleta: 2014030529
; Profesor: Dr. Salvador Godoy
; Escuela: Centro de Investigación de Cómputo - IPN
; Semestre: 2021/1
;; ===============================


;Dado un tablero de juego y el juegador que está jugando, determina cuál es la puntuación
; Puntuaciones por linea/diagonal libre con:
;	0		puntos:	0 elementos 
;	8		puntos:	1 elemento
;	64		puntos:	2 elementos
;	512		puntos:	3 elementos
;	4096	puntos: 4 elementos
(defun getPlayerScore (state player) 
	(let (
		(score 0)
		(cnt1 0)
		(cnt2 0)
		(flag1 NIL)
		(flag2 NIL)
		)

		;Revisa filas y columnas
		(loop for i from 0 to 3 by 1 do
			(loop for j from 0 to 3 by 1 do
				(if (equal (nth j (nth i state)) player) 
					(setq cnt1 (+ 1 cnt1))
					(unless (null (nth j (nth i state)))
						(setq flag1 T)
					)
				)
				(if (equal (nth i (nth j state)) player) 
					(setq cnt2 (+ 1 cnt2))
					(unless (null (nth i (nth j state)))
						(setq flag2 T)
					)
				)
			)
			(unless flag1
				(case cnt1
					(1 (setq score (+ 8 score)))
					(2 (setq score (+ 64 score)))
					(3 (setq score (+ 512 score)))
					(4 (setq score (+ 4096 score)))
				)
			)
			(unless flag2
				(case cnt2
					(1 (setq score (+ 8 score)))
					(2 (setq score (+ 64 score)))
					(3 (setq score (+ 512 score)))
					(4 (setq score (+ 4096 score)))
				)
			)
			(setq cnt1 0)
			(setq cnt2 0)
			(setq flag1 NIL)
			(setq flag2 NIL)
		)

		(setq cnt1 0)
		(setq cnt2 0)
		(setq flag1 NIL)
		(setq flag2 NIL)

		;Revisa diagonales
		(loop for i from 0 to 3 by 1 do
			(if (equal (nth i (nth i state)) player) 
				(setq cnt1 (+ 1 cnt1))
				(unless (null (nth i (nth i state)))
					(setq flag1 T)
				)
			)
			(if (equal (nth i (nth (- 3 i) state)) player) 
				(setq cnt2 (+ 1 cnt2))
				(unless (null (nth i (nth (- 3 i) state)))
					(setq flag2 T)
				)
			)
		)
		(unless flag1
			(case cnt1
				(1 (setq score (+ 8 score)))
				(2 (setq score (+ 64 score)))
				(3 (setq score (+ 512 score)))
				(4 (setq score (+ 4096 score)))
			)
		)
		(unless flag2
			(case cnt2
				(1 (setq score (+ 8 score)))
				(2 (setq score (+ 64 score)))
				(3 (setq score (+ 512 score)))
				(4 (setq score (+ 4096 score)))
			)
		)

		;(format t "state -> ~A, score = ~A ~%" state score)

		score
	)
)

;Regresa la puntuación f(e) = G - P 
(defun getStateScore (state)
	(- (getPlayerScore state 'O) (getPlayerScore state 'X))
)

;Regresa T/NIL si el jugador ya ganó o aún no
(defun wonPlayer (state player)
	;Dado que las puntuaciones llevan una escala de 8*marcasColocadas, 
	;nunca es posible obtener 4096 de otra forma que no sea teniendo 4
	;en una misma fila/columna/diagonal
	(>= (getPlayerScore state player) 4096)
)

;Regresa T/NIL si algún jugador ya ganó
(defun isGameOver (state)
	(or (wonPlayer state 'O) (wonPlayer state 'X))
)


; ==== Bloque de las operaciones ===
(defparameter possibleOps '(
	;x, y, idCasilla
	(1	1	6)
	(1	2	7)
	(2	1	10)
	(2	2	11)
	(0	0	1)
	(0	3	4)
	(3	0	13)
	(3	3	16)
	(0	1	2)
	(0	2	3)
	(1	0	5)
	(1	3	8)
	(2	0	9)
	(2	3	12)
	(3	1	14)
	(3	2	15)
	))

;Si la operaciones son dinámicas, usar esta función para generarlas
(defun addOps ()
	;To-Do
)

;Dado un estado y una operación, determina si es válido aplicarla o no
(defun isOpValid (state op)
	(null (nth (second op) (nth (first op) state)))
)

;Dado un estado regresa las operaciones puede (o le conviene) aplicar
(defun getOps (state)
	(remove-if 
		#'(lambda (op) 
			(not (isOpValid state op))
		) 
	possibleOps)
)

;Dado un estado y una operación, crea un nuevo estado con la operación aplicada
(defun doOp (state op player)
	(let (
		(newState (copy-tree state))
		)
		(setf (nth (second op) (nth (first op) newState)) player)
		newState
	)
)

(defun miniMax (state depth maxDepth player)
	(if (or (isGameOver state) (equal depth maxDepth))
		(return-from miniMax (list (getStateScore state) NIL))
	)

	(let (
		(bestOp NIL)
		(bestVal (if (equal player 'O) -32768 32768))
		(result NIL)
		(currentVal -32768)
		)

		(loop for op in (getOps state) do
			(setq result (miniMax (doOp state op player) (+ 1 depth) maxDepth (if (equal player 'O) 'X 'O)))
			; (format t "Current State -> ~30A ~%" state)
			; (format t "Applying op=~A, next state is ~A, score = ~A ~%" op (doOp state op 'O) (first result))
			(setq currentVal (first result))

			(if (equal player 'O)
				(if (> currentVal bestVal)
					(progn
						; (format t "On state ~A, Op ~7A was the !best ~%" state op)
						(setq bestVal currentVal)
						(setq bestOp op)
					)
				)
				(if (< currentVal bestVal)
					(progn
						; (format t "On state ~A, Op ~7A was the !best ~%" state op)
						(setq bestVal currentVal)
						(setq bestOp op)
					)
				)
			)
		)

		(list bestVal bestOp)
	)
)

; (defvar *output* NIL)

; (defvar someState '(
; 	(NIL X O O)
; 	(NIL  X  NIL NIL)
; 	(NIL NIL NIL NIL)
; 	(NIL X NIL NIL)
; 	))
; (print (tictactoe someState))

(defun tictactoe (state)
	(setq *output*
		(third (second (miniMax state 0 2 'O)))
	)
)