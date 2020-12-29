#!/usr/bin/sbcl --script

;; ===============================
; Ejercicio "Metagato"
; Alumna: Enya Quetzalli Gómez Rodríguez (Eduardo Gómez Rodríguez)
; Boleta: 2014030529
; Profesor: Dr. Salvador Godoy
; Escuela: Centro de Investigación de Cómputo - IPN
; Semestre: 2021/1
;; ===============================

;Representación:
; tipo-dato puede ser celda o gato
; valor contiene NIL para nada, X para cruz u O para circulo
; (:tipo-dato :valor :contenido) 

(defvar mapaGato '(
		((:gato X (
			((:celda X  ) (:celda O  ) (:celda NIL))
			((:celda NIL) (:celda NIL) (:celda NIL))
			((:celda NIL) (:celda NIL) (:celda NIL))
			)
		)
		(:gato X (
			((:celda X  ) (:celda O  ) (:celda NIL))
			((:celda NIL) (:celda NIL) (:celda NIL))
			((:celda NIL) (:celda NIL) (:celda NIL))
			)
		)
		(:gato X (
			((:celda X  ) (:celda O  ) (:celda NIL))
			((:celda NIL) (:celda NIL) (:celda NIL))
			((:celda NIL) (:celda NIL) (:celda NIL))
			)
		))

		((:gato NIL (
			((:celda X  ) (:celda O  ) (:celda NIL))
			((:celda NIL) (:celda NIL) (:celda NIL))
			((:celda NIL) (:celda NIL) (:celda NIL))
			)
		)
		(:gato NIL (
			((:celda X  ) (:celda O  ) (:celda NIL))
			((:celda NIL) (:celda NIL) (:celda NIL))
			((:celda NIL) (:celda NIL) (:celda NIL))
			)
		)
		(:gato NIL (
			((:celda X  ) (:celda O  ) (:celda NIL))
			((:celda NIL) (:celda NIL) (:celda NIL))
			((:celda NIL) (:celda NIL) (:celda NIL))
			)
		))

		((:gato NIL (
			((:celda X  ) (:celda O  ) (:celda NIL))
			((:celda NIL) (:celda NIL) (:celda NIL))
			((:celda NIL) (:celda NIL) (:celda NIL))
			)
		)
		(:gato NIL (
			((:celda X  ) (:celda O  ) (:celda NIL))
			((:celda NIL) (:celda NIL) (:celda NIL))
			((:celda NIL) (:celda NIL) (:celda NIL))
			)
		)
		(:gato NIL (
			((:celda X  ) (:celda O  ) (:celda NIL))
			((:celda NIL) (:celda NIL) (:celda NIL))
			((:celda NIL) (:celda NIL) (:celda NIL))
			)
		))
	)
)

(defun printChar (char-num color)
	(format t "~A ~%" (code-char (if (> char-num 126) (- char-num 255) (char-num))))
)

(defun printMetagato2 (mapa)
	;(printChar 201 blue)
	(format t "~a~a~a~a~a~a~a~a~a~a~%" (code-char 9556) (code-char 9552) (code-char 9552) (code-char 9552) (code-char 9552) (code-char 9552) (code-char 9552) (code-char 9552) (code-char 9552) (code-char 9574))
	(format t "~a~a~a~a~a~a~a~a~%" (code-char 9553) (code-char 32) (code-char 32) 'a (code-char 32) 'b (code-char 32) 'c)
	(format t "~a~a~a~a~a~a~a~a~a~%" (code-char 9553) (code-char 32) (code-char 9484) (code-char 9472) (code-char 9516) (code-char 9472) (code-char 9516) (code-char 9472) (code-char 9488))
	(format t "~a~a~a~a~a~a~a~a~a~%" (code-char 9553) '1 (code-char 9474) 'x (code-char 9474) 'x (code-char 9474) 'x (code-char 9474))
	(format t "~a~a~a~a~a~a~a~a~a~%" (code-char 9553) (code-char 32) (code-char 9500) (code-char 9472) (code-char 9532) (code-char 9472) (code-char 9532) (code-char 9472) (code-char 9508))
	(format t "~a~a~a~a~a~a~a~a~a~%" (code-char 9553) '2 (code-char 9474) 'x (code-char 9474) 'x (code-char 9474) 'x (code-char 9474))
	(format t "~a~a~a~a~a~a~a~a~a~%" (code-char 9553) (code-char 32) (code-char 9500) (code-char 9472) (code-char 9532) (code-char 9472) (code-char 9532) (code-char 9472) (code-char 9508))
	(format t "~a~a~a~a~a~a~a~a~a~%" (code-char 9553) '3 (code-char 9474) 'x (code-char 9474) 'x (code-char 9474) 'x (code-char 9474))
	(format t "~a~a~a~a~a~a~a~a~a~%" (code-char 9553) (code-char 32) (code-char 9492) (code-char 9472) (code-char 9524) (code-char 9472) (code-char 9524) (code-char 9472) (code-char 9496))
)

(defun setFrameBoard (board)
	(loop for i from 0 to 31 do
		(setf (aref board 0 i) (code-char 9552))
	)
	(loop for i from 0 to 29 do
		(setf (aref board i 0) (code-char 9553))
		(setf (aref board 0 i) (code-char 9552))
	)

	;Lineas
	(setf (aref board 1 31) (code-char 9553))
	(setf (aref board 0 31) (code-char 9559))
	(setf (aref board 0 0) (code-char 9556))
	(setf (aref board 29 0) (code-char 9562))
	(setf (aref board 29 1) (code-char 9552))
	(setf (aref board 29 2) (code-char 9552))

	;Letras
	(setf (aref board 1 9) 'A)
	(setf (aref board 1 18) 'B)
	(setf (aref board 1 27) 'C)

	;Numeros
	(setf (aref board 7 2) '1)
	(setf (aref board 16 2) '2)
	(setf (aref board 25 2) '3)
)

(defun setBigBoard (board x y)
	;Draw lines
	(loop for i from 0 to 27 do
		(setf (aref board (+ x 0) (+ y i)) (code-char 9552))
		(setf (aref board (+ x 9) (+ y i)) (code-char 9552))
		(setf (aref board (+ x 18) (+ y i)) (code-char 9552))
		(setf (aref board (+ x 27) (+ y i)) (code-char 9552))

		(setf (aref board (+ x i) (+ y 0)) (code-char 9553))
		(setf (aref board (+ x i) (+ y 9)) (code-char 9553))
		(setf (aref board (+ x i) (+ y 18)) (code-char 9553))
		(setf (aref board (+ x i) (+ y 27)) (code-char 9553))
	)

	;Draw corners
	(setf (aref board (+ x 0) (+ y 0)) (code-char 9556))
	(setf (aref board (+ x 0) (+ y 27)) (code-char 9559))
	(setf (aref board (+ x 27) (+ y 0)) (code-char 9562))
	(setf (aref board (+ x 27) (+ y 27)) (code-char 9565))

	;Crosses
	(setf (aref board (+ x 9) (+ y 9)) (code-char 9580))
	(setf (aref board (+ x 9) (+ y 18)) (code-char 9580))
	(setf (aref board (+ x 18) (+ y 9)) (code-char 9580))
	(setf (aref board (+ x 18) (+ y 18)) (code-char 9580))	

	;T interserctions
	(setf (aref board (+ x 0) (+ y 9)) (code-char 9574))
	(setf (aref board (+ x 0) (+ y 18)) (code-char 9574))
	(setf (aref board (+ x 9) (+ y 0)) (code-char 9568))
	(setf (aref board (+ x 18) (+ y 0)) (code-char 9568))
	(setf (aref board (+ x 9) (+ y 27)) (code-char 9571))
	(setf (aref board (+ x 18) (+ y 27)) (code-char 9571))
	(setf (aref board (+ x 27) (+ y 9)) (code-char 9577))
	(setf (aref board (+ x 27) (+ y 18)) (code-char 9577))

)

(defun drawOnCellX (board x y)
	(setf (aref board (+ x 1) (+ y 1)) (code-char 9608))
	(setf (aref board (+ x 1) (+ y 6)) (code-char 9608))

	(setf (aref board (+ x 2) (+ y 2)) (code-char 9608))
	(setf (aref board (+ x 2) (+ y 5)) (code-char 9608))
	
	(setf (aref board (+ x 3) (+ y 3)) (code-char 9608))
	(setf (aref board (+ x 3) (+ y 4)) (code-char 9608))
	
	(setf (aref board (+ x 4) (+ y 3)) (code-char 9608))
	(setf (aref board (+ x 4) (+ y 4)) (code-char 9608))
	
	(setf (aref board (+ x 5) (+ y 2)) (code-char 9608))
	(setf (aref board (+ x 5) (+ y 5)) (code-char 9608))
	
	(setf (aref board (+ x 6) (+ y 1)) (code-char 9608))
	(setf (aref board (+ x 6) (+ y 6)) (code-char 9608))
)

(defun drawOnCellO (board x y)
	(setf (aref board (+ x 1) (+ y 1)) (code-char 9608))
	(setf (aref board (+ x 1) (+ y 6)) (code-char 9608))
	(loop for i from 1 to 6 by 1 do
		(setf (aref board (+ x 1) (+ y i)) (code-char 9608))
		(setf (aref board (+ x 6) (+ y i)) (code-char 9608))
		(setf (aref board (+ x i) (+ y 1)) (code-char 9608))
		(setf (aref board (+ x i) (+ y 6)) (code-char 9608))
	)
)


(defun drawOnCellCat (board x y game)
	;Letters
	(setf (aref board (+ x 0) (+ y 2)) 'A)
	(setf (aref board (+ x 0) (+ y 4)) 'B)
	(setf (aref board (+ x 0) (+ y 6)) 'C)

	;Numbers
	(setf (aref board (+ x 2) (+ y 0)) '1)
	(setf (aref board (+ x 4) (+ y 0)) '2)
	(setf (aref board (+ x 6) (+ y 0)) '3)

	;Matrix
	(setf (aref board (+ x 1) (+ y 1)) (code-char 9484))
	(setf (aref board (+ x 1) (+ y 2)) (code-char 9472))
	(setf (aref board (+ x 1) (+ y 3)) (code-char 9516))
	(setf (aref board (+ x 1) (+ y 4)) (code-char 9472))
	(setf (aref board (+ x 1) (+ y 5)) (code-char 9516))
	(setf (aref board (+ x 1) (+ y 6)) (code-char 9472))
	(setf (aref board (+ x 1) (+ y 7)) (code-char 9488))

	(setf (aref board (+ x 2) (+ y 1)) (code-char 9474))
	(setf (aref board (+ x 2) (+ y 3)) (code-char 9474))
	(setf (aref board (+ x 2) (+ y 5)) (code-char 9474))
	(setf (aref board (+ x 2) (+ y 7)) (code-char 9474))

	(setf (aref board (+ x 3) (+ y 1)) (code-char 9500))
	(setf (aref board (+ x 3) (+ y 2)) (code-char 9472))
	(setf (aref board (+ x 3) (+ y 3)) (code-char 9532))
	(setf (aref board (+ x 3) (+ y 4)) (code-char 9472))
	(setf (aref board (+ x 3) (+ y 5)) (code-char 9532))
	(setf (aref board (+ x 3) (+ y 6)) (code-char 9472))
	(setf (aref board (+ x 3) (+ y 7)) (code-char 9508))

	(setf (aref board (+ x 4) (+ y 1)) (code-char 9474))
	(setf (aref board (+ x 4) (+ y 3)) (code-char 9474))
	(setf (aref board (+ x 4) (+ y 5)) (code-char 9474))
	(setf (aref board (+ x 4) (+ y 7)) (code-char 9474))

	(setf (aref board (+ x 5) (+ y 1)) (code-char 9500))
	(setf (aref board (+ x 5) (+ y 2)) (code-char 9472))
	(setf (aref board (+ x 5) (+ y 3)) (code-char 9532))
	(setf (aref board (+ x 5) (+ y 4)) (code-char 9472))
	(setf (aref board (+ x 5) (+ y 5)) (code-char 9532))
	(setf (aref board (+ x 5) (+ y 6)) (code-char 9472))
	(setf (aref board (+ x 5) (+ y 7)) (code-char 9508))

	(setf (aref board (+ x 6) (+ y 1)) (code-char 9474))
	(setf (aref board (+ x 6) (+ y 3)) (code-char 9474))
	(setf (aref board (+ x 6) (+ y 5)) (code-char 9474))
	(setf (aref board (+ x 6) (+ y 7)) (code-char 9474))

	(setf (aref board (+ x 7) (+ y 1)) (code-char 9492))
	(setf (aref board (+ x 7) (+ y 2)) (code-char 9472))
	(setf (aref board (+ x 7) (+ y 3)) (code-char 9524))
	(setf (aref board (+ x 7) (+ y 4)) (code-char 9472))
	(setf (aref board (+ x 7) (+ y 5)) (code-char 9524))
	(setf (aref board (+ x 7) (+ y 6)) (code-char 9472))
	(setf (aref board (+ x 7) (+ y 7)) (code-char 9496))

	;Moves
	(loop for i from 0 to 2 by 1 do
		(loop for j from 0 to 2 by 1 do
			(setf (aref board (+ x 2 (* 2 i)) (+ y 2 (* 2 j))) 
				(case (second (nth j (nth i game)))
					('X 'X)
					('O 'O)
					(T (code-char 32))
				)
			)
		)
	)
)

(defun drawBigCell (board x y game)
	(case (second game)
		('X (drawOnCellX board x y))
		('O (drawOnCellO board x y))
		(T (drawOnCellCat board x y (third game)))
	)
)

(defun highlightBigCell (board x y)
	(loop for i from 0 to 7 by 1 do
		(loop for j from 0 to 7 by 1 do
			(setf (aref board (+ i x) (+ j y)) T)
		)
	)
)

(defun printMetagato (game &optional (highlightedCell NIL))
	(let (
		(board (make-array '(30 32) :initial-element (code-char 32)))
		(highlightBoard (make-array '(30 32) :initial-element NIL))
		(x 2)
		(y 4)
		)

		(setFrameBoard board)
		(setBigBoard board x y)

		(unless (null highlightedCell)
			(highlightBigCell 
				highlightBoard 
				(+ 1 x (* (if (null (first highlightedCell)) 0 (first highlightedCell)) 9)) 
				(+ 1 y (* (if (null (second highlightedCell)) 0 (second highlightedCell)) 9))
			)
		)
		
		(loop for i from 0 to 2 by 1 do
			(loop for j from 0 to 2 by 1 do
				(drawBigCell board (+ 1 x (* i 9)) (+ 1 y (* j 9)) (nth j (nth i game)))
			)
		)

		(loop for i from 0 to 29 by 1 do
			(loop for j from 0 to 31 by 1 do
				(if (aref highlightBoard i j)
					(format t "~c[43m~a" #\ESC (aref board i j))
					(format t "~c[0m~a" #\ESC (aref board i j))
				)
				;(format t "~a" (aref highlightBoard i j))
			)
			(format t "~%")
		)

	)
)


;(printMetagato mapaGato '(2 2))

(defun loadEmptyGame ()
	(setq mapaGato '(
		((:gato NIL (
			((:celda NIL) (:celda NIL) (:celda NIL))
			((:celda O) (:celda NIL) (:celda NIL))
			((:celda NIL) (:celda NIL) (:celda NIL))
			)
		)
		(:gato NIL (
			((:celda NIL) (:celda NIL) (:celda NIL))
			((:celda NIL) (:celda O) (:celda NIL))
			((:celda NIL) (:celda NIL) (:celda NIL))
			)
		)
		(:gato NIL (
			((:celda NIL) (:celda NIL) (:celda NIL))
			((:celda NIL) (:celda NIL) (:celda O))
			((:celda NIL) (:celda NIL) (:celda NIL))
			)
		))

		((:gato X (
			((:celda NIL) (:celda NIL) (:celda NIL))
			((:celda NIL) (:celda NIL) (:celda NIL))
			((:celda NIL) (:celda NIL) (:celda NIL))
			)
		)
		(:gato O (
			((:celda NIL) (:celda NIL) (:celda NIL))
			((:celda NIL) (:celda NIL) (:celda NIL))
			((:celda NIL) (:celda NIL) (:celda NIL))
			)
		)
		(:gato NIL (
			((:celda NIL) (:celda NIL) (:celda NIL))
			((:celda NIL) (:celda NIL) (:celda NIL))
			((:celda NIL) (:celda NIL) (:celda NIL))
			)
		))

		((:gato NIL (
			((:celda NIL) (:celda NIL) (:celda NIL))
			((:celda NIL) (:celda NIL) (:celda NIL))
			((:celda NIL) (:celda NIL) (:celda NIL))
			)
		)
		(:gato NIL (
			((:celda NIL) (:celda NIL) (:celda NIL))
			((:celda NIL) (:celda NIL) (:celda NIL))
			((:celda NIL) (:celda NIL) (:celda NIL))
			)
		)
		(:gato NIL (
			((:celda NIL) (:celda NIL) (:celda NIL))
			((:celda NIL) (:celda NIL) (:celda NIL))
			((:celda NIL) (:celda NIL) (:celda NIL))
			)
		))
	))
)

(defun isCellValid (cell)
	(setq cell (string-upcase cell))
	(cond 
		((string= cell "1A") T)
		((string= cell "1B") T)
		((string= cell "1C") T)
		((string= cell "2A") T)
		((string= cell "2B") T)
		((string= cell "2C") T)
		((string= cell "3A") T)
		((string= cell "3B") T)
		((string= cell "3C") T)
		(T NIL)
	)
)

(defun getCell (inputCell)
	(let (
		(cell (string-upcase inputCell))
		)
		(cond 
			((string= cell "1A") '(0 0))
			((string= cell "1B") '(0 1))
			((string= cell "1C") '(0 2))
			((string= cell "2A") '(1 0))
			((string= cell "2B") '(1 1))
			((string= cell "2C") '(1 2))
			((string= cell "3A") '(2 0))
			((string= cell "3B") '(2 1))
			((string= cell "3C") '(2 2))
			(T NIL)
		)
	)
)

;Regresa T/NIL si algún jugador ya ganó o si ya no hay más movimientos posibles
(defvar gameOver NIL)
(defun isGameOver (state playingCell)
	gameOver
)

(defun isCellFree (state playingCell userMove)
	(let (
		(game (nth (second playingCell) (nth (first playingCell) state)))
		(cell NIL)
		)
		(if (null (second game))
			(progn
				(setq cell (nth (second userMove) (nth (first userMove) (third game))))
				(if (null (second cell))
					T
					NIL
				)
			)
			NIL
		)
	)
)

(defun doUserMove (state playingCell userMove)
	(setf (second (nth (second userMove) (nth (first userMove) (third (nth (second playingCell) (nth (first playingCell) state)))))) 'X)
)

;Dado un tablero de juego y el juegador que está jugando, determina cuál es la puntuación
; Puntuaciones por linea/diagonal libre con:
;	0		puntos:	0 elementos 
;	8		puntos:	1 elemento
;	64		puntos:	2 elementos
;	512		puntos:	3 elementos
(defun getInnerCellScore (state player) 
	(let (
		(score 0)
		(cnt1 0)
		(cnt2 0)
		(flag1 NIL)
		(flag2 NIL)
		)

		;Revisa filas y columnas
		(loop for i from 0 to 2 by 1 do
			(loop for j from 0 to 2 by 1 do
				(if (equal (second (nth j (nth i state))) player) 
					(setq cnt1 (+ 1 cnt1))
					(unless (null (second (nth j (nth i state))))
						(setq flag1 T)
					)
				)
				(if (equal (second (nth i (nth j state))) player) 
					(setq cnt2 (+ 1 cnt2))
					(unless (second (null (nth i (nth j state))))
						(setq flag2 T)
					)
				)
			)
			(unless flag1
				(case cnt1
					(1 (setq score (+ 8 score)))
					(2 (setq score (+ 64 score)))
					(3 (setq score (+ 512 score)))
				)
			)
			(unless flag2
				(case cnt2
					(1 (setq score (+ 8 score)))
					(2 (setq score (+ 64 score)))
					(3 (setq score (+ 512 score)))
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
		(loop for i from 0 to 2 by 1 do
			(if (equal (second (nth i (nth i state))) player) 
				(setq cnt1 (+ 1 cnt1))
				(unless (null (second (nth i (nth i state))))
					(setq flag1 T)
				)
			)
			(if (equal (second (nth i (nth (- 2 i) state))) player) 
				(setq cnt2 (+ 1 cnt2))
				(unless (null (second (nth i (nth (- 2 i) state))))
					(setq flag2 T)
				)
			)
		)
		(unless flag1
			(case cnt1
				(1 (setq score (+ 8 score)))
				(2 (setq score (+ 64 score)))
				(3 (setq score (+ 512 score)))
			)
		)
		(unless flag2
			(case cnt2
				(1 (setq score (+ 8 score)))
				(2 (setq score (+ 64 score)))
				(3 (setq score (+ 512 score)))
			)
		)

		(format t "state -> ~A, score = ~A ~%" state score)

		score
	)
)

(defun startGame ()
	(let (
		(input NIL)
		(playingCell NIL)
		(userMove NIL)
		)

		(loadEmptyGame)

		;Seleccionar primera casilla
		(loop until (isCellValid input) do
			(printMetagato mapaGato)
			(format t "Introduce la celda grande en la que deseas tirar (Ej 1A): ") (terpri)
			(setq input (read-line))(terpri)
			(unless (isCellValid input) 
				(format t "La casilla introducida es invalida, por favor ingrese una correcta ~%")(terpri)
			)
		)

		(setq playingCell (getCell input))
		(format t "Casilla ~A ~A seleccionada." input playingCell) (terpri)
		(setq input NIL)

		;Do game
		(loop until (isGameOver mapaGato playingCell) do

			;Tira el usuario
			(loop until (isCellValid input) do
				(printMetagato mapaGato playingCell)
				(format t "Introduce la celda donde deseas tirar (En la casilla resaltada) (Ej 1A)): ") (terpri)
				(setq input (read-line))(terpri)
				(if (isCellValid input) 
					(progn 
						(setq userMove (getCell input))
						(unless (isCellFree mapaGato playingCell userMove)
							(format t "La casilla introducida es ya esta ocupada, por favor ingrese una diferente ~%")(terpri)
						)
					)
					(progn
						(format t "La casilla introducida es invalida, por favor ingrese una correcta ~%")(terpri)
					)
				)
			)

			;Se hace la jugada del usuario
			(doUserMove mapaGato playingCell userMove)

			;Se repinta el tablero con la tirada
			(setq playingCell userMove)
			(printMetagato mapaGato playingCell)
			(setq gameOver T)

			;Si el usuario no ganó, tira mi IA
			(getInnerCellScore (third (nth (second playingCell) (nth (first playingCell) mapaGato))) 'O)

			;Se repinta el tablero

			;Se imprime las tiradas que se hicieron por la IA

			;Repite
		)

		(format t "Juego terminado~%") (terpri)
	)

)

(startGame)