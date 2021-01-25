#!/usr/bin/sbcl --script

;; ===============================
; Ejercicio "Nine Man Morris"
; Alumna: Enya Quetzalli Gómez Rodríguez (Eduardo Gómez Rodríguez)
; Boleta: 2014030529
; Profesor: Dr. Salvador Godoy
; Escuela: Centro de Investigación de Cómputo - IPN
; Semestre: 2021/1
;; ===============================


;Global Variables
(defvar input NIL)
(defvar stopGame NIL)
(defvar auxList NIL)
(defvar auxElement NIL)
(defvar rojasOnHouse 0)
(defvar rojasOnBoard 0)
(defvar rojasOnCementery 0)
(defvar azulesOnHouse 0)
(defvar azulesOnBoard 0)
(defvar azulesOnCementery 0)

;; =================================
; Game Graph
;; =================================

; 01 ----------- 02 ----------- 03
; |              |              |
; |    04 ------ 05 ------ 06   |
; |    |         |         |    |
; |    |    07 - 08 - 09   |    |
; |    |    |         |    |    |
; 10 - 11 - 12        13 - 14 - 15
; |    |    |         |    |    |
; |    |    16 - 17 - 18   |    |
; |    |         |         |    |
; |    19 ------ 20 ------ 21   |
; |              |              |
; 22 ----------- 23 ----------- 24

;; =================================
; Logic Graph of the game
;; =================================

;In this section, you will find the board as a graph and the management of the possible movements on the board.
;This will create an Id for each cell on the board, and then with an adjacency list will connect with the other cells.
;It will also setup some states for the graphic engine. 
(defvar graph (make-hash-table :test #'equal))
(defvar lines '(
	(1  2  3 )
	(4  5  6 )
	(7  8  9 )
	(10 11 12)
	(13 14 15)
	(16 17 18)
	(19 20 21)
	(22 23 24)
	(1  10 22)
	(4  11 19)
	(7  12 16)
	(2  5  8 )
	(17 20 23)
	(9  13 18)
	(6  14 21)
	(3  15 24)
	))

(defun createGraph () 
	(setf (gethash 1 graph)  '((:arriba . NIL) (:abajo . 10 ) (:derecha . 2  ) (:izquierda . NIL)))
	(setf (gethash 2 graph)  '((:arriba . NIL) (:abajo . 5  ) (:derecha . 3  ) (:izquierda . 1  )))
	(setf (gethash 3 graph)  '((:arriba . NIL) (:abajo . 15 ) (:derecha . NIL) (:izquierda . 2  )))
	(setf (gethash 4 graph)  '((:arriba . NIL) (:abajo . 11 ) (:derecha . 5  ) (:izquierda . NIL)))
	(setf (gethash 5 graph)  '((:arriba . 2  ) (:abajo . 8  ) (:derecha . 6  ) (:izquierda . 4  )))
	(setf (gethash 6 graph)  '((:arriba . NIL) (:abajo . 14 ) (:derecha . NIL) (:izquierda . 5  )))
	(setf (gethash 7 graph)  '((:arriba . NIL) (:abajo . 12 ) (:derecha . 8  ) (:izquierda . NIL)))
	(setf (gethash 8 graph)  '((:arriba . 5  ) (:abajo . NIL) (:derecha . 9  ) (:izquierda . 7  )))
	(setf (gethash 9 graph)  '((:arriba . NIL) (:abajo . 13 ) (:derecha . NIL) (:izquierda . 8  )))
	(setf (gethash 10 graph) '((:arriba . 1  ) (:abajo . 22 ) (:derecha . 11 ) (:izquierda . NIL)))
	(setf (gethash 11 graph) '((:arriba . 4  ) (:abajo . 19 ) (:derecha . 12 ) (:izquierda . 10 )))
	(setf (gethash 12 graph) '((:arriba . 7  ) (:abajo . 16 ) (:derecha . NIL) (:izquierda . 11 )))
	(setf (gethash 13 graph) '((:arriba . 9  ) (:abajo . 18 ) (:derecha . 14 ) (:izquierda . NIL)))
	(setf (gethash 14 graph) '((:arriba . 6  ) (:abajo . 21 ) (:derecha . 15 ) (:izquierda . 13 )))
	(setf (gethash 15 graph) '((:arriba . 3  ) (:abajo . 24 ) (:derecha . NIL) (:izquierda . 14 )))
	(setf (gethash 16 graph) '((:arriba . 12 ) (:abajo . NIL) (:derecha . 17 ) (:izquierda . NIL)))
	(setf (gethash 17 graph) '((:arriba . NIL) (:abajo . 20 ) (:derecha . 18 ) (:izquierda . 16 )))
	(setf (gethash 18 graph) '((:arriba . 13 ) (:abajo . NIL) (:derecha . NIL) (:izquierda . 17 )))
	(setf (gethash 19 graph) '((:arriba . 11 ) (:abajo . NIL) (:derecha . 20 ) (:izquierda . NIL)))
	(setf (gethash 20 graph) '((:arriba . 17 ) (:abajo . 23 ) (:derecha . 21 ) (:izquierda . 19 )))
	(setf (gethash 21 graph) '((:arriba . 14 ) (:abajo . NIL) (:derecha . NIL) (:izquierda . 20 )))
	(setf (gethash 22 graph) '((:arriba . 10 ) (:abajo . NIL) (:derecha . 23 ) (:izquierda . NIL)))
	(setf (gethash 23 graph) '((:arriba . 20 ) (:abajo . NIL) (:derecha . 24 ) (:izquierda . 22 )))
	(setf (gethash 24 graph) '((:arriba . 15 ) (:abajo . NIL) (:derecha . NIL) (:izquierda . 23 )))
)
(createGraph)


(defun moveInGraph (currentNode direction)
	(loop for adj in (gethash currentNode graph) do
		(if (equal direction (first adj))
			(unless (null (rest adj))
				(return-from moveInGraph (rest adj))
			)
		)
	)
	(return-from moveInGraph currentNode) 
)

(defun copy-table (table)
  (let ((new-table (make-hash-table
                    :test (hash-table-test table)
                    :size (hash-table-size table))))
    (maphash #'(lambda(key value)
                 (setf (gethash key new-table) (copy-tree value)))
             table)
    new-table))


;; =================================
; Graphic engine for the game
;; =================================

;On the graphic engine, you're going to find a lot of useful fuctions that will let the game be draw on the terminal console.
;It uses ANSI colors standard, so the consoile where you run the game needs to be compatible with it. 


;Board definitions
(defconstant width 79)
(defconstant heigth 30)
(defvar boardText (make-array (list width heigth)))
(defvar boardTextStyle (make-array (list width heigth)))
(defvar boardTextColor (make-array (list width heigth)))
(defvar boardBackgroundColor (make-array (list width heigth)))
(defvar cellPropeties (make-hash-table :test #'equal))
(defvar linePropeties (make-hash-table :test #'equal))
(defvar ansiQueue NIL)

;ANSI colors constants
(defconstant none 0)
(defconstant black 1)
(defconstant red 2)
(defconstant green 3)
(defconstant yellow 4)
(defconstant blue 5)
(defconstant magenta 6)
(defconstant cyan 7)
(defconstant white 8)

;ANSI text-style constants
(defconstant none 0)
(defconstant bold 1)
(defconstant italic 3)
(defconstant underline 4)
(defconstant inverse 7)

;Unicode char constants
(defconstant whitespace (code-char 32))

;Propeties: (cellLetter backgroundColor textColor isPartOfLine)
(defun resetCells () 
	(setf (gethash 1 cellPropeties)  (list 'A black white NIL))
	(setf (gethash 2 cellPropeties)  (list 'B black white NIL))
	(setf (gethash 3 cellPropeties)  (list 'C black white NIL))
	(setf (gethash 4 cellPropeties)  (list 'D black white NIL))
	(setf (gethash 5 cellPropeties)  (list 'E black white NIL))
	(setf (gethash 6 cellPropeties)  (list 'F black white NIL))
	(setf (gethash 7 cellPropeties)  (list 'G black white NIL))
	(setf (gethash 8 cellPropeties)  (list 'H black white NIL))
	(setf (gethash 9 cellPropeties)  (list 'I black white NIL))
	(setf (gethash 10 cellPropeties) (list 'J black white NIL))
	(setf (gethash 11 cellPropeties) (list 'K black white NIL))
	(setf (gethash 12 cellPropeties) (list 'L black white NIL))
	(setf (gethash 13 cellPropeties) (list 'M black white NIL))
	(setf (gethash 14 cellPropeties) (list 'N black white NIL))
	(setf (gethash 15 cellPropeties) (list 'O black white NIL))
	(setf (gethash 16 cellPropeties) (list 'P black white NIL))
	(setf (gethash 17 cellPropeties) (list 'Q black white NIL))
	(setf (gethash 18 cellPropeties) (list 'R black white NIL))
	(setf (gethash 19 cellPropeties) (list 'S black white NIL))
	(setf (gethash 20 cellPropeties) (list 'T black white NIL))
	(setf (gethash 21 cellPropeties) (list 'U black white NIL))
	(setf (gethash 22 cellPropeties) (list 'V black white NIL))
	(setf (gethash 23 cellPropeties) (list 'W black white NIL))
	(setf (gethash 24 cellPropeties) (list 'X black white NIL))
)

;Propeties: (lineColor)
(defun resetLines ()
	(setf (gethash '(1  2 ) linePropeties) (list black))
	(setf (gethash '(2  3 ) linePropeties) (list black))
	(setf (gethash '(4  5 ) linePropeties) (list black))
	(setf (gethash '(5  6 ) linePropeties) (list black))
	(setf (gethash '(7  8 ) linePropeties) (list black))
	(setf (gethash '(8  9 ) linePropeties) (list black))
	(setf (gethash '(10 11) linePropeties) (list black))
	(setf (gethash '(11 12) linePropeties) (list black))
	(setf (gethash '(13 14) linePropeties) (list black))
	(setf (gethash '(14 15) linePropeties) (list black))
	(setf (gethash '(16 17) linePropeties) (list black))
	(setf (gethash '(17 18) linePropeties) (list black))
	(setf (gethash '(19 20) linePropeties) (list black))
	(setf (gethash '(20 21) linePropeties) (list black))
	(setf (gethash '(22 23) linePropeties) (list black))
	(setf (gethash '(23 24) linePropeties) (list black))

	(setf (gethash '(1  10) linePropeties) (list black))
	(setf (gethash '(10 22) linePropeties) (list black))
	(setf (gethash '(4  11) linePropeties) (list black))
	(setf (gethash '(11 19) linePropeties) (list black))
	(setf (gethash '(7  12) linePropeties) (list black))
	(setf (gethash '(12 16) linePropeties) (list black))
	(setf (gethash '(2  5 ) linePropeties) (list black))
	(setf (gethash '(5  8 ) linePropeties) (list black))
	(setf (gethash '(17 20) linePropeties) (list black))
	(setf (gethash '(20 23) linePropeties) (list black))
	(setf (gethash '(9  13) linePropeties) (list black))
	(setf (gethash '(13 18) linePropeties) (list black))
	(setf (gethash '(6  14) linePropeties) (list black))
	(setf (gethash '(14 21) linePropeties) (list black))
	(setf (gethash '(3  15) linePropeties) (list black))
	(setf (gethash '(15 24) linePropeties) (list black))
)

(defun getCellNumber (letter)
	(cond
		((string= letter "A") 1 )
		((string= letter "B") 2 )
		((string= letter "C") 3 )
		((string= letter "D") 4 )
		((string= letter "E") 5 )
		((string= letter "F") 6 )
		((string= letter "G") 7 )
		((string= letter "H") 8 )
		((string= letter "I") 9 )
		((string= letter "J") 10)
		((string= letter "K") 11)
		((string= letter "L") 12)
		((string= letter "M") 13)
		((string= letter "N") 14)
		((string= letter "O") 15)
		((string= letter "P") 16)
		((string= letter "Q") 17)
		((string= letter "R") 18)
		((string= letter "S") 19)
		((string= letter "T") 20)
		((string= letter "U") 21)
		((string= letter "V") 22)
		((string= letter "W") 23)
		((string= letter "X") 24)
		(T nil)
	)
)

(defun getCellLetter (cellNumber)
	(cond
		((= cellNumber 1 ) "A")
		((= cellNumber 2 ) "B")
		((= cellNumber 3 ) "C")
		((= cellNumber 4 ) "D")
		((= cellNumber 5 ) "E")
		((= cellNumber 6 ) "F")
		((= cellNumber 7 ) "G")
		((= cellNumber 8 ) "H")
		((= cellNumber 9 ) "I")
		((= cellNumber 10) "J")
		((= cellNumber 11) "K")
		((= cellNumber 12) "L")
		((= cellNumber 13) "M")
		((= cellNumber 14) "N")
		((= cellNumber 15) "O")
		((= cellNumber 16) "P")
		((= cellNumber 17) "Q")
		((= cellNumber 18) "R")
		((= cellNumber 19) "S")
		((= cellNumber 20) "T")
		((= cellNumber 21) "U")
		((= cellNumber 22) "V")
		((= cellNumber 23) "W")
		((= cellNumber 24) "X")
		(T nil)
	)
)
(defun getCellColor (cellNumber &optional (board cellPropeties)) 
	(nth 1 (gethash cellNumber board))
)


(defun clearScreen () 
	(format t "~C[2J" #\Esc)(terpri)

	(loop for i from 0 to (- heigth 1) by 1 do
		(loop for j from 0 to (- width 1) by 1 do
			(setf (aref boardText j i) whitespace)
			(setf (aref boardTextStyle j i) none)
			(setf (aref boardTextColor j i) none)
			(setf (aref boardBackgroundColor j i) none)			
		)
	)

)

(defun putchar (char color style x y)
	(setf (aref boardText x y) char)
	(setf (aref boardTextColor x y) (if (= none color) none (+ 30 color -1)))
	(setf (aref boardTextStyle x y) style)
)

(defun setcolor (color x y)
	(setf (aref boardBackgroundColor x y) (+ 40 color -1))
)

(defun quitchar (x y)
	(setf (aref boardText x y) whitespace)
	(setf (aref boardTextColor x y) none)
)

(defun quitcolor (x y)
	(setf (aref boardBackgroundColor x y) none)
)

(defun writeText (text color style x y)
	(do (
		(k 0 (+ 1 k))
		(i x (+ 1 i))
	    )
	    ((or (= k (length text)) (= i width)) T)
		(putchar (char text k) white style i y)
	)
)

(defun renderText ()
	(loop for i from 0 to (- heigth 1) by 1 do
		(loop for j from 0 to (- width 1) by 1 do
			(setf (aref boardText j i) 'X)
		)
	)
)

(defun renderBackgroundColor () 
	(loop for i from 0 to (- heigth 5) by 1 do
		(loop for j from 0 to (- width 1) by 1 do
			(setf (aref boardBackgroundColor j i) (+ 40 red))
		)
	)
)

(defun renderTextColor () 
	(loop for i from 0 to (- heigth 1) by 1 do
		(loop for j from 0 to (- width 1) by 1 do
			(setf (aref boardBackgroundColor j i) (+ 30 green))
		)
	)
)

(defun renderBoard ()
	(loop for i from 0 to (- heigth 1) by 1 do
		(loop for j from 0 to (- width 1) by 1 do
			(unless (equal none (aref boardBackgroundColor j i))
				(push (aref boardBackgroundColor j i) ansiQueue)
			)
			(unless (equal none (aref boardTextColor j i))
				(push (aref boardTextColor j i) ansiQueue)
			)
			(unless (equal none (aref boardTextStyle j i))
				(push (aref boardTextStyle j i) ansiQueue)
			)
			(format t "~c[" #\ESC)
			(if (null ansiQueue)
				(format t "0")
				(progn
					(format t "~A" (pop ansiQueue))
					(loop for code in ansiQueue do
						(format t "~c~A" #\; code)
					)
				)
			)
			(setq ansiQueue NIL)
			(format t "m~A~c[0m" (aref boardText j i) #\ESC)
		)
		(format t "~c[0m~%" #\ESC)
	)
)

(defun refresh ()
	;(renderText)
	;(renderTextColor)
	;(renderBackgroundColor)
	(drawGame)
	(renderBoard)
	(terpri)
)

(defun graphicEngine ()
	(clearScreen)
	(loop until stopGame do
		(refresh)
		(sleep 1)
		(clearScreen)
	)
)

(defun render ()
	(clearScreen)
	(refresh)
)


;; ========================
; Game Render
;; ========================

(defun drawPiece (color x y &optional (selected NIL) (centerColor none) (letter '0)) 
	(loop for i from 0 to 2 do
		(putchar (code-char 9602) color none (min (+ x i) width) (min (+ y 0) heigth))
		;(setcolor color (min (+ x i) width) (min (+ y 0) heigth))
	) 
	(loop for i from 0 to 2 do
		;(putchar (code-char 9608) color none (min (+ x i) width) (min (+ y 1) heigth))
		(setcolor color (min (+ x i) width) (min (+ y 1) heigth))
	) 
	(loop for i from 0 to 2 do
		(putchar (code-char 9620) color none (min (+ x i) width) (min (+ y 2) heigth))
		;(setcolor color (min (+ x i) width) (min (+ y 2) heigth))
	)

	(cond (selected
		(putchar letter centerColor bold (min (+ x 1) width) (min (+ y 1) heigth))
		(setcolor color(min (+ x 1) width) (min (+ y 1) heigth))
	))
)

(defun drawLine (direction color size x y)
	(cond 
		((equal direction :vertical)
			(loop for i from 0 to (- size 1) by 1 do
				(putchar (code-char 9475) color none (min (+ x 0) width) (min (+ y i) heigth))
			)
		)
		((equal direction :horizontal)
			(loop for i from 0 to (- size 1) by 1 do
				(putchar (code-char 9473) color none (min (+ x i) width) (min (+ y 0) heigth))
			)
		)
	)
)

(defun drawTeamSection (name color x y)
	(loop for i from 0 to 8 do
		(loop for j from 0 to 26 do
			(setcolor color (+ x i) (+ y j))
		)
	)

	(writeText name white bold (+ 1 x) (+ 1 y))
	(setq auxList NIL)
	(push (list (+ 3 x) (+ 19 y)) auxList)
	(push (list (+ 5 x) (+ 15 y)) auxList)
	(push (list (+ 1 x) (+ 15 y)) auxList)
	(push (list (+ 5 x) (+ 11 y)) auxList)
	(push (list (+ 1 x) (+ 11 y)) auxList)
	(push (list (+ 5 x) (+ 7 y)) auxList)
	(push (list (+ 1 x) (+ 7 y)) auxList)
	(push (list (+ 5 x) (+ 3 y)) auxList)
	(push (list (+ 1 x) (+ 3 y)) auxList)

	(loop for i from 0 to (- (if (= color red) rojasOnHouse azulesOnHouse) 1) by 1 do
		(setq auxElement (pop auxList))
		(drawpiece white (first auxElement) (second auxElement))
	)
)

(defun drawCementery (x y)
	(loop for i from 0 to 17 do
		(loop for j from 0 to 2 do
			(setcolor magenta (+ x i) (+ y j))
		)
	)

	(loop for i from 0 to 17 do
		(loop for j from 3 to 26 do
			(setcolor white (+ x i) (+ y j))
		)
	)

	(writeText "CEMENTERIO" white bold (+ 1 x) (+ 1 y))
	
	;Red Deaths
	(setq auxList NIL)
	(push (list (+ 3 x) (+ 19 y)) auxList)
	(push (list (+ 5 x) (+ 15 y)) auxList)
	(push (list (+ 1 x) (+ 15 y)) auxList)
	(push (list (+ 5 x) (+ 11 y)) auxList)
	(push (list (+ 1 x) (+ 11 y)) auxList)
	(push (list (+ 5 x) (+ 7 y)) auxList)
	(push (list (+ 1 x) (+ 7 y)) auxList)
	(push (list (+ 5 x) (+ 3 y)) auxList)
	(push (list (+ 1 x) (+ 3 y)) auxList)

	(loop for i from 0 to (- rojasOnCementery 1) by 1 do
		(setq auxElement (pop auxList))
		(drawpiece red (first auxElement) (second auxElement))
	)

	(setq auxList NIL)
	(push (list (+ 11 x) (+ 19 y)) auxList)
	(push (list (+ 13 x) (+ 15 y)) auxList)
	(push (list (+ 9 x) (+ 15 y)) auxList)
	(push (list (+ 13 x) (+ 11 y)) auxList)
	(push (list (+ 9 x) (+ 11 y)) auxList)
	(push (list (+ 13 x) (+ 7 y)) auxList)
	(push (list (+ 9 x) (+ 7 y)) auxList)
	(push (list (+ 13 x) (+ 3 y)) auxList)
	(push (list (+ 9 x) (+ 3 y)) auxList)

	(loop for i from 0 to (- azulesOnCementery 1) by 1 do
		(setq auxElement (pop auxList))
		(drawpiece blue (first auxElement) (second auxElement))
	)
)

(defun drawTable (x y) 
	(loop for i from 0 to 40 do
		(loop for j from 0 to 26 do
			(setcolor white (+ i x) (+ j y))
		)
	)

	(drawpiece (nth 1 (gethash 1  cellPropeties)) (+ 1  x) (+ 0  y) T white 'A)
	(drawpiece (nth 1 (gethash 2  cellPropeties)) (+ 19 x) (+ 0  y) T white 'B)
	(drawpiece (nth 1 (gethash 3  cellPropeties)) (+ 37 x) (+ 0  y) T white 'C)

	(drawpiece (nth 1 (gethash 4  cellPropeties)) (+ 7  x) (+ 4  y) T white 'D)
	(drawpiece (nth 1 (gethash 5  cellPropeties)) (+ 19 x) (+ 4  y) T white 'E)
	(drawpiece (nth 1 (gethash 6  cellPropeties)) (+ 31 x) (+ 4  y) T white 'F)

	(drawpiece (nth 1 (gethash 7  cellPropeties)) (+ 13 x) (+ 8  y) T white 'G)
	(drawpiece (nth 1 (gethash 8  cellPropeties)) (+ 19 x) (+ 8  y) T white 'H)
	(drawpiece (nth 1 (gethash 9  cellPropeties)) (+ 25 x) (+ 8  y) T white 'I)

	(drawpiece (nth 1 (gethash 10 cellPropeties)) (+ 1  x) (+ 12 y) T white 'J)
	(drawpiece (nth 1 (gethash 11 cellPropeties)) (+ 7  x) (+ 12 y) T white 'K)
	(drawpiece (nth 1 (gethash 12 cellPropeties)) (+ 13 x) (+ 12 y) T white 'L)
	(drawpiece (nth 1 (gethash 13 cellPropeties)) (+ 25 x) (+ 12 y) T white 'M)
	(drawpiece (nth 1 (gethash 14 cellPropeties)) (+ 31 x) (+ 12 y) T white 'N)
	(drawpiece (nth 1 (gethash 15 cellPropeties)) (+ 37 x) (+ 12 y) T white 'O)

	(drawpiece (nth 1 (gethash 16 cellPropeties)) (+ 13 x) (+ 16 y) T white 'P)
	(drawpiece (nth 1 (gethash 17 cellPropeties)) (+ 19 x) (+ 16 y) T white 'Q)
	(drawpiece (nth 1 (gethash 18 cellPropeties)) (+ 25 x) (+ 16 y) T white 'R)

	(drawpiece (nth 1 (gethash 19 cellPropeties)) (+ 7  x) (+ 20 y) T white 'S)
	(drawpiece (nth 1 (gethash 20 cellPropeties)) (+ 19 x) (+ 20 y) T white 'T)
	(drawpiece (nth 1 (gethash 21 cellPropeties)) (+ 31 x) (+ 20 y) T white 'U)

	(drawpiece (nth 1 (gethash 22 cellPropeties)) (+ 1  x) (+ 24 y) T white 'V)
	(drawpiece (nth 1 (gethash 23 cellPropeties)) (+ 19 x) (+ 24 y) T white 'W)
	(drawpiece (nth 1 (gethash 24 cellPropeties)) (+ 37 x) (+ 24 y) T white 'X)


	(drawLine :horizontal (nth 0 (gethash '(1  2 ) linePropeties)) 13 (+ 5  x) (+ 1  y))
	(drawLine :horizontal (nth 0 (gethash '(2  3 ) linePropeties)) 13 (+ 23 x) (+ 1  y))
	(drawLine :horizontal (nth 0 (gethash '(4  5 ) linePropeties)) 7  (+ 11 x) (+ 5  y))
	(drawLine :horizontal (nth 0 (gethash '(5  6 ) linePropeties)) 7  (+ 23 x) (+ 5  y))
	(drawLine :horizontal (nth 0 (gethash '(7  8 ) linePropeties)) 1  (+ 17 x) (+ 9  y))
	(drawLine :horizontal (nth 0 (gethash '(8  9 ) linePropeties)) 1  (+ 23 x) (+ 9  y))
	(drawLine :horizontal (nth 0 (gethash '(10 11) linePropeties)) 1  (+ 5  x) (+ 13 y))
	(drawLine :horizontal (nth 0 (gethash '(11 12) linePropeties)) 1  (+ 11 x) (+ 13 y))
	(drawLine :horizontal (nth 0 (gethash '(13 14) linePropeties)) 1  (+ 29 x) (+ 13 y))
	(drawLine :horizontal (nth 0 (gethash '(14 15) linePropeties)) 1  (+ 35 x) (+ 13 y))
	(drawLine :horizontal (nth 0 (gethash '(16 17) linePropeties)) 1  (+ 17 x) (+ 17 y))
	(drawLine :horizontal (nth 0 (gethash '(17 18) linePropeties)) 1  (+ 23 x) (+ 17 y))
	(drawLine :horizontal (nth 0 (gethash '(19 20) linePropeties)) 7  (+ 11 x) (+ 21 y))
	(drawLine :horizontal (nth 0 (gethash '(20 21) linePropeties)) 7  (+ 23 x) (+ 21 y))
	(drawLine :horizontal (nth 0 (gethash '(22 23) linePropeties)) 13 (+ 5  x) (+ 25 y))
	(drawLine :horizontal (nth 0 (gethash '(23 24) linePropeties)) 13 (+ 23 x) (+ 25 y))

	(drawLine :vertical (nth 0 (gethash '(1  10) linePropeties)) 9  (+ 2  x) (+ 3  y))
	(drawLine :vertical (nth 0 (gethash '(10 22) linePropeties)) 9  (+ 2  x) (+ 15 y))
	(drawLine :vertical (nth 0 (gethash '(4  11) linePropeties)) 5  (+ 8  x) (+ 7  y))
	(drawLine :vertical (nth 0 (gethash '(11 19) linePropeties)) 5  (+ 8  x) (+ 15 y))
	(drawLine :vertical (nth 0 (gethash '(7  12) linePropeties)) 1  (+ 14 x) (+ 11 y))
	(drawLine :vertical (nth 0 (gethash '(12 16) linePropeties)) 1  (+ 14 x) (+ 15 y))
	(drawLine :vertical (nth 0 (gethash '(2  5 ) linePropeties)) 1  (+ 20 x) (+ 3  y))
	(drawLine :vertical (nth 0 (gethash '(5  8 ) linePropeties)) 1  (+ 20 x) (+ 7  y))
	(drawLine :vertical (nth 0 (gethash '(17 20) linePropeties)) 1  (+ 20 x) (+ 19 y))
	(drawLine :vertical (nth 0 (gethash '(20 23) linePropeties)) 1  (+ 20 x) (+ 23 y))
	(drawLine :vertical (nth 0 (gethash '(9  13) linePropeties)) 1  (+ 26 x) (+ 11 y))
	(drawLine :vertical (nth 0 (gethash '(13 18) linePropeties)) 1  (+ 26 x) (+ 15 y))
	(drawLine :vertical (nth 0 (gethash '(6  14) linePropeties)) 5  (+ 32 x) (+ 7  y))
	(drawLine :vertical (nth 0 (gethash '(14 21) linePropeties)) 5  (+ 32 x) (+ 15 y))
	(drawLine :vertical (nth 0 (gethash '(3  15) linePropeties)) 9  (+ 38 x) (+ 3  y))
	(drawLine :vertical (nth 0 (gethash '(15 24) linePropeties)) 9  (+ 38 x) (+ 15 y))

)

(defun drawGameTitle (x y)
	(loop for i from 0 to (- width 1) by 1 do
		(loop for j from 0 to 2 by 1 do
			(setcolor green i (+ j y))
		)
	)

	(writeText "NINE MEN MORRIS" white bold (+ 23 x) (+ 1 y))

)

(defun drawGame ()
	(drawGameTitle 0 0)
	(drawTeamSection "ROJAS" red 0 3)
	(drawTeamSection "AZULES" blue 50 3)
	(drawCementery 60 3)
	(drawTable 9 3)
)

(define-alien-type nil
  (struct termios
          (c_iflag unsigned-long)
          (c_oflag unsigned-long)
          (c_cflag unsigned-long)
          (c_lflag unsigned-long)
          (c_cc (array unsigned-char 20))
          (c_ispeed unsigned-long)
          (c_ospeed unsigned-long)))

(declaim (inline tcgetattr))
(define-alien-routine "tcgetattr" int
                      (fd int)
                      (term (* (struct termios))))

(declaim (inline tcsetattr))
(define-alien-routine "tcsetattr" int
                      (fd int)
                      (action int)
                      (term (* (struct termios))))

(defun read-single-byte (&optional (s *standard-input*))
  (with-alien ((old (struct termios))
               (new (struct termios)))
    (let ((e0 (tcgetattr 0 (addr old)))
          (e1 (tcgetattr 0 (addr new)))
          (n-lflag (slot new 'c_lflag)))
      (declare (ignorable e0 e1))           
      (unwind-protect
        (progn
          (setf (ldb (byte 1 8) n-lflag) 0) ; disables canonical mode
          (setf (ldb (byte 1 3) n-lflag) 0) ; disables echoing input char
          (setf (slot new 'c_lflag) n-lflag)
          (tcsetattr 0 0 (addr new))
          (read-byte s))
        (tcsetattr 0 0 (addr old))))))

;; =================================
; Game Controls
;; =================================

(defvar selectedCell nil)

(defun cleanVariables ()
	(setq rojasOnHouse 9)
	(setq azulesOnHouse 9)
	(setq rojasOnBoard 0)
	(setq rojasOnCementery 0)
	(setq azulesOnBoard 0)
	(setq azulesOnCementery 0)
)

(defun isCellFree (cellNumber)
	(equal black (nth 1 (gethash cellNumber cellPropeties)))
)

(defun markCell (player cellNumber)
	(cond 
		((equal :rojas player)
			(setf (nth 1 (gethash cellNumber cellPropeties)) red)
			(setq rojasOnHouse (- rojasOnHouse 1))
			(setq rojasOnBoard (+ rojasOnBoard 1))
			)
		((equal :azules player)
			(setf (nth 1 (gethash cellNumber cellPropeties)) blue)
			(setq azulesOnHouse (- azulesOnHouse 1))
			(setq azulesOnBoard (+ azulesOnHouse 1))
			)
	)
)

(defun markLine (player line)
	(cond 
		((equal :rojas player)
			(setf (nth 0 (gethash (list (nth 0 line) (nth 1 line)) linePropeties)) red)
			(setf (nth 0 (gethash (list (nth 1 line) (nth 2 line)) linePropeties)) red)
			(setf (nth 3 (gethash (nth 0 line) cellPropeties)) T)
			(setf (nth 3 (gethash (nth 1 line) cellPropeties)) T)
			(setf (nth 3 (gethash (nth 2 line) cellPropeties)) T)
			)
		((equal :azules player)
			(setf (nth 0 (gethash (list (nth 0 line) (nth 1 line)) linePropeties)) blue)
			(setf (nth 0 (gethash (list (nth 1 line) (nth 2 line)) linePropeties)) blue)
			(setf (nth 3 (gethash (nth 0 line) cellPropeties)) T)
			(setf (nth 3 (gethash (nth 1 line) cellPropeties)) T)
			(setf (nth 3 (gethash (nth 2 line) cellPropeties)) T)
			)
	)
)

(defun unmarkLine (player line)
	(setf (nth 0 (gethash (list (nth 0 line) (nth 1 line)) linePropeties)) black)
	(setf (nth 0 (gethash (list (nth 1 line) (nth 2 line)) linePropeties)) black)
	
	;TO-DO validar cuantas lineas forma parte de una celda
	(setf (nth 3 (gethash (nth 0 line) cellPropeties)) T)
	(setf (nth 3 (gethash (nth 1 line) cellPropeties)) T)
	(setf (nth 3 (gethash (nth 2 line) cellPropeties)) T)
)

(defun listenUserMove ()
	(setq input nil)
	(loop until (not (null selectedCell)) do
		(render)
		(format t "~c[~a~c~a~c~am   JUEGAN ROJAS    ~c[0m~%" #\ESC (+ 40 red -1) #\; (+ 30 white ) #\; 1 #\ESC *query-io*)
		(format t "      En qué casilla deseas tirar? (Ingresa la letra) " *query-io*)(force-output)
		(setq input (read-line *query-io*))
		(setq input (string-upcase input))
		(setq selectedCell (getCellNumber input))

		(cond 
			((null selectedCell)
				(format t "      La celda es~c[1m invalida ~c[0m~%" #\ESC #\ESC *query-io*)
				(format t "      Por favor, vuelva a ingresar una celda correcta.~%" *query-io*)(force-output)
				(sleep 1)
				(format t "." *query-io*)(force-output)
				(sleep 1)
				(format t "." *query-io*)(force-output)
				(sleep 1)
				(format t "." *query-io*)(force-output)
				(sleep 1)
				(format t "." *query-io*)(force-output)
				(sleep 1)
				(format t "." *query-io*)(force-output)
				(sleep 1)
				(format t "." *query-io*)(force-output)
			)

			((not (isCellFree selectedCell))
				(format t "      La celda ya se encuentra~c[1m ocupada ~c[0m~%" #\ESC #\ESC *query-io*)
				(format t "      Por favor, vuelva a ingresar una celda libre.~%" *query-io*)(force-output)
				(sleep 1)
				(format t "." *query-io*)(force-output)
				(sleep 1)
				(format t "." *query-io*)(force-output)
				(sleep 1)
				(format t "." *query-io*)(force-output)
				(sleep 1)
				(format t "." *query-io*)(force-output)
				(sleep 1)
				(format t "." *query-io*)(force-output)
				(sleep 1)
				(format t "." *query-io*)(force-output)
				(setq selectedCell nil)
			)

			(T
				(markCell :rojas selectedCell)
				(render)
				(format t "~c[~a~c~a~c~am   JUEGAN ROJAS    ~c[0m~%" #\ESC (+ 40 red -1) #\; (+ 30 white ) #\; 1 #\ESC *query-io*)
				(format t "      Rojas tiran en ~c[1m~a~c[0m~%" #\ESC input #\ESC *query-io*)
				(format t "      Presione ENTER para cambiar de turno. " *query-io*)(force-output)
				(read-line)
			)
		)
	)

)

(defun makeQuetzaMove ()
	(render)
	(format t "~c[~a~c~a~c~am   JUEGAN AZULES    ~c[0m~%" #\ESC (+ 40 blue -1) #\; (+ 30 white ) #\; 1 #\ESC *query-io*)
	(format t "      Inteligencia Artifical está calculando su tiro " *query-io*)(force-output)
	(sleep 1)
	(format t "." *query-io*)(force-output)
	(sleep 1)
	(format t "." *query-io*)(force-output)
	(sleep 1)
	(format t "." *query-io*)(force-output)

	(setq selectedCell (second (miniMaxStage1 (copy-table cellPropeties) 0 2 :azules)))
	(markCell :azules selectedCell)
	(render)
	(format t "~c[~a~c~a~c~am   JUEGAN AZULES    ~c[0m~%" #\ESC (+ 40 blue -1) #\; (+ 30 white ) #\; 1 #\ESC *query-io*)
	(format t "      Azules tiran en ~c[1m~a~c[0m~%" #\ESC (getCellLetter selectedCell) #\ESC *query-io*)
	(format t "      Presione ENTER para cambiar de turno. " *query-io*)(force-output)
	(read-line)
)

;; =================================
; Artificial Intellince Player
;; =================================

(defvar 4-connected-cells '(5 11 14 20))
(defvar 3-connected-cells '(2 8 10 12 13 15 17 23))
(defvar 2-connected-cells '(1 3 4 6 7 9 16 18 19 21 22 24))
(defvar mills '(
	((1  2  3 ) (10 5  15))
	((4  5  6 ) (2  8  11 14))
	((7  8  9 ) (5  12 13))
	((10 11 12) (1  4  7  22 19 16))
	((13 14 15) (9  6  3  18 21 24))
	((16 17 18) (12 13 20))
	((19 20 21) (11 17 14 23))
	((22 23 24) (10 20 15))
	((1  10 22) (2  11 22))
	((4  11 19) (10 5  12 20))
	((7  12 16) (11 8  17))
	((2  5  8 ) (1  4  7  3  6  9))
	((17 20 23) (16 19 22 18 21 24))
	((9  13 18) (8  17 14))
	((6  14 21) (5  20 15))
	((3  15 24) (2  23 14))
	))

(defun getCellScoreByConnections (cellNumber)
	(cond 
		((member cellNumber 4-connected-cells)
			20
		)

		((member cellNumber 3-connected-cells)
			10
		)

		((member cellNumber 2-connected-cells)
			2
		)

		(t 0)
	)
)

;Primer heurística
(defun getCellsScore (player board)
	(let (
		(myColor (if (equal player :rojas) red blue))
		(enemyColor (if (equal player :rojas) blue red))
		(totalScore 0)
		(cellScore '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
		)

		;Las celdas con 4 conexiones tienen una mayor ventaja sobre las de 3 o 2 conexiones ya que
		;   se pueden mover mejor en la segunda fase con menores posibilidades de ser bloqueadas
		(loop for i from 1 to 24 by 1 do 
			(cond 
				((member i 4-connected-cells)
					(setf (nth i cellScore) (+ 20 (nth i cellScore)))
				)

				((member i 3-connected-cells)
					(setf (nth i cellScore) (+ 10 (nth i cellScore)))
				)

				((member i 2-connected-cells)
					(setf (nth i cellScore) (+ 2 (nth i cellScore)))
				)
			)
		)

		;Donde el enemigo coloca una ficha, todas las adjacentes a ella tomarán mayor relevancia
		;   esto es por el hecho de que bloquear los movimientos del enemigo es parte escencial del juego
		;   sin embargo únicamente se asignará un solo punto adicional a las casillas aledañas
		(loop for i from 1 to 24 by 1 do
			(cond 
				((equal enemyColor (nth 1 (gethash i board)))
					(loop for adjacentCell in (gethash i graph) do
						(unless (null (rest adjacentCell))
							(setf (nth (rest adjacentCell) cellScore) (+ 1 (nth (rest adjacentCell) cellScore)))
						)
					)

					)
			)
		)

		;Donde yo puedo formar lineas, las fichas involucradas en la linea deben obtener una
		;   puntuacion adicional ya que esto les dará mayor relevancia al ser candidatas a una linea. 

		(loop for i from 1 to 24 by 1 do 
			(format t "Score of cell ~a is ~a ~%" i (nth i cellScore))
		)
	)
)

;Segunda heurística
(defun getBoardScore (player board)
	(let (
		(myColor (if (equal player :rojas) red blue))
		(enemyColor (if (equal player :rojas) blue red))
		(totalScore 0)
		(tempScore 0)
		(goodCounter 0)
		(badCounter 0)
		(line nil)
		(adjacentsToLine nil)
		(isBloqued nil)
		)

		;Se evaluará cada linea, y dependiendo de el número de fichas que tenga se tomará una puntuación basada
		;    en las fichas aledañas. Por ejemplo, una ficha completamente bloqueada perderá su valor. 
		;    Una linea enemiga generará un puntuaje negativo, pero si está totalmente bloqueada el puntaje será menor.

		(loop for mill in mills do 
			;(format t "Processing mill ~a ~%" mill)
			(setq line (first mill))
			(setq adjacentsToLine (second mill))
			(setq goodCounter 0)
			(setq badCounter 0)
			(setq tempScore 0)
			(setq isBloqued t)

			(loop for cell in line do
				(cond
					((equal (getCellColor cell board) myColor) 
						(setq goodCounter (+ 1 goodCounter))
					)
					((equal (getCellColor cell board) enemyColor)
						(setq badCounter (+ 1 badCounter))
					)
				)
			)

			(cond 
				;En caso de que tengamos una linea completa, debemos ver si está totalmente bloqueada 
				;   o se encuentra libre (En la segunda etapa podra mover y regresar)
				((equal goodCounter 3)
					(loop for cell in adjacentsToLine do
						(if (equal (getCellColor cell board) black)
							(setq isBloqued nil)
						)
					)
					(loop for cell in line do
						(setq tempScore (+ tempScore (getCellScoreByConnections cell)))
					)
					(unless isBloqued
						(setq tempScore (+ tempScore 50))
					)
				)

				((equal goodCounter 2)
					(loop for cell in line do
						(if (equal (getCellColor cell board) myColor)
							(setq tempScore (+ tempScore (getCellScoreByConnections cell)))
						)
					)

					(unless (equal goodCounter 0)
						(setq tempScore (+ tempScore 6))
					)
				)

				((equal goodCounter 1)
					(loop for cell in line do
						(setq isBloqued t)
						(loop for adj in (gethash cell graph) do
							(unless (equal myColor (getCellColor (rest adj) board))
								(setq isBloqued nil)
							)
						)

						(unless isBloqued
							(setq tempScore (+ tempScore (getCellScoreByConnections cell)))
						)
					)
				)

				((equal badCounter 3)
					(loop for cell in adjacentsToLine do
						(if (equal (getCellColor cell board) black)
							(setq isBloqued nil)
						)
					)

					(unless isBloqued
						(setq tempScore (+ tempScore -80))
					)
				)
			)

			(setq totalScore (+ totalScore tempScore))
		)
		totalScore
	)
)

(defun isPlayerBloqued (player &optional (board cellPropeties))
	(let (
		(myColor (if (equal player :rojas) red blue))
		(enemyColor (if (equal player :rojas) blue red))
		(isBloqued t)
		(haveCells nil)
		)
		(loop for i from 1 to 24 by 1 do
			(if (equal myColor (getCellColor i board))
				(loop for cell in (gethash i graph) do
					(setq haveCells t)
					(unless (null (rest cell))
						(if (equal black (getCellColor (rest cell) board))
							(setq isBloqued nil)
						)
					)
				)
			)
		)
		(and isBloqued haveCells)
	)
)

;Regresa T/NIL si el jugador ya ganó o aún no
(defun wonPlayer (board player)
	(cond 
		((equal player :rojas)
			(or (isPlayerBloqued player board) (>= rojasOnCementery 7))
		)
		((equal player :azules)
			(or (isPlayerBloqued player board) (>= azulesOnCementery 7))
		)
	)
)

;Regresa T/NIL si algún jugador ya ganó
(defun isGameOver (board)
	(or (wonPlayer board :rojas) (wonPlayer board :azules))
)

(defparameter possibleOps '(
	5 11 14 20 2 8 10 12 13 15 17 23 1 3 4 6 7 9 16 18 19 21 22 24))

;Dado un estado y una operación, determina si es válido aplicarla o no
(defun isOpValid (board op)
	(equal black (getCellColor op board))
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
		(newState (copy-table state))
		)
		(setf (nth 1 (gethash op state)) (if (equal player :rojas) red blue))
		newState
	)
)

(defun miniMaxStage1 (state depth maxDepth player)
	(if (or (isGameOver state) (equal depth maxDepth))
		(return-from miniMaxStage1 (list (getBoardScore player state) NIL))
	)

	(let (
		(bestOp NIL)
		(bestVal (if (equal player :azules) -100000000 100000000))
		(result NIL)
		(currentVal -100000000)
		)

		(loop for op in (getOps state) do
			(setq result (miniMaxStage1 (doOp state op player) (+ 1 depth) maxDepth (if (equal player :rojas) :azules :rojas)))
			;(format t "Current State -> ~30A ~%" state)
			; (format t "Applying op=~A, next state is ~A, score = ~A ~%" op (doOp state op 'O) (first result))
			(setq currentVal (first result))

			(if (equal player :azules)
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


(defun startGame ()
	(cleanVariables)
	(resetCells)
	(resetLines)
	(loop until (or (= rojasOnHouse 0) (= azulesOnHouse 0)) do
		(listenUserMove)
		(makeQuetzaMove)
	)
	;(getCellsScore :azules cellPropeties)
	(format t "Done for now~%")
	(format t "Missing second stage heuristic~%")
)

(startGame)
