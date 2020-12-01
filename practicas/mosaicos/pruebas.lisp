#!/usr/bin/sbcl --script

(load "mosaic-lib.lisp")
(defparameter mosBoard NIL)
(defparameter mosPieces NIL)
(setq mosBoard (get-board))
(setq mosPieces (get-pieces))

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

		(loop for i from 6 downto 0 do 
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
(defvar someState '(NIL (B 4) (A 5) NIL NIL NIL NIL NIL NIL NIL))
(print mosBoard)
(addOps)
(print ops)