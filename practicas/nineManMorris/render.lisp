#!/usr/bin/sbcl --script

(defun clearScreen () 
	(format t "~C[2J" #\Esc)(terpri)
)

(defun renderScreen () 
	(format t "Hola Mundo")
)

(defun refresh ()
	(renderScreen)
	(terpri)
)

(defun graphicEngine ()
	(loop do
		(refresh)
		(sleep 0.04166)
		(clearScreen)
	)
)


(graphicEngine)
