; (require "machine.lisp")

; (make-vm 'vm 100)

; (+ (* 2 3) 4))
; (setf test1
; 	'( 
; 		(MOVE 2 R0)
; 		(MOVE R0 R1)
; 		(MOVE 3 R0)
; 		(MOVE R0 R2)
; 		(MULT R1 R2)
; 		(MOVE R2 R0)
; 		(MOVE R0 R1)
; 		(MOVE 4 R0)
; 		(MOVE R0 R2)
; 		(ADD R1 R2)
; 		(MOVE R2 R0)
; 	)
; )

; (setf fact
; 	'(
; 		(WRITE "FACT(")
; 		(WRITE R1)
; 		(WRITE ") = ")
; 		(JSR FACT)
; 		(WRITE R0)
; 		(HALT)

; 	(LABEL FACT)
; 		(MOVE R1 R0)
; 	(LABEL BOUCLE)
; 		(CMP 2 R1)
; 		(JPP END)
; 		(DECR R1)
; 		(MULT R1 R0)
; 		(JMP BOUCLE)

; 	(LABEL END)
; 		(RTN)
; 	)
; ; )

; (setf (get 'vm 'R1) 4)

; (get 'vm 'labels)
; (get 'vm 'mem)

(require "machine.lisp")

(setf expr '(+ (* 2 3) 4))
(setf chemin "../code/ASM.lisp")
(setf str
	(compile-expr expr)
)

(raïteFaïle chemin str)

(setf code (rideFaïle chemin))
(loader 'vm code)
(exec-vm 'vm)