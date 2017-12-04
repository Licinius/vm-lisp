(require "machine.lisp")

(make-vm 'vm 100)

(setf test1
	'( 
		("PUSH" 1)
		("JMP" YOHO)
		("PUSH" 2)
		("LABEL" YOHO)
		("PUSH" 3)
		("PUSH" 4)
		("HALT")
	)
)


(setf fact
	'( 
	("LABEL" FACT-S)
		("MOVE" 1 R2)
		("JMP" FACT)

	("LABEL" FACT)
		("CMP" 1 R1)
		("JPE" END)
		("MULT" R2 R1)
		("DECR" R1)
		("JMP" FACT)

	("LABEL" END)
		("MOVE" R2 R0)
	)
)

(setf fact2
	'(

		("WRITE" "FACT(")
		("WRITE" R1)
		("WRITE" ") = ")
		("JSR" FACT)
		("WRITE" "")
		("WRITE" R0)
		("HALT")

	("LABEL" FACT)
		("MOVE" R1 R0)
	("LABEL" BOUCLE)
		("CMP" 2 R1)
		("JPP" END)
		("DECR" R1)
		("MULT" R1 R0)
		("JMP" BOUCLE)

	("LABEL" END)
		("RTN")
	)
)


(loader 'vm fact2)
(setf (get 'vm 'R1) 4)
; fact(4)

; (get 'vm 'labels)
; (get 'vm 'mem)

(exec-vm 'vm)