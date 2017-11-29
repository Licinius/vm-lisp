(require "instruction.lisp")
(require "machine.lisp")

(make-vm 'vm 25)

(setf l1
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

(setf l2
	'( 
		("LABEL" FACT-S)
		("MOVE" 1 R2)
		("JMP" FACT)

		("LABEL" FACT)
		("CMP" 1 R1)
		("JPE" end)
		("MULT" R2 R1)
		("DECR" R1)
		("JMP" FACT)

		("LABEL" END)
		("MOVE" R2 R0)
	)
)

(loader 'vm l)

(get 'vm 'labels)

(get 'vm 'mem)

(exec-vm 'vm)
