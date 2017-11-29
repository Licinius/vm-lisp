(defun set-Symb (vm nom val) (setf (get vm nom) val) )
(defun init-mem (nom &optional(taille 1000)) (set-Symb nom 'mem (make-array taille)) )
(defun set-flag-init (vm)
	(set-Symb vm 'DEQ 0)
	(set-Symb vm 'DPG 0)
	(set-Symb vm 'DPP 0)

)

(defun make-vm (nom &optional (taille 1000))
	(init-mem nom taille)

	(set-Symb nom 'labels (make-hash-table))
	(set-Symb nom 'SP taille)
	(set-Symb nom 'RA 0)
	(set-Symb nom 'PC 0)
	
	;; Flag
	(set-flag-init nom)

	;; Register
	(set-Symb nom 'R0 0)
	(set-Symb nom 'R1 0)
	(set-Symb nom 'R2 0)

	;; VM State (0 on, 1 off)
	(set-Symb nom 'state 0)
)

(defun loader (vm instr &optional(ptr 0))
	(cond
		((not (equal (car instr) NIL))
			(cond
				(
					(equal(caar instr) "LABEL")
						(setf (gethash (cadar instr) (get vm 'labels)) ptr)
						(loader vm (cdr instr) ptr)
				)
				(
					(setf (aref (get vm 'mem) ptr) (car instr))
					(loader vm (cdr instr) (+ ptr 1))
				)
			)
		)
	)
)

(defun set-flag-DEQ (vm)
	(set-flag-init vm)
	(set-Symb vm 'DEQ 1)
)

(defun set-flag-DPG (vm)
	(set-flag-init vm)
	(set-Symb vm 'DPG 1)
)

(defun set-flag-DPP (vm)
	(set-flag-init vm)
	(set-Symb vm 'DPP 1)
)

(defun exec-vm (vm)
	(loop while (not (or (equal (aref (get vm 'mem) (get vm 'PC)) NIL) (equal (get vm 'state) 1)))
		do
			(write (get vm 'PC))
			(cond 
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) "MOVE") (vm-move vm (nth 1 (aref (get vm 'mem) (get vm 'PC))) (nth 2 (aref (get vm 'mem) (get vm 'PC)))) )
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) "LOAD") (vm-load vm (nth 1 (aref (get vm 'mem) (get vm 'PC))) (nth 2 (aref (get vm 'mem) (get vm 'PC)))) )
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) "STORE") (vm-store vm (nth 1 (aref (get vm 'mem) (get vm 'PC))) (nth 2 (aref (get vm 'mem) (get vm 'PC)))) )
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) "INCR") (vm-incr vm (nth 1 (aref (get vm 'mem) (get vm 'PC)))) )
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) "DECR") (vm-decr vm (nth 1 (aref (get vm 'mem) (get vm 'PC)))) )
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) "ADD") (vm-add vm (nth 1 (aref (get vm 'mem) (get vm 'PC))) (nth 2 (aref (get vm 'mem) (get vm 'PC)))) )
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) "SUB") (vm-sub vm (nth 1 (aref (get vm 'mem) (get vm 'PC))) (nth 2 (aref (get vm 'mem) (get vm 'PC)))) )
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) "MULT") (vm-mult vm (nth 1 (aref (get vm 'mem) (get vm 'PC))) (nth 2 (aref (get vm 'mem) (get vm 'PC)))) )
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) "DIV") (vm-div vm (nth 1 (aref (get vm 'mem) (get vm 'PC))) (nth 2 (aref (get vm 'mem) (get vm 'PC)))) )
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) "PUSH") (vm-push vm (nth 1 (aref (get vm 'mem) (get vm 'PC)))) )
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) "POP") (vm-pop vm (nth 1 (aref (get vm 'mem) (get vm 'PC)))) )
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) "JPG") (vm-jpg vm (nth 1 (aref (get vm 'mem) (get vm 'PC)))) )
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) "JEQ") (vm-jeq vm (nth 1 (aref (get vm 'mem) (get vm 'PC)))) )
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) "JPP") (vm-jpp vm (nth 1 (aref (get vm 'mem) (get vm 'PC)))) )
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) "JGE") (vm-jge vm (nth 1 (aref (get vm 'mem) (get vm 'PC)))) )
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) "JPE") (vm-jpe vm (nth 1 (aref (get vm 'mem) (get vm 'PC)))) )
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) "JMP") (vm-jmp vm (nth 1 (aref (get vm 'mem) (get vm 'PC)))) )
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) "RTN") (vm-rtn vm) )
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) "CMP") (vm-cmp vm (nth 1 (aref (get vm 'mem) (get vm 'PC))) (nth 2 (aref (get vm 'mem) (get vm 'PC)))) )
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) "HALT") (vm-halt vm) )
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) "NOP") (vm-nop vm))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) "CAR") (vm-car vm))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) "CDR") (vm-cdr vm))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) "CONS") (vm-cons vm (nth 1 (aref (get vm 'mem) (get vm 'PC))) (nth 2 (aref (get vm 'mem) (get vm 'PC)))) )
		)
		(incf (get vm 'PC))
	)
	"La VM est fermé"
)