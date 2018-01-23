; MOVE P1 P2
(defun vm-move (vm P1 P2)
	(if (symbolp P1)
		(setf (get vm P2) (get vm P1))
		(setf (get vm P2) P1)
	)
)

; LOAD adr P
(defun vm-load (vm adr P)
	(setf (get vm P) (aref (get vm 'mem) adr))
)

; STORE P adr
(defun vm-store (vm P adr)
	(if (symbolp P)
		(setf (aref (get vm 'mem) adr) (get vm P))
		(setf (aref (get vm 'mem) adr) P)
	)
)

; INCR P
(defun vm-incr (vm P)
	(incf (get vm P))
)

; DECR P
(defun vm-decr (vm P)
	(decf (get vm P))
)

; ADD P1 P2
(defun vm-add (vm P1 P2)
	(setf (get vm P2) (+ (get vm P1) (get vm P2)))
)

; SUB P1 P2
(defun vm-sub (vm P1 P2)
	(setf (get vm P2) (- (get vm P1) (get vm P2)))
)

; MULT P1 P2
(defun vm-mult (vm P1 P2)
	(setf (get vm P2) (* (get vm P1) (get vm P2)))
)

; DIV P1 P2
(defun vm-div (vm P1 P2)
	(if (not (equal (get vm P1) 0))
		(setf (get vm P2) (/ (get vm P1) (get vm P2)))
	)
)

; PUSH P
(defun vm-push (vm P)
	(vm-decr vm 'SP)
	(vm-store vm P (get vm 'SP))
)

; POP P
(defun vm-pop (vm P)
	(vm-load vm (get vm 'SP) P)
	;;(write (format nil "POP ~a ~%" (get vm P)))
	(vm-incr vm 'SP)
)

; JPG etiq
(defun vm-jpg (vm etiq)
	(if (equal (get vm 'DPG) 1)
		(vm-jmp vm etiq)
	)
)

; JEQ etiq
(defun vm-jeq (vm etiq)
	(if (equal (get vm 'DEQ) 1)
		(vm-jmp vm etiq)
	)
)

; JPP etiq
(defun vm-jpp (vm etiq)
	(if (equal (get vm 'DPP) 1)
		(vm-jmp vm etiq)
	)
)

; JGE etiq
(defun vm-jge (vm etiq)
	(if (or (equal (get vm 'DPG) 1) (equal (get vm 'DEQ) 1))
		(vm-jmp vm etiq)
	)
)

; JPE etiq
(defun vm-jpe (vm etiq)
	(if (or (equal (get vm 'DPP) 1) (equal (get vm 'DEQ) 1))
		(vm-jmp vm etiq)
	)
)

; JMP etiq
(defun vm-jmp (vm etiq)
	(if (integerp etiq)
		(setf (get vm 'PC) (+ (get vm 'PC) etiq))
		(setf (get vm 'PC) (- (gethash etiq (get vm 'labels)) 1))
	)
)

; JSR etiq
(defun vm-jsr (vm etiq)
	(setf (get vm 'RA) (get vm 'PC))
	(vm-jmp vm etiq)
)

; RTN
(defun vm-rtn (vm)
	(setf (get vm 'PC) (get vm 'RA))
)

; CMP atom P2
(defun vm-cmp-atom (vm P1 P2)
	(cond
		((equal P1 (get vm P2)) (set-flag-DEQ vm))
		((< P1 (get vm P2)) (set-flag-DPP vm))
		((> P1 (get vm P2)) (set-flag-DPG vm))
	)
) 

; CMP P1 P2
(defun vm-cmp-reg (vm P1 P2)
	(cond
		((equal (get vm P1) (get vm P2)) (set-flag-DEQ vm))
		((< (get vm P1) (get vm P2)) (set-flag-DPP vm))
		((> (get vm P1) (get vm P2)) (set-flag-DPG vm))
	)
)

; CMP P1 P2
(defun vm-cmp (vm P1 P2)
	(if (symbolp P1)
		(vm-cmp-reg vm P1 P2)
		(vm-cmp-atom vm P1 P2)
	)
)

; HALT 
(defun vm-halt (vm)
	(set-Symb vm 'state 1)
)

; NOP
(defun vm-nop (vm))

; CAR P
(defun vm-car (vm P)
	(car (get vm P))
)

; CDR P
(defun vm-cdr (vm P)
	(cdr (get vm P))
)

; CONS P1 P2
(defun vm-cons (vm P1 P2)
	(setf (get vm P1) (cons (get vm P1) (get vm P2)))
)



; WRITE P
(defun vm-write (vm P)
	(if (symbolp P)
		(write (get vm P))
		(write P)
	)
)

; GET OFFSET P1 P2
(defun vm-get (vm OFFSET P1 P2)
	(vm-load vm (+ (get vm P1) OFFSET) P2)
	#| (write (format nil "GET ~a ~%" (get vm P2) )) |#
)

; SET P1 OFFSET P2
(defun vm-set (vm P1 OFFSET P2)
	(vm-store vm P1 (+ (get vm P2) OFFSET))
)