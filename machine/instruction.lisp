; MOVE P1 P2
(defun vm-move (vm P1 P2)
	(if (atom P1)
		(setf (get vm P2) P1)
		(setf (get vm P2) (get vm P1))
	)
)

; LOAD adr P
(defun vm-load (vm adr P)
	(setf (get vm P) (aref (get vm 'mem) adr))
)

; STORE P adr
(defun vm-store (vm P adr)
	(if (atom P)
		(setf (aref (get vm 'mem) adr) P)
		(setf (aref (get vm 'mem) adr) (get vm P))
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
	(setf (get vm P2) (+ (get vm P2) (get vm P1)))
)

; SUB P1 P2
(defun vm-sub (vm P1 P2)
	(setf (get vm P2) (- (get vm P2) (get vm P1)))
)

; MULT P1 P2
(defun vm-mult (vm P1 P2)
	(setf (get vm P2) (* (get vm P2) (get vm P1)))
)

; DIV P1 P2
(defun vm-div (vm P1 P2)
	(if (not (equal (get vm P1) 0))
		(setf (get vm P2) (/ (get vm P2) (get vm P1)))
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
	(vm-decr vm (get vm 'SP))
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
	(setf (get vm 'PC) (gethash etiq (get vm 'labels)))
)

; JSR etiq
(defun vm-jsr (vm etiq)
	(vm-push vm (get vm 'RA))
	(vm-jmp vm etiq)
)

; RTN
(defun vm-rtn (vm)
	(vm-pop vm (get vm 'R0))
	(vm-jmp vm (get vm 'R0))
)

; CMP P1 P2
(defun vm-cmp-atom (vm P1 P2)
	(cond
		((equal (get vm P2) P1) (set-flag-DEQ vm))
		((< (get vm P2) P1) (set-flag-DPP vm))
		((> (get vm P2) P1) (set-flag-DPG vm))
	)
) 

(defun vm-cmp-reg (vm P1 P2)
	(cond
		((equal (get vm P2) (get vm P1)) (set-flag-DEQ vm))
		((< (get vm P2) (get vm P1)) (set-flag-DPP vm))
		((> (get vm P2) (get vm P1)) (set-flag-DPG vm))
	)
) 
(defun vm-cmp (vm P1 P2)
	(if (atom P1)
		(vm-cmp-atom vm P1 P2)
		(vm-cmp-reg vm P1 P2)
	)
)
;;HALT 
(defun vm-halt (vm)
	(set-Symb vm 'state 1)
)

;;NOP
(defun vm-nop (vm))

;;CAR R
(defun vm-car (vm P)
	(car (get vm P))
)

;;CDR R
(defun vm-cdr (vm P)
	(cdr (get vm P))
)
;;CONS R1 R2
(defun vm-cons (vm P1 P2)
	(setf (get vm P1) (cons (get vm P1) (get vm P2)))
)


