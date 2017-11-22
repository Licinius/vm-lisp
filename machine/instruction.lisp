(require "machine.lisp")

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
	(setf (aref (get vm 'mem) adr) (get vm P))
)

; INCR P
(defun vm-incr (vm P)
	(setf (get vm P) (+ (get vm P) 1))
)

; DECR P
(defun vm-decr (vm P)
	(setf (get vm P) (- (get vm P) 1))
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

; PUSH P1
(defun vm-push (vm P1)
	(vm-decr (get vm 'SP))
	(vm-store P1 (get vm 'SP))
)

; POP P1
(defun vm-pop (vm P1)
	(vm-load (get vm 'SP) P1)
	(vm-decr (get vm 'SP))
)

; JPG etiq
(defun vm-jpg (vm etiq)
	(if (equal (get vm 'DPG) 1)
		(vm-jmp etiq)
	)
)

; JEQ etiq
(defun vm-jeq (vm etiq)
	(if (equal (get vm 'DEQ) 1)
		(vm-jmp etiq)
	)
)

; JPP etiq
(defun vm-jpp (vm etiq)
	(if (equal (get vm 'DPP) 1)
		(vm-jmp etiq)
	)
)

; JGE etiq
(defun vm-jge (vm etiq)
	(if (or (equal (get vm 'DPG) 1) (equal (get vm 'DEQ) 1))
		(vm-jmp etiq)
	)
)

; JPE etiq
(defun vm-jpe (vm etiq)
	(if (or (equal (get vm 'DPP) 1) (equal (get vm 'DEQ) 1))
		(vm-jmp etiq)
	)
)

; JMP etiq
(defun vm-jmp (vm etiq)
	(setf (get vm 'PC) (gethash etiq (get vm 'labels)))
)

; JSR etiq
(defun vm-jsr (vm etiq)
	(vm-push (get vm 'RA))
	(vm-jmp etiq)
)

; RTN
(defun vm-rtn (vm)
	(vm-pop P1)
	(vm-jmp P1)
)

; CMP P1 P2
(defun vm-cmp-atom (vm P1 P2)
	(if (atom P1)
		(cond
			((equal (get vm P2) (get vm P1)) (set-flag-DEQ vm))
			((< (get vm P2) (get vm P1)) (set-flag-DPP vm))
			((> (get vm P2) (get vm P1)) (set-flag-DPG vm))
		)
		(vm-cmp-reg vm P1 P2)
	)
) 

(defun vm-cmp-reg (vm P1 P2)
	(if (atom P1)
		(vm-cmp-atom vm P1 P2)
		(cond
			((equal (get vm P2) (get vm P1)) (set-flag-DEQ vm))
			((< (get vm P2) (get vm P1)) (set-flag-DPP vm))
			((> (get vm P2) (get vm P1)) (set-flag-DPG vm))
		)

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


;;A copier dans machine.lisp
(defun set-flag-DEQ (vm)
	(set-Symb vm 'DEQ 1)
	(set-Symb vm 'DPG 0)
	(set-Symb vm 'DPP 0)
)

(defun set-flag-DPG (vm)
	(set-Symb vm 'DEQ 0)
	(set-Symb vm 'DPG 1)
	(set-Symb vm 'DPP 0)
)

(defun set-flag-DPP (vm)
	(set-Symb vm 'DEQ 0)
	(set-Symb vm 'DPG 0)
	(set-Symb vm 'DPP 1)
)