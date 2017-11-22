; MOVE P1 P2
(defun MOVE (vm P1 P2)
	(if (atom P1)
		(setf (get vm P2) P1)
		(setf (get vm P2) (get vm P1))
	)
)

; LOAD adr P
(defun LOAD (vm adr P)
	(setf (get vm P) (aref (get vm 'mem) adr))
)

; STORE P adr
(defun STORE (vm P adr)
	(setf (aref (get vm 'mem) adr) (get vm P))
)

; INCR P
(defun INCR (vm P)
	(setf (get vm P) (+ (get vm P) 1))
)

; DECR P
(defun DECR (vm P)
	(setf (get vm P) (- (get vm P) 1))
)

; ADD P1 P2
(defun ADD (vm P1 P2)
	(setf (get vm P2) (+ (get vm P2) (get vm P1)))
)

; SUB P1 P2
(defun SUB (vm P1 P2)
	(setf (get vm P2) (- (get vm P2) (get vm P1)))
)

; MULT P1 P2
(defun MULT (vm P1 P2)
	(setf (get vm P2) (* (get vm P2) (get vm P1)))
)

; DIV P1 P2
(defun DIV (vm P1 P2)
	(if (not (equal (get vm P1) 0))
		(setf (get vm P2) (/ (get vm P2) (get vm P1)))
	)
)

; PUSH P1
(defun PUSH (vm P1)
	(DECR (get vm 'SP))
	(STORE P1 (get vm 'SP))
)

; POP P1
(defun POP (vm P1)
	(LOAD (get vm 'SP) P1)
	(INCR (get vm 'SP))
)

; JPG etiq
(defun JPG (vm etiq)
	(if (equal (get vm 'DPG) 1)
		(JMP etiq)
	)
)

; JEQ etiq
(defun JEQ (vm etiq)
	(if (equal (get vm 'DEQ) 1)
		(JMP etiq)
	)
)

; JPP etiq
(defun JPP (vm etiq)
	(if (equal (get vm 'DPP) 1)
		(JMP etiq)
	)
)

; JGE etiq
(defun JGE (vm etiq)
	(if (or (equal (get vm 'DPG) 1) (equal (get vm 'DEQ) 1)
		(JMP etiq)
	)
)

; JPE etiq
(defun JPE (vm etiq)
	(if (or (equal (get vm 'DPP) 1) (equal (get vm 'DEQ) 1)
		(JMP etiq)
	)
)

; JMP etiq
(defun JMP (vm etiq)
	(setf (get vm 'PC) (gethash etiq (get vm 'labels)))
)

; JSR etiq
(defun JSR (vm etiq)
	(PUSH (get vm 'RA))
	(JMP etiq)
)

; RTN
(defun RTN (vm)
	(POP P1)
	(JMP P1)
)

; CMP P1 P2
(defun CMP (vm P1 P2) 
	(cond
		((equal (get vm P2) (get vm P1)) (setf (get vm 'DEQ) 1))
		((lt (get vm P2) (get vm P1)) (setf (get vm 'DPP) 1))
		((gt (get vm P2) (get vm P1)) (setf (get vm 'DPG) 1))
	)
) 
