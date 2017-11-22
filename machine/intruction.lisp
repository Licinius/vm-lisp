; make-vm
(defun make-vm (nom &optional(taille 250))
	(set-Symb nom 'mem (make-array taille))
	(set-Symb nom 'labels (make-hash-table))
	(set-Symb nom 'SP taille)
	(set-Symb nom 'RA 0)
	(set-Symb nom 'PC 0)
	
	;Flag
	(set-Symb nom 'DPG 0)
	(set-Symb nom 'DEQ 0)
	(set-Symb nom 'DPP 0)

	(set-Symb nom 'R0 0)

	(set-Symb nom 'inst 0)
)

; set-Symb
(defun set-Symb (vm nom val) (setf (get vm nom) val) )

; ------------------------------- VM FUNCTIONS -------------------------------

; MOVE R1 R2
(defun MOVE (vm R1 R2)
	(if (atom R1)
		(setf (get vm R2) R1)
		(setf (get vm R2) (get vm R1))
	)
)

; LOAD adr R
(defun LOAD (vm adr R)
	(setf (get vm R) (aref (get vm 'mem) adr))
)

; STORE R adr
(defun STORE (vm R adr)
	(setf (aref (get vm 'mem) adr) (get vm R))
)

; INCR R
(defun INCR (vm R)
	(setf (get vm R) (+ (get vm R) 1))
)

; DECR R
(defun DECR (vm R)
	(setf (get vm R) (- (get vm R) 1))
)

; ADD R1 R2
(defun ADD (vm R1 R2)
	(setf (get vm R2) (+ (get vm R2) (get vm R1)))
)

; SUB R1 R2
(defun SUB (vm R1 R2)
	(setf (get vm R2) (- (get vm R2) (get vm R1)))
)

; MULT R1 R2
(defun MULT (vm R1 R2)
	(setf (get vm R2) (* (get vm R2) (get vm R1)))
)

; DIV R1 R2
(defun DIV (vm R1 R2)
	(if (not (equal (get vm R1) 0))
		(setf (get vm R2) (/ (get vm R2) (get vm R1)))
	)
)

; PUSH R
(defun PUSH (vm R)
	(DECR (get vm 'SP))
	(STORE R (get vm 'SP))
)

; POP R
(defun POP (vm R)
	(LOAD (get vm 'SP) R)
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
	(POP R)
	(JMP R)
)

; CMP R1 R2
(defun CMP (vm R1 R2) 
	(cond
		((equal (get vm R2) (get vm R1)) (setf (get vm 'DEQ) 1))
		((lt (get vm R2) (get vm R1)) (setf (get vm 'DPP) 1))
		((gt (get vm R2) (get vm R1)) (setf (get vm 'DPG) 1))
	)
) 
