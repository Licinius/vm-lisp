; MOVE R1 R2
(if (atom R1)
	(setf (get vm R2) R1)
	(setf (get vm R2) (get vm R1))
)

; LOAD adr R
(setf (get vm R) (aref (get vm 'mem) adr))

; STORE R adr
(setf (aref (get vm 'mem) adr) (get vm R))

; INCR R
(setf (get vm R) (+ (get vm R) 1))

; DECR R
(setf (get vm R) (- (get vm R) 1))

; ADD R1 R2
(setf (get vm R2) (+ (get vm R2) (get vm R1)))

; SUB R1 R2
(setf (get vm R2) (- (get vm R2) (get vm R1)))

; MULT R1 R2
(setf (get vm R2) (* (get vm R2) (get vm R1)))

; DIV R1 R2
(if (not (equal (get vm R1) 0))
	(setf (get vm R2) (/ (get vm R2) (get vm R1)))
)

; PUSH R
(DECR (get vm 'SP))
(STORE R (get vm 'SP))


; POP R
(LOAD (get vm 'SP) R)
(INCR (get vm 'SP))


; JPG etiq
(if (equal (get vm 'DPG) 1)
	(JMP etiq)
)

; JEQ etiq
(if (equal (get vm 'DEQ) 1)
	(JMP etiq)
)

; JPP etiq
(if (equal (get vm 'DPP) 1)
	(JMP etiq)
)

; JGE etiq
(if (or (equal (get vm 'DPG) 1) (equal (get vm 'DEQ) 1)
	(JMP etiq)
)

; JPE etiq
(if (or (equal (get vm 'DPP) 1) (equal (get vm 'DEQ) 1)
	(JMP etiq)
)

; JMP etiq
(setf (get vm 'PC) (gethash etiq (get vm 'labels)))

; JSR etiq
(PUSH (get vm 'RA))
(JMP etiq)


; RTN
(POP R)
(JMP R)

; CMP R1 R2
(cond
	((equal (get vm R2) (get vm R1)) (setf (get vm 'DEQ) 1))
	((lt (get vm R2) (get vm R1)) (setf (get vm 'DPP) 1))
	((gt (get vm R2) (get vm R1)) (setf (get vm 'DPG) 1))
)

