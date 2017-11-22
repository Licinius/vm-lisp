(defun set-Symb (vm nom val) (setf (get vm nom) val) )
(defun init-mem (nom &optional(taille 1000)) (set-Symb nom 'mem (make-array taille :initial-element 0)) )

(defun make-vm (nom &optional (taille 1000))
	(init-mem nom taille)

	(set-Symb nom 'labels (make-hash-table))
	(set-Symb nom 'SP taille)
	(set-Symb nom 'RA 0)
	(set-Symb nom 'PC 0)
	
	;; Flag
	(set-Symb nom 'DPG 0)
	(set-Symb nom 'DEQ 0)
	(set-Symb nom 'DPP 0)

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