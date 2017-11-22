(defun set-Symb (vm nom val) (setf (get vm nom) val) )


(defun make-vm (nom &optional (taille 1000))
	(set-Symb nom 'mem (make-array taille :initial-element 0))
	;;(init-vm-mem (get nom 'mem))
	(set-Symb nom 'labels (make-hash-table))
	(set-Symb nom 'SP taille)
	(set-Symb nom 'RA 0)
	(set-Symb nom 'PC 0)
	
	;;Flag
	(set-Symb nom 'DPG 0)
	(set-Symb nom 'DEQ 0)
	(set-Symb nom 'DPP 0)

	(set-Symb nom 'R0 0)
	(set-Symb nom 'R1 0)
	(set-Symb nom 'R2 0)

	(set-Symb nom 'inst 0)

)

