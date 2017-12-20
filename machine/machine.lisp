(require "instruction.lisp")

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
		(cond 
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'MOVE)	(vm-move	vm	(nth 1 (aref (get vm 'mem) (get vm 'PC))) (nth 2 (aref (get vm 'mem) (get vm 'PC)))	))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'LOAD)	(vm-load	vm	(nth 1 (aref (get vm 'mem) (get vm 'PC))) (nth 2 (aref (get vm 'mem) (get vm 'PC)))	))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'STORE)	(vm-store	vm	(nth 1 (aref (get vm 'mem) (get vm 'PC))) (nth 2 (aref (get vm 'mem) (get vm 'PC)))	))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'INCR)	(vm-incr	vm	(nth 1 (aref (get vm 'mem) (get vm 'PC)))											))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'DECR)	(vm-decr	vm	(nth 1 (aref (get vm 'mem) (get vm 'PC)))											))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'ADD)	(vm-add		vm	(nth 1 (aref (get vm 'mem) (get vm 'PC))) (nth 2 (aref (get vm 'mem) (get vm 'PC)))	))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'SUB)	(vm-sub		vm	(nth 1 (aref (get vm 'mem) (get vm 'PC))) (nth 2 (aref (get vm 'mem) (get vm 'PC)))	))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'MULT)	(vm-mult	vm	(nth 1 (aref (get vm 'mem) (get vm 'PC))) (nth 2 (aref (get vm 'mem) (get vm 'PC)))	))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'DIV)	(vm-div		vm	(nth 1 (aref (get vm 'mem) (get vm 'PC))) (nth 2 (aref (get vm 'mem) (get vm 'PC)))	))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'PUSH)	(vm-push	vm	(nth 1 (aref (get vm 'mem) (get vm 'PC)))											))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'POP)	(vm-pop		vm	(nth 1 (aref (get vm 'mem) (get vm 'PC)))											))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'JPG)	(vm-jpg		vm	(nth 1 (aref (get vm 'mem) (get vm 'PC)))											))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'JEQ)	(vm-jeq		vm	(nth 1 (aref (get vm 'mem) (get vm 'PC)))											))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'JPP)	(vm-jpp		vm	(nth 1 (aref (get vm 'mem) (get vm 'PC)))											))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'JGE)	(vm-jge		vm	(nth 1 (aref (get vm 'mem) (get vm 'PC)))											))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'JPE)	(vm-jpe		vm	(nth 1 (aref (get vm 'mem) (get vm 'PC)))											))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'JMP)	(vm-jmp		vm	(nth 1 (aref (get vm 'mem) (get vm 'PC)))											))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'JSR)	(vm-jsr		vm	(nth 1 (aref (get vm 'mem) (get vm 'PC)))											))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'RTN)	(vm-rtn		vm 																						))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'CMP)	(vm-cmp		vm	(nth 1 (aref (get vm 'mem) (get vm 'PC))) (nth 2 (aref (get vm 'mem) (get vm 'PC)))	))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'HALT)	(vm-halt	vm																						))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'NOP)	(vm-nop		vm																						))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'CAR)	(vm-car		vm	(nth 1 (aref (get vm 'mem) (get vm 'PC)))											))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'CDR)	(vm-cdr		vm	(nth 1 (aref (get vm 'mem) (get vm 'PC)))											))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'CONS)	(vm-cons	vm	(nth 1 (aref (get vm 'mem) (get vm 'PC))) (nth 2 (aref (get vm 'mem) (get vm 'PC)))	))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'WRITE)	(vm-write	vm	(nth 1 (aref (get vm 'mem) (get vm 'PC)))											))
		)
		(incf (get vm 'PC))
	)
	"Fin de VM"
)

(defun compile-expr (expr)
	(let (comp)
		(cond	
			( (atom expr) (setf comp (format nil "(MOVE ~a R0)~%" expr)) )
			( t	
				(setf comp (concatenate 'string comp (compile-expr (second expr) )))
				(setf comp (concatenate 'string comp (format nil "(MOVE R0 R1)~%")))
				(setf comp (concatenate 'string comp (compile-expr (third expr) )))
				(setf comp (concatenate 'string comp (format nil "(MOVE R0 R2)~%")))
				(cond
					( (equal (first expr) '+) (setf comp (concatenate 'string comp (format nil "(~a R1 R2)~%" 'ADD)))	)
					( (equal (first expr) '-) (setf comp (concatenate 'string comp (format nil "(~a R1 R2)~%" 'SUB)))	)
					( (equal (first expr) '*) (setf comp (concatenate 'string comp (format nil "(~a R1 R2)~%" 'MULT)))	)
					( (equal (first expr) '/) (setf comp (concatenate 'string comp (format nil "(~a R1 R2)~%" 'DIV)))	)
				)
				(setf comp (concatenate 'string comp (format nil "(MOVE R2 R0)~%")))
			)
		)
		(return-from compile-expr comp)
	)
)

(defun readFile (path)
	(let ( (fin (open path :if-does-not-exist :error :direction :input)) )
		(setf obj '())
		(setf line (read fin nil nil nil))
		(loop while (not (equal line nil)) do
			(write line)
			(setq obj (append obj (list line)))
			(setf line (read fin nil nil nil))
		)
		(close fin)
		(return-from readFile obj)
	)
)

(defun writeFile (path str)
	(let ((fout (open path :if-does-not-exist :create :if-exists :supersede :direction :io)))
		(format fout str)
		(close fout)
	)
)

(defun mdefun (nom code)
	(set-Symb nom 'code code)
	(set-Symb nom 'comp (compile code) )
)


; ordre execution :
; 	print CMP
; 	stocker false
; 	jump cond pc + taille false
; 	print false
; 	stocke true
; 	jump pc + taille true
; 	print true
(defun compile-if (expr)


	(let (true false length_t length_f) 
		; in first arg of if (bool)
		(setf comp (concatenate 'string comp (compile-expr (second (first expr))) ))
		(setf comp (concatenate 'string comp (format nil "(PUSH R0)~%")))
		(setf comp (concatenate 'string comp (compile-expr (third (first expr))) ))
		(setf comp (concatenate 'string comp (format nil "(MOVE R0 R1)~%")))
		(setf comp (concatenate 'string comp (format nil "(POP R2)~%")))
		(setf comp (concatenate 'string comp (format nil "(CMP R1 R2)~%")))
		
		(setq true (compile (third expr)))
		(setf length_t (list-length true))
		(setq false (compile (fourth expr)))
		(setf length_f (list-length false))
		;;jump taille false plus 1 pour sauter le 'jump pc +taille true'
		(cond
			( (equal (first (second expr)) '<)
				(setf comp (concatenate 'string comp (format nil "(JPP ~a)~%" somewhere)))
			)
			( (equal (first (second expr)) '>) ()
				(setf comp (concatenate 'string comp (format nil "(JPG ~a)~%" somewhere)))
			)
			( (equal (first (second expr)) '=) ()
				(setf comp (concatenate 'string comp (format nil "(JEQ ~a)~%" somewhere)))
			) 
			( (equal (first (second expr)) '<=) ()
				(setf comp (concatenate 'string comp (format nil "(JPE ~a)~%" somewhere)))
			) 
			( (equal (first (second expr)) '>=) ()
				(setf comp (concatenate 'string comp (format nil "(JGE ~a)~%" somewhere)))
			)
		)
		(format nil "~a ~%" false);;print false
		;; jump après le true ---ici---
		(format nil "~a ~%" true);;print true
	)
	
)