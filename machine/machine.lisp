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

(defun compile-expr (vm expr)
	(let (comp)
		(cond	
			( (atom expr) (setf comp (format nil "(MOVE ~a R0)~%" expr)) )
			( t	
				(setf comp (concatenate 'string comp (compile-expr vm (second expr) )))
				(setf comp (concatenate 'string comp (format nil "(MOVE R0 R1)~%")))
				(setf comp (concatenate 'string comp (compile-expr vm (third expr) )))
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

(defun mdefun (nom params code)
	(set-Symb nom 'params params)
	(set-Symb nom 'nbP (list-length params))
	(set-Symb nom 'code code)
)
(defun compile-comp(vm expr)
	(let (comp)
		(write expr)
		(setf comp (concatenate 'string comp (compile-expr vm (second  expr)) ))
		(setf comp (concatenate 'string comp (format nil "(PUSH R0)~%")))
		(setf comp (concatenate 'string comp (compile-expr vm (third  expr)) ))
		(setf comp (concatenate 'string comp (format nil "(MOVE R0 R1)~%")))
		(setf comp (concatenate 'string comp (format nil "(POP R2)~%")))
		(setf comp (concatenate 'string comp (format nil "(CMP R1 R2)~%")))

		(cond
			( (equal (first expr) '<)
				(setf comp (concatenate 'string comp (format nil "(JPP CMPTRUE) ~%")))
			)
			( (equal (first expr) '>) ()
				(setf comp (concatenate 'string comp (format nil "(JPG CMPTRUE) ~%")))
			)
			( (equal (first expr) '=) ()
				(setf comp (concatenate 'string comp (format nil "(JEQ CMPTRUE) ~%")))
			) 
			( (equal (first expr) '<=) ()
				(setf comp (concatenate 'string comp (format nil "(JPE CMPTRUE) ~%")))
			) 
			( (equal (first expr) '>=) ()
				(setf comp (concatenate 'string comp (format nil "(JGE CMPTRUE) ~%")))
			)
		)

		(setf comp (concatenate 'string comp (format nil "(MOVE 0 R0) ~%")))
		(setf comp (concatenate 'string comp (format nil "(JMP IFSUITE) ~%")))
		(setf comp (concatenate 'string comp (format nil "(LABEL CMPTRUE) ~%")))
		(setf comp (concatenate 'string comp (format nil "(MOVE 1 R0) ~%")))


		(return-from compile-comp comp)
	)

)
; ordre execution :
; 	print CMP
; 	stocker false
; 	jump cond pc + taille false
; 	print false
; 	stocke true
; 	jump pc + taille true
; 	print true
(defun compile-if (vm expr)

	(let (comp true false length_t length_f) 
		; in first arg of if (bool)
		(setf comp (concatenate 'string comp (compile-comp vm (second expr))) )
		(setf comp (concatenate 'string comp (format nil "(LABEL IFSUITE) ~%")))

		(setf comp (concatenate 'string comp (format nil "(CMP 1 R0) ~%")))

		(setq true (compile-expr vm (third expr)))
		(setf length_t (count #\newline true))
		(setq false (compile-expr vm (fourth expr)))
		(setf length_f (count #\newline false))
		;;jump taille false plus 1 pour sauter le 'jump pc +taille true'
		
		(setf comp (concatenate 'string comp (format nil "(JEQ ~a) ~%" (+ length_f 1))))
		(setf comp (concatenate 'string comp (format nil "~a" false)));;print false
		(setf comp (concatenate 'string comp (format nil "(JMP ~a) ~%" length_t)))
		(setf comp (concatenate 'string comp (format nil "~a" true)));;print true
		(return-from compile-if comp)
	)
	
)
;;Compilation appel de fct 
(defun compile-fctcall (vm call)
	(let (comp nbParam)
		(setf nbParam 0)
		(loop for arg in (cdr call) do 
			(setf comp (concatenate 'string comp (compile-expr vm arg)))
			(setf comp (concatenate 'string comp (format nil "(PUSH R0) ~%")))
			(setf nbParam (+ nbParam 1))
		)
		(setf comp (concatenate 'string comp (format nil "(PUSH ~a) ~%" nbParam)))
		(setf comp (concatenate 'string comp (format nil "(MOVE FP R1) ~%" )))
		(setf comp (concatenate 'string comp (format nil "(MOVE SP FP) ~%" )))
		(setf comp (concatenate 'string comp (format nil "(MOVE SP R2) ~%" )))
		(setf comp (concatenate 'string comp (format nil "(SUB ~a R2) ~%" nbParam)))
		(setf comp (concatenate 'string comp (format nil "(DECR R2) ~%" )))
		(setf comp (concatenate 'string comp (format nil "(PUSH R2) ~%" )))
		(setf comp (concatenate 'string comp (format nil "(PUSH R1) ~%" )))

		(setf comp (concatenate 'string comp (format nil "(JSR ~a) ~%" (car call))))

		(setf comp (concatenate 'string comp (format nil "(POP R1) ~%" )))
		(setf comp (concatenate 'string comp (format nil "(POP R2) ~%" )))

		(setf comp (concatenate 'string comp (format nil "(MOVE R1 FP) ~%" )))
		(setf comp (concatenate 'string comp (format nil "(POP R2 SP) ~%" )))
		(return-from compile-fctcall comp)
	)

)
;;Disponible dans la documentation sur les strings
;; Ne fait pas parti des ANSI Standard
(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos)))
(defun replace-params-reg (string_ list_ )
	(let (part replacement comp cpt)
		(setf cpt 0)
		(setf comp string_)
		(loop for arg in list_ do 
			(setf part (format nil " ~a " arg))
			(setf cpt (+ cpt 1))
			(setf replacement (format nil " R~a " cpt))
			(setf comp (replace-all comp part replacement ))
		)
		(return-from replace-params-reg comp)
	)
)
(defun compile-fct (fct)
	(let (comp)
		(setf comp (concatenate 'string comp (format nil "(LABEL ~a) ~%" fct)))
		(setf comp (concatenate 'string comp (replace-params-reg (compile (get fct 'code)) (get fct 'params) ) ))
		(setf comp (concatenate 'string comp (format nil "(RTN) ~%" fct)))
	)
)

(defun compile_(fct))