(require "machine.lisp")

(make-vm 'vm 100)

(setf test1
	'( 
		(MOVE 2 R0)
		(MOVE R0 R1)
		(MOVE 3 R0)
		(MOVE R0 R2)
		(MULT R1 R2)
		(MOVE R2 R0)
		(MOVE R0 R1)
		(MOVE 4 R0)
		(MOVE R0 R2)
		(ADD R1 R2)
		(MOVE R2 R0)
	)
)

(setf fact
	'(
		(WRITE "FACT(")
		(WRITE R1)
		(WRITE ") = ")
		(JSR FACT)
		(WRITE R0)
		(HALT)

	(LABEL FACT)
		(MOVE R1 R0)
	(LABEL BOUCLE)
		(CMP 2 R1)
		(JPP END)
		(DECR R1)
		(MULT R1 R0)
		(JMP BOUCLE)

	(LABEL END)
		(RTN)
	)
)

(MOVE 2 R1)
(MULT 3 R1)
(ADD 4 R1)


(loader 'vm test1)
(setf (get 'vm 'R1) 4)

; (get 'vm 'labels)
; (get 'vm 'mem)

(exec-vm 'vm)



(setf expr '(+ (* 2 3) 4))
(write (first expr))
(write (second expr))
(write (third expr))
(eql (nth 0 expr) +)

(compile-expr expr)

(defun compile-expr2(expr)
	(cond	
		( (atom expr) (format t "~a " expr) )
		( t	(cond
				( (equal (first expr) '+) (format t "~a ~%" 'ADD) (compile-expr (second expr) ) (compile-expr (third expr)) )
				( (equal (first expr) '-) (format t "~a ~%" 'SUB) (compile-expr (second expr) ) (compile-expr (third expr) ) )
				( (equal (first expr) '*) (format t "~a ~%" 'MULT) (compile-expr (second expr) ) (compile-expr (third expr) ) )
				( (equal (first expr) '/) (format t "~a ~%" 'DIV) (compile-expr (second expr) ) (compile-expr (third expr) ) )
			)
		)
	)
)



(defun compile-expr (expr)
	(cond	
		( (atom expr) (format t "MOVE ~a R0~%" expr) )
		( t	
			(compile-expr (second expr))
			(format t "MOVE R0 R1~%") 
			(compile-expr (third expr)) 
			(format t "MOVE R0 R2~%")
			(cond
				( (equal (first expr) '+) (format t "~a R1 R2~%" 'ADD) )
				( (equal (first expr) '-) (format t "~a R1 R2~%" 'SUB) )
				( (equal (first expr) '*) (format t "~a R1 R2~%" 'MULT))
				( (equal (first expr) '/) (format t "~a R1 R2~%" 'DIV) )
			)
			(format t "MOVE R2 R0~%")
		)
	)
)




(setf code '())

(let ((in (open "../code/ASM.lisp" :if-does-not-exist nil)))
  (when in
    (loop do 
    	(setf cpt 0)
    	(setf line (read in nil))
    	(format t "~a ~%" line)
    	; (setq code (cons (cdr code) line))
    	(setq code (append code line))
    	(setf cpt (+ cpt 1))
        while (not (equal line nil))
     )
    (close in)
    )
)