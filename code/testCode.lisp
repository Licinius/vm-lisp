(setf test1
	'( 
		(PUSH 5)
		(PUSH 4)
		(PUSH 3)
		(PUSH 2)
		(PUSH 1)
		(PUSH 0)
		(SET 10 3 SP)
	)
)

(setf testFunction
	'((defun fact (n)
		(
			(if (< n 2)
				1
				(* n (fact (- n 1)) )
			)
		)
	)
	(fact 7)
	)
)
(cl-user::quit)


(require "machine.lisp")
(make-vm 'vm 1000)

; (get 'vm 'labels)
; (get 'vm 'mem)

(setf testFunction1
	'(
		(defun plus (n m)
			(
				(+ n m)
			)
		)
		(defun moins (n m)
			(
				(- n m)
			)
		)
		(plus 5 (moins 20 10))
	)
)
(compile-load 'vm testFunction1)
(exec-vm 'vm)
;;ou 

(compile-load-exec 'vm testFunction)