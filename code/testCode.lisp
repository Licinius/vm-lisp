(cl-user::quit)
(setf factFunction
	'((defun fact (n)
		(
			(if (< n 2)
				1
				(* n (fact (- n 1)) )
			)
		)
	)
	(fact 6)
	)
)
(setf fiboFunction
	'((defun fibo (n)
		(
			(if (< n 2)
				n
				(+ (fibo(- n 1)) (fibo (- n 2) ))
			)
		)
	)
	(fibo 12)
	)
)


(setf testLoop 
	'( (defun loop (n)
			((while (< n 6)
				(setf n (+ n 1))
				)
				n
			)

		)
	 (loop 1)
	)

)

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
(compile-load-exec 'vm fiboFunction)
(compile-load-exec 'vm testFunction)