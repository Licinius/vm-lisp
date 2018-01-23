(cl-user::quit)
(require "machine.lisp")
(format nil "Test factoriel ~%" )
;;Ici peuvent commencer les tests
(make-vm 'vm 1000)

(setf factFunction '((defun fact (n) ((if (< n 2) 1 (* n (fact (- n 1)) ) ) ) ) (fact 6) ) )
(compile-load-exec 'vm factFunction)
(format nil "6! -> Resultat attendu : 720 Resultat obtenu : ~a ~%" (get 'vm 'R0))
;;========================================;;
(make-vm 'vm 1000)
(setf factFunction '((defun fact (n) ((if (< n 2) 1 (* n (fact (- n 1)) ) ) ) ) (fact 13) ) ) 
(compile-load-exec 'vm factFunction)
(format nil "13! -> Resultat attendu : 6227020800 Resultat obtenu : ~a ~%" (get 'vm 'R0))
;;========================================;;
(make-vm 'vm 1000)
(setf factFunction '((defun fact (n) ((if (< n 2) 1 (* n (fact (- n 1)) ) ) ) ) (fact 3) ) ) 
(compile-load-exec 'vm factFunction)
(format nil "3! -> Resultat attendu : 6 Resultat obtenu : ~a ~%" (get 'vm 'R0))
;;========================================;;

(format nil "Test fibonacci ~%" )
(make-vm 'vm 1000)
(setf fiboFunction '((defun fibo (n) ((if (< n 2) n (+ (fibo(- n 1)) (fibo (- n 2) )) ) ) ) (fibo 12) ) )
(compile-load-exec 'vm fiboFunction)
(format nil "fibo(12) -> Resultat attendu : 144 Resultat obtenu : ~a ~%" (get 'vm 'R0))
;;========================================;;
(make-vm 'vm 1000)
(setf fiboFunction '((defun fibo (n) ((if (< n 2) n (+ (fibo(- n 1)) (fibo (- n 2) )) ) ) ) (fibo 3) ) )
(compile-load-exec 'vm fiboFunction)
(format nil "fibo(3) -> Resultat attendu : 3 Resultat obtenu : ~a ~%" (get 'vm 'R0))

;;========================================;;
(format nil "Test (plus (moins)) ~%" )
(make-vm 'vm 1000)
(setf testFunction1 '((defun plus (n m) ((+ n m) ) ) (defun moins (n m) ((- n m) ) ) (plus 5 (moins 20 10)) ) ) 
(compile-load-exec 'vm testFunction1)
(format nil "(plus 5 (moins 20 10)) -> Resultat attendu : 15 Resultat obtenu : ~a ~%" (get 'vm 'R0))

;;(setf testLoop '( (defun loop (n) ((while (< n 6) (setf n (+ n 1)) ) n ) ) (loop 1) ) )