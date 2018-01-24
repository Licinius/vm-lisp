(require "machine.lisp")
;;Ici peuvent commencer les tests

(setf factFunction6 '((defun fact (n) (if (< n 2) 1 (* n (fact (- n 1))) ) ) (fact 6)))
(setf factFunction13 '((defun fact (n) (if (< n 2) 1 (* n (fact (- n 1))) ) ) (fact 13)))
(setf factFunction3 '((defun fact (n) (if (< n 2) 1 (* n (fact (- n 1))) ) ) (fact 3)))
(setf fiboFunction12 '((defun fibo (n) (if (< n 2) n (+ (fibo(- n 1)) (fibo (- n 2) ))  ) ) (fibo 12) ) )
(setf fiboFunction3 '((defun fibo (n) (if (< n 2) n (+ (fibo(- n 1)) (fibo (- n 2) ))  ) ) (fibo 3) ) )
(setf plusMoinsFunction2010 '((defun plus (n m) (+ n m) ) (defun moins (n m) (- n m) ) (plus 5 (moins 20 10)) ) ) 

;;========================================;;
(make-vm 'vm 1000)
(compile-load-exec 'vm factFunction6)
(setf fact6 (get 'vm 'R0))
;;========================================;;
(make-vm 'vm 1000)
(compile-load-exec 'vm factFunction13)
(setf fact13 (get 'vm 'R0))
;;========================================;;
(make-vm 'vm 1000)
(compile-load-exec 'vm factFunction3)
(setf fact3 (get 'vm 'R0))
;;========================================;;
(make-vm 'vm 1000)
(compile-load-exec 'vm fiboFunction12)
(setf fibo12 (get 'vm 'R0))
;;========================================;;
(make-vm 'vm 1000)
(compile-load-exec 'vm fiboFunction3)
(setf fibo3 (get 'vm 'R0))
;;========================================;;
(make-vm 'vm 1000)
(compile-load-exec 'vm plusMoinsFunction2010)
(setf plusMoins2010 (get 'vm 'R0))
;;========================================;;

;;(setf testLoop '( (defun loop (n) ((while (< n 6) (setf n (+ n 1)) ) n ) ) (loop 1) ) )

(setf resTest (format nil "~%Test factoriel ~%"))
(setf resTest (concatenate 'string resTest (format nil "6! -> Resultat attendu : 720 Resultat obtenu : ~a ~%" fact6)))
(setf resTest (concatenate 'string resTest (format nil "13! -> Resultat attendu : 6227020800 Resultat obtenu : ~a ~%" fact13)))
(setf resTest (concatenate 'string resTest (format nil "3! -> Resultat attendu : 6 Resultat obtenu : ~a ~%" fact3)))

(setf resTest (concatenate 'string resTest (format nil "~%Test fibonacci ~%" )))
(setf resTest (concatenate 'string resTest (format nil "fibo(12) -> Resultat attendu : 144 Resultat obtenu : ~a ~%" fibo12)))
(setf resTest (concatenate 'string resTest (format nil "fibo(3) -> Resultat attendu : 2 Resultat obtenu : ~a ~%" fibo3)))

(setf resTest (concatenate 'string resTest (format nil "~%Test (plus (moins)) ~%" )))
(setf resTest (concatenate 'string resTest (format nil "(plus 5 (moins 20 10)) -> Resultat attendu : 15 Resultat obtenu : ~a ~%" plusMoins2010)))

resTest