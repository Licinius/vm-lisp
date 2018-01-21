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
	(set-Symb nom 'FP taille)
	(set-Symb nom 'RA 0)
	(set-Symb nom 'PC 0)
	
	;; Flag
	(set-flag-init nom)

	;; Register
	(set-Symb nom 'R0 0)
	(set-Symb nom 'R1 0)
	(set-Symb nom 'R2 0)
	(set-Symb nom 'R3 0)
	(set-Symb nom 'R4 0)
	(set-Symb nom 'R5 0)

	;; VM State (0 on, 1 off)
	(set-Symb nom 'state 0)
)

(defun loader (vm instr &optional(ptr 0))
	(cond
		((not (equal (car instr) NIL))
			(cond
				(
					(equal(caar instr) 'LABEL)
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
		#| 		(write (format nil " trace : ~a ~%" (aref (get vm 'mem) (get vm 'PC)))) |##| Afficher la trace d'éxecution |#
		(cond 
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'MOVE)	(vm-move	vm	(nth 1 (aref (get vm 'mem) (get vm 'PC))) (nth 2 (aref (get vm 'mem) (get vm 'PC)))												))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'LOAD)	(vm-load	vm	(nth 1 (aref (get vm 'mem) (get vm 'PC))) (nth 2 (aref (get vm 'mem) (get vm 'PC)))												))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'STORE)	(vm-store	vm	(nth 1 (aref (get vm 'mem) (get vm 'PC))) (nth 2 (aref (get vm 'mem) (get vm 'PC)))												))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'INCR)	(vm-incr	vm	(nth 1 (aref (get vm 'mem) (get vm 'PC)))																						))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'DECR)	(vm-decr	vm	(nth 1 (aref (get vm 'mem) (get vm 'PC)))																						))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'ADD)	(vm-add		vm	(nth 1 (aref (get vm 'mem) (get vm 'PC))) (nth 2 (aref (get vm 'mem) (get vm 'PC)))												))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'SUB)	(vm-sub		vm	(nth 1 (aref (get vm 'mem) (get vm 'PC))) (nth 2 (aref (get vm 'mem) (get vm 'PC)))												))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'MULT)	(vm-mult	vm	(nth 1 (aref (get vm 'mem) (get vm 'PC))) (nth 2 (aref (get vm 'mem) (get vm 'PC)))												))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'DIV)	(vm-div		vm	(nth 1 (aref (get vm 'mem) (get vm 'PC))) (nth 2 (aref (get vm 'mem) (get vm 'PC)))												))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'PUSH)	(vm-push	vm	(nth 1 (aref (get vm 'mem) (get vm 'PC)))																						))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'POP)	(vm-pop		vm	(nth 1 (aref (get vm 'mem) (get vm 'PC)))																						))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'JPG)	(vm-jpg		vm	(nth 1 (aref (get vm 'mem) (get vm 'PC)))																						))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'JEQ)	(vm-jeq		vm	(nth 1 (aref (get vm 'mem) (get vm 'PC)))																						))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'JPP)	(vm-jpp		vm	(nth 1 (aref (get vm 'mem) (get vm 'PC)))																						))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'JGE)	(vm-jge		vm	(nth 1 (aref (get vm 'mem) (get vm 'PC)))																						))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'JPE)	(vm-jpe		vm	(nth 1 (aref (get vm 'mem) (get vm 'PC)))																						))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'JMP)	(vm-jmp		vm	(nth 1 (aref (get vm 'mem) (get vm 'PC)))																						))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'JSR)	(vm-jsr		vm	(nth 1 (aref (get vm 'mem) (get vm 'PC)))																						))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'RTN)	(vm-rtn		vm 																																	))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'CMP)	(vm-cmp		vm	(nth 1 (aref (get vm 'mem) (get vm 'PC))) (nth 2 (aref (get vm 'mem) (get vm 'PC)))												))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'HALT)	(vm-halt	vm																																	))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'NOP)	(vm-nop		vm																																	))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'CAR)	(vm-car		vm	(nth 1 (aref (get vm 'mem) (get vm 'PC)))																						))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'CDR)	(vm-cdr		vm	(nth 1 (aref (get vm 'mem) (get vm 'PC)))																						))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'CONS)	(vm-cons	vm	(nth 1 (aref (get vm 'mem) (get vm 'PC))) (nth 2 (aref (get vm 'mem) (get vm 'PC)))												))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'WRITE)	(vm-write	vm	(nth 1 (aref (get vm 'mem) (get vm 'PC)))																						))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'GET)	(vm-get		vm	(nth 1 (aref (get vm 'mem) (get vm 'PC))) (nth 2 (aref (get vm 'mem) (get vm 'PC))) (nth 3 (aref (get vm 'mem) (get vm 'PC)))	))
			( (equal (nth 0 (aref (get vm 'mem) (get vm 'PC))) 'SET)	(vm-set		vm	(nth 1 (aref (get vm 'mem) (get vm 'PC))) (nth 2 (aref (get vm 'mem) (get vm 'PC))) (nth 3 (aref (get vm 'mem) (get vm 'PC)))	))

		)
		(incf (get vm 'PC))
	)
	"Fin de VM"
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

(defun mdefun (vm nom params code)
	(set-Symb vm nom '())
	(set-Symb (get vm nom) 'params params)
	(set-Symb (get vm nom) 'nbP (list-length params))
	(set-Symb (get vm nom) 'code code)
)

(defun compile-expr (expr env)
	(let (comp)
		(cond	
			( (atom expr) (setf comp (format nil "(MOVE ~a R0)~%" expr)) )
			( t	
				(setf comp (concatenate 'string comp (compile-line (second expr) env)))
				(setf comp (concatenate 'string comp (format nil "(PUSH R0)~%")))
				(setf comp (concatenate 'string comp (compile-line (third expr) env)))
				(setf comp (concatenate 'string comp (format nil "(POP R1)~%")))
				(cond
					( (equal (first expr) '+) (setf comp (concatenate 'string comp (format nil "(~a R1 R0)~%" 'ADD)))	)
					( (equal (first expr) '-) (setf comp (concatenate 'string comp (format nil "(~a R1 R0)~%" 'SUB)))	)
					( (equal (first expr) '*) (setf comp (concatenate 'string comp (format nil "(~a R1 R0)~%" 'MULT)))	)
					( (equal (first expr) '/) (setf comp (concatenate 'string comp (format nil "(~a R1 R0)~%" 'DIV)))	)
				)
			)
		)
		(return-from compile-expr comp)
	)
)

(defun compile-comp(expr env)
	(let (comp)
		(setf comp (concatenate 'string comp (compile-line  (second  expr) env) ))
		(setf comp (concatenate 'string comp (format nil "(PUSH R0)~%")))
		(setf comp (concatenate 'string comp (compile-line  (third  expr) env) ))
		(setf comp (concatenate 'string comp (format nil "(POP R1)~%")))
		(setf comp (concatenate 'string comp (format nil "(CMP R1 R0)~%")))

		;;JMP TO CMPTRUE
		(cond
			( (equal (first expr) '<)
				(setf comp (concatenate 'string comp (format nil "(JPP 2) ~%"))) 
			)
			( (equal (first expr) '>) ()
				(setf comp (concatenate 'string comp (format nil "(JPG 2) ~%")))
			)
			( (equal (first expr) '=) ()
				(setf comp (concatenate 'string comp (format nil "(JEQ 2) ~%")))
			) 
			( (equal (first expr) '<=) ()
				(setf comp (concatenate 'string comp (format nil "(JPE 2) ~%")))
			) 
			( (equal (first expr) '>=) ()
				(setf comp (concatenate 'string comp (format nil "(JGE 2) ~%")))
			)
		)

		(setf comp (concatenate 'string comp (format nil "(MOVE 0 R0) ~%")))
		(setf comp (concatenate 'string comp (format nil "(JMP 1) ~%"))) ;;JUMP TO IF SUITE 
		;;CMPTRUE
		(setf comp (concatenate 'string comp (format nil "(MOVE 1 R0) ~%")))


		(return-from compile-comp comp)
	)

)
(defun count-substrings (substring string)
  (loop
    with sub-length = (length substring)
    for i from 0 to (- (length string) sub-length)
    when (string= string substring
                  :start1 i :end1 (+ i sub-length))
    count it))

(defun count-instruction (code)
	(- (count #\newline code) (count-substrings "LABEL" code ))
)
; ordre execution :
; 	print CMP
; 	stocker false
; 	jump cond pc + taille false
; 	print false
; 	stocke true
; 	jump pc + taille true
; 	print true
(defun compile-if (expr env)

	(let (comp true false length_t length_f) 
		; in first arg of if (bool)
		(setf comp (concatenate 'string comp (compile-comp  (second expr) env)) )
		;;CMP IFSUITE

		(setf comp (concatenate 'string comp (format nil "(CMP 0 R0) ~%")))

		(setq true (compile-line  (third expr) env))
		(setf length_t (count-instruction true))

		(setq false (compile-line  (fourth expr) env))
		(setf length_f (count-instruction false))
		;;jump taille false plus 1 pour sauter le 'jump pc +taille true'
		
		(setf comp (concatenate 'string comp (format nil "(JEQ ~a) ~%" (+ length_f 1))))
		(setf comp (concatenate 'string comp (format nil "~a" false)));;print false
		(setf comp (concatenate 'string comp (format nil "(JMP ~a) ~%" length_t )))
		(setf comp (concatenate 'string comp (format nil "~a" true)));;print true
		(return-from compile-if comp)
	)
	
)
;;Compilation d'une boucle tant que
(defun compile-while (expr env)
	(let (comp true test length_t length_test boucle)
		;;Compilation du test
		(setq test (compile-comp  (second expr) env)) 
		(setf length_test (count-instruction test))
		(setf comp (concatenate 'string comp (format nil "~a" test)));;print test

		(setf comp (concatenate 'string comp (format nil "(CMP 0 R0) ~%")))
		
		(setq true (compile-line  (third expr) env))
		(setf length_t (count-instruction true))
		
		(setf comp (concatenate 'string comp (format nil "(JEQ ~a) ~%" (+ length_t 1))))
		(setf comp (concatenate 'string comp (format nil "~a" true)));;print true
		(setf boucle (+ length_t 2 length_test))
		(setf comp (concatenate 'string comp (format nil "(JMP ~a) ~%" (- 0 boucle) )))
		(return-from compile-while comp)
	)
)

;;Compilation de setf 
(defun compile-setf (expr env)
	(let (comp)
		(setf comp (concatenate 'string comp (compile-expr  (third expr) env)) )
		(setf comp (concatenate 'string comp (replace-params-reg (format nil "(MOVE R0 ~a ) ~%" (second expr)) env)))
		(return-from compile-setf comp)
	)
)

;;Compilation appel de fct 
(defun compile-fctcall (call env)
	(let (comp nbParam)
		(setf nbParam 0)
		(loop for arg in (reverse (cdr call)) do 
			(setf comp (concatenate 'string comp (compile-line  arg env)))
			(setf comp (concatenate 'string comp (format nil "(PUSH R0) ~%")))
			(setf nbParam (+ nbParam 1))
		)
		(setf comp (concatenate 'string comp (format nil "(PUSH ~a) ~%" nbParam)))
		(setf comp (concatenate 'string comp (format nil "(MOVE FP R1) ~%" )))
		(setf comp (concatenate 'string comp (format nil "(MOVE SP FP) ~%" )))
		(setf comp (concatenate 'string comp (format nil "(MOVE SP R2) ~%" )))
		(setf comp (concatenate 'string comp (format nil "(MOVE ~a R3) ~%" (+ nbParam 1))))
		(setf comp (concatenate 'string comp (format nil "(ADD R3 R2) ~%" )))
		(setf comp (concatenate 'string comp (format nil "(PUSH R2) ~%" )))
		(setf comp (concatenate 'string comp (format nil "(PUSH R1) ~%" )))
		(setf comp (concatenate 'string comp (format nil "(PUSH RA) ~%" )))
		(setf comp (concatenate 'string comp (format nil "(JSR ~a) ~%" (car call))))

		(setf comp (concatenate 'string comp (format nil "(POP R1) ~%" )))
		(setf comp (concatenate 'string comp (format nil "(MOVE R1 RA) ~%" )))

		(setf comp (concatenate 'string comp (format nil "(POP R1) ~%" )))
		(setf comp (concatenate 'string comp (format nil "(MOVE R1 FP) ~%" )))

		(setf comp (concatenate 'string comp (format nil "(POP R1) ~%" )))
		(setf comp (concatenate 'string comp (format nil "(MOVE R1 SP) ~%" )))


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
(defun replace-params-reg (string_ env )
	(let (part replacement comp)
		(setf comp string_)
		(loop for arg in env do 
			(setf part (format nil " ~a " (car arg)))
			(setf replacement (format nil " R~a " (cdr arg)))
			(setf comp (replace-all comp part replacement ))
		)
		(return-from replace-params-reg comp)
	)
)
(defun lastValueEnv(env)
	(if (null env)
		4
		(cdr (first (last env)))
	)
)

(defun consPair (env arg index)
	(if (null env)
		(setf env (list (cons arg index)))
		(setf env (cons (car env) (consPair (cdr env) arg index) )) 
	)
)

(defun compile-fct (fct env)
	(let (comp)
		(setf comp (concatenate 'string comp (format nil "(JMP end_~a) ~%" (second fct))))
		(setf comp (concatenate 'string comp (format nil "(LABEL ~a) ~%" (second fct))))
		;;Ajout des paramètres à l'environnement
		 (let (index cpt)
		 	(setf index (lastValueEnv env))
		 	(setf cpt 1)
			(loop for arg in (third fct) do
				(setf comp (concatenate 'string comp (format nil "(GET ~a FP R~a) ~%" cpt index)))
				(setf env (consPair env arg index))
				(setf index (+ index 1))
				(setf cpt (+ cpt 1))
			)
		)
		
		(setf comp (concatenate 'string comp (replace-params-reg (compile-all (fourth fct) env ) env)))
		; ;;Restaurer param ?
		;  (let (index cpt)
		;  	(setf index (lastValueEnv env))
		;  	(setf cpt 1)
		; 	(loop for arg in (third fct) do
		; 		(setf comp (concatenate 'string comp (format nil "(SET R~a ~a FP) ~%" index cpt)))
		; 		(setf env (consPair env arg index))
		; 		(setf index (+ index 1))
		; 		(setf cpt (+ cpt 1))
		; 	)
		; )

		(setf comp (concatenate 'string comp (format nil "(RTN) ~%" )))
		(setf comp (concatenate 'string comp (format nil "(LABEL end_~a) ~%" (second fct))))
		(return-from compile-fct comp)
	)
)
(defun isOp (op)
	(let (res)
		(setf res nil)
		(cond 
			((equal op '+) (setf  res T))
			((equal op '-) (setf  res T))
			((equal op '*) (setf  res T))
			((equal op '/) (setf  res T))
			((numberp op) (setf res T))
		)
		(return-from isOP res)
	)
)

(defun isComp(op)
	(let (res)
		(setf res nil)
		(cond 
			((equal op '<) (setf  res T))
			((equal op '>) (setf  res T))
			((equal op '=) (setf  res T))
			((equal op '<=) (setf  res T))
			((equal op '>=) (setf  res T))
		)
		(return-from isComp res)
	)
)
;;Compile une ligne
(defun compile-line(line env)
 	(let (comp)
 		(cond 
 			((atom line) (setf comp (compile-expr line env))) ;; Si c'est un atom genre A ou un nombre
 			((equal (car line) 'if) (setf comp(compile-if line env))) ;;Si c'est un if
 			((equal (car line) 'defun) (setf comp (compile-fct line env))) ;;Si c'est une définition de fonction
 			((equal (car line) 'while) (setf comp (compile-while line env))) ;;Si c'est une boucle
 			((equal (car line) 'setf) (setf comp (compile-setf line env))) ;;Si c'est une boucle
 			((isOp (car line)) (setf comp(compile-expr line env))) ;;Si c'est une expr arithmetique
 			((isComp (car line)) (setf comp (compile-comp line env)));;Si c'est un operateur de comparaison
 			((atom (car line)) (setf comp (compile-fctcall line env))) ;;Si c'est un atom, peut-être mieux de chercher si l'atom est bien une fonction
 		)
 		(return-from compile-line comp)
 	)
)

#| a faire : compile-all compile toutes les fonctions avant tout (donc rajouter symbole liste dans vm pour toutes les fonctions) |#
(defun compile-all (progr env)
	(let (comp)
		(loop for line in progr do
			(setf comp (concatenate 'string comp (compile-line line env)))
		)
		(return-from compile-all comp)
	)
)

(defun compile-load (vm progr)
	(writeFile "..\\code\\ASM.lisp" (compile-all progr '()))
	(loader vm (readFile "..\\code\\ASM.lisp"))
)

(defun compile-load-exec (vm progr)
	(compile-load vm progr)
	(exec-vm vm)
)