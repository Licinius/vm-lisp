(require "instruction.lisp")

; definit un symbole dans une variable
(defun set-Symb (vm nom val) (setf (get vm nom) val) )
; initialisation memoire
(defun init-mem (nom &optional(taille 1000)) (set-Symb nom 'mem (make-array taille)) )
; initialisation des flags
(defun set-flag-init (vm)
	(set-Symb vm 'DEQ 0)
	(set-Symb vm 'DPG 0)
	(set-Symb vm 'DPP 0)

)

; creation de la machine virtuelle (taille de base 1000)
(defun make-vm (nom &optional (taille 1000))
	(init-mem nom taille) ; memoire

	;; Registre de base
	(set-Symb nom 'labels (make-hash-table)) ; label (cle = nom, value = numero instruction)
	(set-Symb nom 'SP taille) ; stack pointer
	(set-Symb nom 'FP taille) ; frame pointer
	(set-Symb nom 'RA 0) ; return adress
	(set-Symb nom 'PC 0) ; program counter
	
	;; Flag
	(set-flag-init nom)

	;; Register
	; (set-Symb nom 'R0 0)
	; (set-Symb nom 'R1 0)
	; (set-Symb nom 'R2 0)
	; (set-Symb nom 'R3 0)

	;; VM State (0 on, 1 off)
	(set-Symb nom 'state 0)
)

; charge une liste d'instruction dans la memoire
(defun loader (vm instr &optional(ptr (get vm 'PC)))
	(cond
		((not (equal (car instr) NIL))
			(cond
				(
					;; si instruction est label -> stocke le numero de l'instruction dans la hash table labels
					(equal(caar instr) 'LABEL)
						(setf (gethash (cadar instr) (get vm 'labels)) ptr)
						(loader vm (cdr instr) ptr)
						(set-Symb vm 'PC (length (cdr instr)))
				)
					;; sinon charger instruction a la suite dans la memoire
				(
					(setf (aref (get vm 'mem) ptr) (car instr))
					(loader vm (cdr instr) (+ ptr 1))
				)
			)
		)
	)
)

; defini flag equal
(defun set-flag-DEQ (vm)
	(set-flag-init vm)
	(set-Symb vm 'DEQ 1)
)

; defini flag plus grand
(defun set-flag-DPG (vm)
	(set-flag-init vm)
	(set-Symb vm 'DPG 1)
)

;defini flag plus petit
(defun set-flag-DPP (vm)
	(set-flag-init vm)
	(set-Symb vm 'DPP 1)
)

; execute le code stocker dans la memoire de vm
(defun exec-vm (vm)
	(loop while (not (or (equal (aref (get vm 'mem) (get vm 'PC)) NIL) (equal (get vm 'state) 1)))
		do
				#| (write (format nil " trace : ~a ~%" (aref (get vm 'mem) (get vm 'PC)))) |#
		(cond 
			;; switch sur les instructions charger (appelle les fonctions lisp correspondantes dans instruction.lisp)
			;; si  		premier element 		equal			*	lancer 	*		vm 				argument 1										argument 2
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
		)
		;; incrementer pc (passer a l'instruction suivante)
		(incf (get vm 'PC))
	)
	"Fin de VM"
)

; lit dans un fichier a l'adresse path
(defun readFile (path)
	;; file in : ouvre le fichier dans path, erreur si n'existe pas, l'ouvre en lecture
	(let ( (fin (open path :if-does-not-exist :error :direction :input)) )
		(setf obj '())
		(setf line (read fin nil nil nil))
		;; pour chaque ligne de file in faire
		(loop while (not (equal line nil)) do
			;; ajouter la ligne a obj
			(write line)
			(setq obj (append obj (list line)))
			(setf line (read fin nil nil nil))
		)
		;; fermer file in et retourner obj
		(close fin)
		(return-from readFile obj)
	)
)

; ecrit str dans un fichier a l'adresse path
(defun writeFile (path str)
	;; file out : ouvre le fichier dans path, cree si n'existe pas, ecrase si existe, l'ouvre en ecriture
	(let ((fout (open path :if-does-not-exist :create :if-exists :supersede :direction :io)))
		;; ecrit str dans file out et ferme file out
		(format fout str)
		(close fout)
	)
)

;;@Deprecated
(defun mdefun (vm nom params code)
	(set-Symb vm nom '())
	(set-Symb (get vm nom) 'params params)
	(set-Symb (get vm nom) 'nbP (list-length params))
	(set-Symb (get vm nom) 'code code)
)

; compile les expressions arithmetiques (avec + - / *)
(defun compile-expr (expr env)
	;; variable local comp stockera les instructions apres compilation
	(let (comp)
		(cond	
			;; si l'expression est un atome (chiffre) le stocker dans R0 et fin
			( (atom expr) (setf comp (format nil "(MOVE ~a R0)~%" expr)) )
			;; sinon
			( t	
				;; compiler le premier argument (stocker dans R0 a la fin)
				(setf comp (concatenate 'string comp (compile-line (second expr) env)))
				(setf comp (concatenate 'string comp (format nil "(PUSH R0)~%")))
				;; compiler le deuxieme argument (stocker dans R0 a la fin)
				(setf comp (concatenate 'string comp (compile-line (third expr) env)))

				;; recuperer premier argument
				(setf comp (concatenate 'string comp (format nil "(POP R1)~%")))

				;; switch sur les operateurs
				(cond
					( (equal (first expr) '+) (setf comp (concatenate 'string comp (format nil "(~a R1 R0)~%" 'ADD)))	)
					( (equal (first expr) '-) (setf comp (concatenate 'string comp (format nil "(~a R1 R0)~%" 'SUB)))	)
					( (equal (first expr) '*) (setf comp (concatenate 'string comp (format nil "(~a R1 R0)~%" 'MULT)))	)
					( (equal (first expr) '/) (setf comp (concatenate 'string comp (format nil "(~a R1 R0)~%" 'DIV)))	)
				)
			)
		)
		;; retourner les instructions assembleur a la fin
		(return-from compile-expr comp)
	)
)

; compiler les comparateurs (avec < <= = >= >)
(defun compile-comp(expr env)
	;; variable local comp stockera les instructions apres compilation
	(let (comp)
		;; compiler le premier argument (stocker dans R0 a la fin)
		(setf comp (concatenate 'string comp (compile-line  (second  expr) env) ))
		(setf comp (concatenate 'string comp (format nil "(PUSH R0)~%")))
		;; compiler le deuxieme argument (stocker dans R0 a la fin)
		(setf comp (concatenate 'string comp (compile-line  (third  expr) env) ))
		;; recuperer premier argument
		(setf comp (concatenate 'string comp (format nil "(POP R1)~%")))

		;; comparer premier et deuxieme argument (affectation des flags au moment de l'execution)
		(setf comp (concatenate 'string comp (format nil "(CMP R1 R0)~%")))

		;; switch sur les comparateurs (si vrai jump au cas true)
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

		;; cas false
		(setf comp (concatenate 'string comp (format nil "(MOVE 0 R0) ~%")))
		(setf comp (concatenate 'string comp (format nil "(JMP 1) ~%"))) ;; saute le cas true
		
		;;cas true
		(setf comp (concatenate 'string comp (format nil "(MOVE 1 R0) ~%")))

		;; retourner les instructions assembleur a la fin
		(return-from compile-comp comp)
	)

)

; compte nombre d'occurence d'une sequence dans une string
(defun count-substrings (substring string)
  (loop
    with sub-length = (length substring)
    for i from 0 to (- (length string) sub-length)
    when (string= string substring
                  :start1 i :end1 (+ i sub-length))
    count it))

; compte le nombre d'instruction dans une liste d'instruction (sans compter les labels)
(defun count-instruction (code)
	(- (count #\newline code) (count-substrings "LABEL" code ))
)

; compiler les if
(defun compile-if (expr env)
	;; variable local comp stockera les instructions apres compilation
	;; variable true stockera les instructions apres compilation si la condition est vraie
	;; variable length_t = nombre d'instruction de true
	;; variable false stockera les instructions apres compilation si la condition est fausse
	;; variable length_f = nombre d'instruction de false
	(let (comp true false length_t length_f) 
		;; compiler la condition du if
		(setf comp (concatenate 'string comp (compile-comp  (second expr) env)) )
		;; traiter si condition est vraie ou fausse
		(setf comp (concatenate 'string comp (format nil "(CMP 0 R0) ~%")))

		;; compiler et stocker la partie true et sa taille ; la partie false et sa taille
		(setq true (compile-line  (third expr) env))
		(setf length_t (count-instruction true))
		(setq false (compile-line  (fourth expr) env))
		(setf length_f (count-instruction false))
		
		;; jump a la partie false si la condition est fausse
		(setf comp (concatenate 'string comp (format nil "(JEQ ~a) ~%" (+ length_t 1)))) ;; +1 pour sauter le jump a la fin de la partie true
		(setf comp (concatenate 'string comp (format nil "~a" true))) ;; stocker la partie true
		(setf comp (concatenate 'string comp (format nil "(JMP ~a) ~%" length_f ))) ;; jump apres la partie false
		(setf comp (concatenate 'string comp (format nil "~a" false))) ;; stocker la partie false

		;; retourner les instructions assembleur a la fin
		(return-from compile-if comp)
	)
	
)

; compiler les while
(defun compile-while (expr env)
	;; variable local comp stockera les instructions apres compilation
	;; variable true stockera les instructions apres compilation si la condition est vraie
	;; variable length_t = nombre d'instruction de true
	;; variable test stockera les instructions apres compilation de la condition du while
	;; variable length_test = nombre d'instruction de test
	;; variable boucle ; 
	(let (comp true test length_t length_test boucle)
		
		;; compiler la condition du while
		(setq test (compile-comp  (second expr) env)) 
		(setf length_test (count-instruction test))
		(setf comp (concatenate 'string comp (format nil "~a" test))) ;; stocker la condition

		;; tester la condition
		(setf comp (concatenate 'string comp (format nil "(CMP 0 R0) ~%")))

		;; compiler et stocker la partie true et sa taille
		(setq true (compile-line  (third expr) env))
		(setf length_t (count-instruction true))

		;; jump apres le while si la condition est fausse
		(setf comp (concatenate 'string comp (format nil "(JEQ ~a) ~%" (+ length_t 1))))
		(setf comp (concatenate 'string comp (format nil "~a" true))) ;; stocker les instructions du while
		
		(setf boucle (+ length_t 2 length_test))
		(setf comp (concatenate 'string comp (format nil "(JMP ~a) ~%" (- 0 boucle) ))) ;; jump a la condition du while

		;; retourner les instructions assembleur a la fin
		(return-from compile-while comp)
	)
)

; compiler les setf
(defun compile-setf (expr env)
	;; variable local comp stockera les instructions apres compilation
	(let (comp)
		;; compiler la valeur a stocker dans la variable
		(setf comp (concatenate 'string comp (compile-expr  (third expr) env)) )
		;; stocker la variable dans l'environement et lui affecter un registre
		(setf comp (concatenate 'string comp (replace-params-reg (format nil "(MOVE R0 ~a ) ~%" (second expr)) env)))
		
		;; retourner les instructions assembleur a la fin
		(return-from compile-setf comp)
	)
)

; compiler un appel de fonction
(defun compile-fctcall (call env)
	;; variable local comp stockera les instructions apres compilation
	;; variable local nbParam stockera le nombre de parametre de la fonction
	(let (comp nbParam)
		(setf nbParam 0)
		;; pour tous les arguments
		(loop for arg in (reverse (cdr call)) do 
			;; compiler les arguments et les empiler dans la pile
			(setf comp (concatenate 'string comp (compile-line  arg env)))
			(setf comp (concatenate 'string comp (format nil "(PUSH R0) ~%"))) 
			(setf nbParam (+ nbParam 1))
		)
		;; ordre d'appel :
		;; 		empiler le nombre de parametre
		;; 		stocker l'ancien FP
		;; 		affecter le nouveau FP (egal SP actuel)
		;; 		calculer et empile l'ancien SP (SP actuel plus le nombre de parametre + 1 [pour l'empilement du nombre de parametres])
		;; 		empiler l'ancien FP
		;; 		empiler l'ancien RA
		;; 			sauter a la fonction appelee (stocker prochaine instruction dans RA)
		;; 			fonction finie (qui a donc utilisee le return [RTN])
		;; 		depiler et revenir a l'ancien RA 
		;; 		depiler et revenir a l'ancien FP
		;; 		depiler et revenir a l'ancien SP (revient a depiler tous les parametres)
		;; 		recuperer l'ancien environemment 
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
		
		(let (index)
		 	(setf index (lastValueEnv env)) 
		 	(setf comp (concatenate 'string comp (format nil "(MOVE FP R3) ~%"))) ;; stocker FP dans R3
		 	;; pour chaque parametre entrant les restocker dans les registres correspondants (comme avant appel d'unc fonction)
			(loop for arg in env do 
				(setf comp (concatenate 'string comp (format nil "(INCR R3) ~%")))
				(setf comp (concatenate 'string comp (format nil "(LOAD R3 R~a) ~%" index)))
				(setf index (+ index 1))
			)
		)

		;; retourner les instructions assembleur a la fin
		(return-from compile-fctcall comp)
	)

)

; /!\ fonction copiee/colle
; Disponible dans la documentation sur les strings
; Ne fait pas parti des ANSI Standard
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
            while pos
       )
    )
)

; remplacer les paramètres par leur registre
(defun replace-params-reg (string_ env ) 
	(let (part replacement comp)
		(setf comp string_)
		;;Parcours de la liste des paires dans l'environnement
		(loop for arg in env do 
			(setf part (format nil " ~a " (car arg))) ;;Le motif qui correspond au nom de la variable
			(setf replacement (format nil " R~a " (cdr arg))) ;;Le motif qui correspond à R suivi du numéro stocké dans l'env.
			(setf comp (replace-all comp part replacement ));;Appel de la fonction pour remplacer <nomVar> par R<numVar>
		)
		(return-from replace-params-reg comp)
	)
)

; donner la valeur de la dernière variable d'environnement
(defun lastValueEnv(env)
	(if (null env)
		4
		(cdr (first (last env)))
	)
)

; ajoutee une paire pointée à une liste de paires pointées
(defun consPair (env arg index)
	;;Si l'environnement est null alors
	(if (null env)
		(setf env (list (cons arg index))) ;;Ajoute à la fin une liste de une paire pointée
		(setf env (cons (car env) (consPair (cdr env) arg index) ))  ;;Sinon concatene avec la suite
	)
)

; compiler une definition de fonction (defun)
(defun compile-fct (fct env)
	;; variable local comp stockera les instructions apres compilation
	(let (comp)
		(setf comp (concatenate 'string comp (format nil "(LABEL ~a) ~%" (second fct)))) ;; label du debut de la fonction
		
		;; ajouter des paramètres à l'environnement en les chargeant a partir de fp (car l'appellant les stocke dans la pile avant de l'appeler)
		(let (index)
		 	(setf index (lastValueEnv env))
		 	(setf comp (concatenate 'string comp (format nil "(MOVE FP R3) ~%")))
			(loop for arg in (third fct) do
				(setf comp (concatenate 'string comp (format nil "(INCR R3) ~%")))
				(setf comp (concatenate 'string comp (format nil "(LOAD R3 R~a) ~%" index)))
				(setf env (consPair env arg index))
				(setf index (+ index 1))
			)
		)

		;; remplacer les occurences des variables de la fonction par les registres correspondant
		(setf comp (concatenate 'string comp (replace-params-reg (compile-all (cdddr fct) env ) env)))
		;; derniere instruction de la fonction, retourne a l'instruction suivant de l'appelant
		(setf comp (concatenate 'string comp (format nil "(RTN) ~%" )))


		;; retourner les instructions assembleur a la fin
		(return-from compile-fct comp)
	)
)

; compiler les car de liste
(defun compile-car (line env)
	;; variable local comp stockera les instructions apres compilation
	(let (comp)
		;; si la variable entrante de car est un atome, stocker l'instruction assembleur car
		;; sinon retourner liste vide
		(if (atom (second line))
			(setf comp (concatenate 'string comp (format nil "(CAR ~a) ~%" (second line))))
			()
		)
		;; retourner les instructions assembleur a la fin
		(return-from compile-car comp)
	)
)

; compiler les cdr de liste
(defun compile-cdr (line env)
	;; variable local comp stockera les instructions apres compilation
	(let (comp)
		;; si la variable entrante de cdr est un atome, stocker l'instruction assembleur cdr
		;; sinon retourner liste vide
		(if (atom (second line))
			(setf comp (concatenate 'string comp (format nil "(CDR ~a) ~%" (second line))))
			()
		)
		;; retourner les instructions assembleur a la fin
		(return-from compile-car comp)

	)
)

;;Compile une line vide et la remplace par l'instruction assembleur NOP
(defun compile-skip(line env)
	;; variable local comp stockera les instructions apres compilation
	(let (comp)
		;; stocker l'instruction nop qui ne fait aucune operation
		(setf comp (concatenate 'string comp (format nil "(NOP) ~%" (second line))))
		;; retourner les instructions assembleur a la fin
		(return-from compile-car comp)

	)
)

; renvoie true si op est un operateur arithmetique, nil sinon
(defun isOp (op)
	;; variable local stockant le resultat booleen
	(let (res)
		;; cas de base res est faux
		(setf res nil)
		;; switch sur les operateurs
		(cond 
			((equal op '+) (setf  res T))
			((equal op '-) (setf  res T))
			((equal op '*) (setf  res T))
			((equal op '/) (setf  res T))
			((numberp op) (setf res T)) 
		)
		;; retourner res
		(return-from isOP res)
	)
)

; renvoie true si op est un comparateur booleen, nil sinon
(defun isComp(op)
	;; variable local stockant le resultat booleen
	(let (res)
		;; cas de base res est faux
		(setf res nil)
		;; switch sur les operateurs
		(cond 
			((equal op '<) (setf  res T))
			((equal op '>) (setf  res T))
			((equal op '=) (setf  res T))
			((equal op '<=) (setf  res T))
			((equal op '>=) (setf  res T))
		)
		;; retourner res
		(return-from isComp res)
	)
)

; compiler une ligne
(defun compile-line(line env)
	;; variable local comp stockera les instructions apres compilation
 	(let (comp)
 		;; switch sur les fonctions lisp que l'on peut compiler
 		;; chaque fonction retourne les instructions assembleurs permettant de faire ces operations
 		(cond 
 			((null line)(setf comp (compile-skip line env))) ;; skip si la ligne est vide
 			((atom line) (setf comp (compile-expr line env))) ;; compile expression si la ligne ne contient qu'un atome
 			((equal (car line) 'if) (setf comp(compile-if line env))) ;; compile if
 			((equal (car line) 'defun) (setf comp (compile-fct line env))) ;; compile fonction (si defun)
 			((equal (car line) 'while) (setf comp (compile-while line env))) ;; compile while 
 			((equal (car line) 'setf) (setf comp (compile-setf line env))) ;; compile setf
 			((equal (car line) 'car) (setf comp (compile-car line env))) ;; compile car
 			((equal (car line) 'cdr) (setf comp (compile-cdr line env))) ;; compile cdr
 			((isOp (car line)) (setf comp(compile-expr line env))) ;; compile expression si la ligne effectue une operation arithmetique
 			((isComp (car line)) (setf comp (compile-comp line env))) ;; compile comp si la ligne effectue une comparaison
 			((atom (car line)) (setf comp (compile-fctcall line env))) ;; compile appel de fonction si le premier element est nom (donc un atome)

 		)
 		;; retourner les instructions assembleur a la fin
 		(return-from compile-line comp)
 	)
)

; compiler un programme (cree un fichier ASM.txt contenant les instructions assembleurs du programme lisp compile)
(defun compile-all (progr env)
	;; variable local comp stockera les instructions apres compilation
	(let (comp)
		;; pour chaque ligne du programme compiler cette ligne
		(loop for line in progr do
			(setf comp (concatenate 'string comp (compile-line line env)))
		)
		;; retourner les instructions assembleur a la fin
		(writeFile "../code/ASM.txt" comp)
		(return-from compile-all comp)
	)
)

; compiler et charger le code assembleur dans la memoire
(defun compile-load (vm progr)
	;; compiler
	(compile-all progr '())
	;; charger le code en memoire
	(loader vm (readFile "../code/ASM.txt"))
)

; compiler, charger et executer le programme
(defun compile-load-exec (vm progr)
	;; compiler et charger
	(compile-load vm progr)
	;; executer
	(exec-vm vm)
)