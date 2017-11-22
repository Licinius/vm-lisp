(make-vm 'vm 25)

(setf l (cons (list "MOVE" "R1" 1) (cons (list "LABEL" "FACT") (cons (list "MOVE" "R2" 2) NIL))))

(loader 'vm l)

(get 'vm 'mem)
(get 'vm 'labels)
