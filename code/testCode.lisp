(require "instruction.lisp")
(require "machine.lisp")

(make-vm 'vm 25)


(loader 'vm l)

; (get 'vm 'labels)

(get 'vm 'mem)

(exec-vm 'vm)
