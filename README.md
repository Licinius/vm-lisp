# vm-lisp

Ceci est un projet universitaire ayant pour but de produire une machine virtuel à registre

Instruction d'exécution :
	
	- Pour importer vm, le compilateur et les instructions de la vm :
	(require "<chemin-vers-machine.lisp>")

	- Pour lancer la machine virtuelle :
	(make-vm <nomVM>)
	# on peut rajouter en paramètre, après le nom de la machine virtuelle, la taille de la mémoire (par défaut : 1000) 
	
	- Pour définir des fonctions en lisp, les mettre dans une liste :
	ex. : '( (defun nameFunction (params) doStuff) )

	- Pour compiler des instructions lisp :
	(compile-all programmeLisp '() ) 
	# la liste vide en deuxième paramètre est l'environnement, il sera modifié au cours de la fonction pour rajouter les variables locales par exemple

	- Pour charger des instructions compilées :
	(loader <nomVM> <liste-d'instructions>)

	- Pour exécuter le code chargé :
	(exec-vm <nomVM>)

	
	# note : Il est possible d'automatiser les étapes de compilation jusqu'à l'exécution grâce aux méthodes "compile-load" et "compile-load-exec" qui permettent respectivement de compiler et charger des instructions lisp dans la machine virtuelle, et de compiler, charger, et exécuter ces mêmes instructions
	
	Pour tout problème avec la machine virtuelle, on pourra prendre pour exemple le fichier de test, testCode.lisp, fournis dans le dossier code.