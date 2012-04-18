vm: bin/pvm
	mkdir -p bin
	cd vm; sbcl --script make-vm-command.lisp
	mv vm/pvm bin/

bin/pvm:
