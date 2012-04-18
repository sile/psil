.PHONY: vm

vm:
	mkdir -p bin
	cd vm; sbcl --script make-vm-command.lisp
	mv vm/pvm bin/
