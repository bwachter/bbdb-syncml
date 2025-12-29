EMACS=emacs
AUTOLOADS_FILE=lisp/lempo-autoloads.el

.PHONY: autoloads byte-compile byte-compile-tests clean tests

all: autoloads

clean:
	@rm -f *.elc

autoloads:
	@$(EMACS) -Q --batch --eval "\
	 (loaddefs-generate '(\"lisp\") \"$(AUTOLOADS_FILE)\"))"

byte-compile: clean
	@$(EMACS) -Q -L bbdb/lisp -L contrib -L ../bbdb-vcard -L dom.el -L . --batch -f batch-byte-compile *.el

byte-compile-tests: clean byte-compile
	@$(EMACS) -Q -L . --batch -f batch-byte-compile t/*.el

tests: clean
	./test.sh
