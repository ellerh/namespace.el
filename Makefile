#

all: namespace.elc

%.elc: %.el
	emacs --batch -L . -f batch-byte-compile $<

check: namespace.elc test.elc
	emacs --batch -L . -l test.el -f ert-run-tests-batch-and-exit
	emacs --batch -L . -l test.elc -f ert-run-tests-batch-and-exit

clean:
	find . -maxdepth 1 -name '*.elc' -exec rm {} \;
