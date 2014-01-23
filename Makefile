
all: namespace.elc

%.elc: %.el
	emacs --batch -f batch-byte-compile $<

clean:
	find . -name '*.elc' -exec rm -v {} \;
