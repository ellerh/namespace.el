

all: namespace.elc

%.elc: %.el
	emacs --batch -f batch-byte-compile $<
