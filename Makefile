#

all: namespace.elc cl-ns.elc

%.elc: %.el
	emacs --batch -L . -f batch-byte-compile $<

define RUNTESTS
  emacs --batch -L . -l $(1) -f ert-run-tests-batch-and-exit
endef

check: namespace.elc namespace-test.elc cl-ns.elc
	$(call RUNTESTS,namespace-test.el)
	$(call RUNTESTS,namespace-test.elc)

clean:
	find . -maxdepth 1 -name '*.elc' -exec rm {} \;
