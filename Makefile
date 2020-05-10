# Makefile for namespace.el

# Copyright (C) 2014 Helmut Eller
# SPDX-License-Identifier: GPL-3.0-or-later

help:
	@printf "\
Main targets\n\
all        -- compile namespace.el\n\
check      -- run tests\n\
clean      -- delete generated files\n\
install    -- install ELPA package\n\
uninstall  -- uninstall ELPA package\n\
help       -- print this message\n"

ELFILES := namespace.el cl-ns.el namespace-tools.el namespace-test.el

ELCFILES := $(ELFILES:.el=.elc)

EMACS := emacs

all: $(ELCFILES)

%.elc: %.el
	$(EMACS) --batch -L .  -f batch-byte-compile $<

RUNTESTS = $(EMACS) --batch -L . -l $(1) -f ert-run-tests-batch-and-exit

check: $(ELCFILES)
	$(call RUNTESTS,namespace-test.elc)
	$(call RUNTESTS,namespace-test.el)

clean:
	find . -maxdepth 1 \
	  -regex '.*\(\.elc\|\.tar\|-pkg\.el\)$$' \
	  -exec rm -v {} \;

PKGFILES := $(ELFILES) namespace-pkg.el README.org

VERSION := $(shell awk '/^;; Version: [0-9.]+/ { print $$3 }' namespace.el)

TARDIR := namespace-$(VERSION)

PKGTAR := $(TARDIR).tar

$(PKGTAR): $(PKGFILES)
	mkdir -p $(TARDIR)
	cp $(PKGFILES) $(TARDIR)
	tar -cvf $@ $(TARDIR)
	rm -r $(TARDIR)

DOCSTRING := Namespaces for Emacs Lisp

namespace-pkg.el:
	echo '(define-package "namespace" "$(VERSION)" "$(DOCSTRING)")' >$@

.INTERMEDIATE: $(PKGTAR) namespace-pkg.el

install: $(PKGTAR)
	$(EMACS) -batch -eval '(package-install-file "$(PKGTAR)")'

uninstall:
	rm -vr ~/.emacs.d/elpa/namespace-$(VERSION)

# Makefile ends here
