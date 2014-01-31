# Makefile for namespace.el

# Copyright (C) 2014 Helmut Eller

# This file is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This file is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

help:
	@echo -e "\
Main targets\n\
all        -- compile namespace.el\n\
check      -- run tests\n\
clean      -- delete generated files\n\
install    -- install ELPA package\n\
uninstall  -- uninstall ELPA package\n\
help       -- print this message"

all: namespace.elc cl-ns.elc

%.elc: %.el
	emacs --batch -L . -f batch-byte-compile $<

RUNTESTS = emacs --batch -L . -l $(1) -f ert-run-tests-batch-and-exit

check: namespace.elc namespace-test.elc cl-ns.elc
	$(call RUNTESTS,namespace-test.el)
	$(call RUNTESTS,namespace-test.elc)

clean:
	find . -maxdepth 1 \
	  -regex '.*\(\.elc\|\.tar\|-pkg\.el\)$$' \
	  -exec rm -v {} \;

PKGFILES := namespace-pkg.el namespace.el cl-ns.el namespace-test.el README.org

VERSION = $(shell awk '/^;; Version: [0-9.]+/ { print $$3 }' namespace.el)

TARDIR = namespace-${VERSION}

PKGTAR = ${TARDIR}.tar

${PKGTAR}: ${PKGFILES}
	mkdir -p ${TARDIR}
	cp ${PKGFILES} ${TARDIR}
	tar -cvf $@ ${TARDIR}
	rm -r ${TARDIR}

DOCSTRING = Namespaces for Emacs Lisp

namespace-pkg.el:
	echo '(define-package "namespace" "${VERSION}" "${DOCSTRING}")' >$@

.INTERMEDIATE: ${PKGTAR} namespace-pkg.el

install: ${PKGTAR}
	emacs -batch -eval '(package-install-file "${PKGTAR}")'

uninstall:
	rm -vr ~/.emacs.d/elpa/namespace-${VERSION}

# Makefile ends here
