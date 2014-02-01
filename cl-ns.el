;; cl-ns.el --- Namespace for cl-lib

;; Copyright (C) 2014 Helmut Eller

;; Author: Helmut Eller <eller.helmut@gmail.com>

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file defines a namespace "cl" that exports most of the usual
;; symbols from cl-lib.

;;; Code:

(require 'cl-lib)
(require 'namespace)

(define-namespace cl-ns-aux
    ((:export dolist dotimes))

  ;; cl-dolist is defined in terms of dolist and that would lead to
  ;; endless recursions.  Fix that my using namespace-global.  Real
  ;; Common Lisp also inserts a TAGBODY around the body, but cl-dolist
  ;; apparently does not.

  (defmacro dolist (spec &rest body)
    `(cl-block nil (namespace-global (dolist ,spec . ,body))))

  (defmacro dotimes (spec &rest body)
    `(cl-block nil (namespace-global (dotimes ,spec . ,body))))

  )

(define-namespace cl
    ((:use cl-ns-aux)
     (:export
      labels
      flet

      remprop
      getf
      tailp
      list-length
      nreconc
      revappend
      concatenate
      subseq
      random-state-p
      make-random-state
      signum
      isqrt
      lcm
      gcd
      notevery
      notany
      every
      some
      mapcon
      mapcan
      mapl
      maplist
      map
      equalp
      coerce
      tree-equal
      nsublis
      sublis
      nsubst-if-not
      nsubst-if
      nsubst
      subst-if-not
      subst-if
      subsetp
      nset-exclusive-or
      set-exclusive-or
      nset-difference
      set-difference
      nintersection
      intersection
      nunion
      union
      rassoc-if-not
      rassoc-if
      assoc-if-not
      assoc-if
      member-if-not
      member-if
      merge
      stable-sort
      search
      mismatch
      count-if-not
      count-if
      count
      position-if-not
      position-if
      position
      find-if-not
      find-if
      find
      nsubstitute-if-not
      nsubstitute-if
      nsubstitute
      substitute-if-not
      substitute-if
      substitute
      delete-duplicates
      remove-duplicates
      delete-if-not
      delete-if
      remove-if-not
      remove-if
      replace
      fill
      reduce
      compiler-macroexpand
      define-compiler-macro
      assert
      check-type
      typep
      deftype
      defstruct
      callf2
      callf
      letf*
      letf
      rotatef
      shiftf
      remf
      psetf
      declare
      the
      locally
      multiple-value-setq
      multiple-value-bind
      symbol-macrolet
      macrolet
      progv
      psetq
      do-all-symbols
      do-symbols
      do*
      do
      loop
      return-from
      return
      block
      etypecase
      typecase
      ecase
      case
      load-time-value
      eval-when
      destructuring-bind
      gentemp
      gensym
      pairlis
      acons
      subst
      adjoin
      copy-list
      ldiff
      list*
      cddddr
      cdddar
      cddadr
      cddaar
      cdaddr
      cdadar
      cdaadr
      cdaaar
      cadddr
      caddar
      cadadr
      cadaar
      caaddr
      caadar
      caaadr
      caaaar
      cdddr
      cddar
      cdadr
      cdaar
      caddr
      cadar
      caadr
      caaar
      tenth
      ninth
      eighth
      seventh
      sixth
      fifth
      fourth
      third
      endp
      rest
      second
      first
      svref
      copy-seq
      evenp
      oddp
      minusp
      plusp
      floatp-safe
      declaim
      proclaim
      nth-value
      multiple-value-call
      multiple-value-apply
      multiple-value-list
      values-list
      values
      pushnew
      decf
      incf

      dolist
      dotimes

      )))

(provide 'cl-ns)

;; cl-ns.el ends here
