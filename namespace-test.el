;; namespace-test.el --- tests for namespace.el         -*-lexical-binding:t-*-

;; Copyright (C) 2014 Helmut Eller

;; Author: Helmut Eller <eller.helmut@gmail.com>

;; This work is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This work is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'namespace)
(require 'ert)
(require 'cl-lib)
(require 'cl-ns)

(define-namespace a
    ((:export a c d))

  (defun a (x y)
    (b (list 'a-a y x)))

  (defun b (x)
    (list 'a--b x))

  (defun c ()
    (list #'a #'b))

  (defun d (x)
    (funcall #'b (list 'a-d x)))

  )

(define-namespace b
    ((:import a)
     (:export e f))

  (defun e (x y)
    (a (list 'b-e x) y))

  (defun f (x)
    (funcall #'d (list 'b-f x)))
  )

(define-namespace c
    ((:import (a (:except a c)))
     (:export a))

  (defun a (x y)
    (a-a (list 'c-a x) y))

  (defun c (x)
    (funcall #'d (list 'c--c x)))
  )

(define-namespace d
    ((:import (a (:except a))
	      (c (:only a)))
     (:export b))

  (defun b (x y)
    (a (list 'd-b x) y))
  )

(define-namespace e
    ((:export make-a
	      a-x
	      b/x
	      make-c
	      c
	      c-x))

  (defstruct a x y)

  (defstruct (b (:conc-name b/))
    x y)

  (defstruct (c (:predicate c?))
    x y)

  )

(define-namespace f
    ((:export make-a
	      a-x
	      make-c
	      c
	      c-x))

  (defstruct a x)

  (defstruct (c (:predicate c?))
    x)

  )

(define-namespace g
    ((:import e)
     (:export a b d))

  (defun a ()
    (make-a :x 123 :y 456))

  (defun b (a)
    (a-x a))

  (defun d (a x)
    (setf (a-x a) x)
    a)

  )

(define-namespace h
    ((:import cl)
     (:export a b c d e f g h))

  (defun a ()
    (loop for x across [0 1 2 3 4]
	  collect x))

  (defun b (x)
    (caddr x))

  (defun c (l)
    (destructuring-bind (x y) l
      (cons y x)))

  (defun d (x l)
    (position x l))

  (defun e (l)
    (let ((r ()))
      (dolist (x l)
	(push x r)
	(when (equal x 3)
	  (return r)))))

  (defun f (n)
    (let ((r 0))
      (dotimes (i n)
	(incf r i)
	(when (equal i 3)
	  (return r)))))

  )

(define-namespace i
    ((:import cl)
     (:export a b c d e f))

  (defun a (n) (cons n "a"))

  (defun b (n)
    (labels ((a (n) (if (= n 0) 1 (* n (a (1- n))))))
      (a n)))

  (defun c ()
    (labels ((a (l)
		(cond ((null l) '())
		      (t (append (a (cdr l)) (list (car l)))))))
      #'a))

  (defun d ()
    (labels ((a (x) (cons "d.a" x)))
      (labels ((a (x) (cons "d.a.a" x)))
	#'a)))

  (defun e ()
    (labels ((a (x) (cons "e.a" x)))
      (labels ((b (x) (cons "e.a.b" x)))
	#'a)))

  (defun f ()
    (labels ((b (x) (cons "e.a" x)))
      (labels ((b (x) (cons "e.a.b" x)))
	#'a)))

  )

(define-namespace j
    ((:export f h))

  (defmacro def (f) `(defun ,f () ,(symbol-name f)))

  (def f)
  (def g)

  (defmacro foo (x y) `(cons ,y ,x))

  (defun h (a b) (foo a b))

  )

(ert-deftest test-a.0 ()
  (should (equal (a-a 1 2) '(a--b (a-a 2 1)))))

(ert-deftest test-a.1 ()
  (should (equal (a--b 1) '(a--b 1))))

(ert-deftest test-a.2 ()
  (should (equal (funcall (elt (a-c) 0) 1 2) '(a--b (a-a 2 1)))))

(ert-deftest test-a.3 ()
  (should (equal (funcall (elt (a-c) 1) 1) '(a--b 1))))

(ert-deftest test-a.4 ()
  (should (equal (a-d 1) '(a--b (a-d 1)))))

(ert-deftest test-a.5 ()
  (should (equal (namespace-eval-in-namespace (namespace-find-namespace 'a)
					      '(b 42))
		 '(a--b 42))))

(ert-deftest test-b.1 ()
  (should (equal (b-e 1 2) '(a--b (a-a 2 (b-e 1))))))

(ert-deftest test-b.2 ()
  (should (equal (b-f 1) '(a--b (a-d (b-f 1))))))

(ert-deftest test-c.1 ()
  (should (equal (c-a 1 2) '(a--b (a-a 2 (c-a 1))))))

(ert-deftest test-c.2 ()
  (should (equal (c--c 1) '(a--b (a-d (c--c 1))))))

(ert-deftest test-d.1 ()
  (should (equal (d-b 1 2) '(a--b (a-a 2 (c-a (d-b 1)))))))

(ert-deftest test-e.1 ()
  (should (equal (e-a-x (e-make-a :x 234 :y 456))
		 234)))

(ert-deftest test-e.2 ()
  (should (e--a-p (e-make-a :x 234 :y 456))))

(ert-deftest test-e.3 ()
  (should (cl-typep (e-make-a :x 234 :y 456) 'e--a)))

(ert-deftest test-e.4 ()
  (should (equal (e--a-y (e-make-a :x 234 :y 456))
		 456)))

(ert-deftest test-e.5 ()
  (should (equal (e-b/x (e--make-b :x 234 :y 456))
		 234)))

(ert-deftest test-e.6 ()
  (should (equal (e-c-x (e-make-c :x 234 :y 456))
		 234)))

(ert-deftest test-e.7 ()
  (should (equal (e--c-y (e-make-c :x 234 :y 456))
		 456)))

(ert-deftest test-e.8 ()
  (should (e--c? (e-make-c :x 234 :y 456))))

(ert-deftest test-e.9 ()
  :expected-result :failed
  (should (cl-typep (e-make-c :x 234 :y 456) 'e-c)))

(ert-deftest test-f.1 ()
  (should (equal (f-a-x (f-make-a :x 123))
		 123)))

(ert-deftest test-f.2 ()
  (should (f--a-p (f-make-a :x 123))))

(ert-deftest test-f.3 ()
  (should (not (f--a-p (e-make-a :x 234 :y 456)))))

(ert-deftest test-f.4 ()
  (should (not (e--a-p (f-make-a :x 234)))))

(ert-deftest test-f.5 ()
  (should (cl-typep (f-make-a :x 234) 'f--a)))

(ert-deftest test-g.1 ()
  (should (e--a-p (g-a))))

(ert-deftest test-g.2 ()
  (should (equal (g-b (g-a)) 123)))

(ert-deftest test-g.3 ()
  (should (equal (e-a-x (g-d (g-a) 789))
		 789)))

(ert-deftest test-h.1 ()
  (should (equal (h-a) '(0 1 2 3 4))))

(ert-deftest test-h.2 ()
  (should (equal (h-b '(0 1 2 3 4))
		 2)))

(ert-deftest test-h.3 ()
  (should (equal (h-c '(0 1))
		 '(1 . 0))))

(ert-deftest test-h.4 ()
  (should (equal (h-d 2 '(0 1 2 3))
		 2)))

(ert-deftest test-h.5 ()
  (should (equal (h-e '(0 1 2 3 4 5))
		 '(3 2 1 0))))

(ert-deftest test-h.6 ()
  (should (equal (h-f 7) 6)))

(ert-deftest test-i.1 ()
  (should (equal (i-a 7) '(7 . "a"))))

(ert-deftest test-i.2 ()
  (should (equal (i-b 7) 5040)))

(ert-deftest test-i.3 ()
  (should (equal (funcall (i-c) '(a b c))
		 '(c b a))))

(ert-deftest test-i.4 ()
  (should (equal (funcall (i-d) 'i.4)
		 '("d.a.a" . i.4))))

(ert-deftest test-i.5 ()
  (should (equal (funcall (i-e) 'i.5)
		 '("e.a" . i.5))))

(ert-deftest test-i.6 ()
  (should (equal (funcall (i-f) 'i.6)
		 '(i.6 . "a"))))

(ert-deftest test-j.1 ()
  (should (equal (j-f) "f")))

(ert-deftest test-j.2 ()
  (should (equal (j--g) "g")))

(ert-deftest test-j.3 ()
  (should (equal (j-h 1 2) '(2 . 1))))

(define-namespace c1.a
  ((:export f)))

(define-namespace c1.b
  ((:export f)))

(ert-deftest test-conflict.1 ()
  (should (equal
	   (condition-case e
	       (eval '(define-namespace c1.c
			((:import c1.a c1.b))))
	     (namespace-import-conflict (cdr e)))
	   '(c1.c ((c1.a-f . c1.b-f))))))

(ert-deftest test-conflict.2 ()
  (should (equal
	   (condition-case e
	       (eval '(define-namespace c2.a
			((:import c1.a)
			 (:import (c1.b (:only f))))))
	     (namespace-import-conflict (cdr e)))
	   '(c2.a ((c1.a-f . c1.b-f))))))

(define-namespace c3.a
    ((:export f)))

(define-namespace c3.b
    ((:import c3.a)
     (:export g)))

(define-namespace c3.c
    ((:import c3.a c3.b)))

(define-namespace c3.d
    ((:import c3.a))
  (defun h () ))

(ert-deftest test-conflict.3 ()
  "Redefine c3.a so that it causes conflicts in c3.b abd c3.c."
  (should (equal
	   (namespace--collect-warnings
	    (eval '(define-namespace c3.a
		       ((:export f g)))))
	   '(((namespace-export-conflict c3.a
					 ((c3.b
					   ((c3.a-g . c3.b-g)))
					  (c3.c
					   ((c3.a-g . c3.b-g)))))
	      (namespace-mark-inconsistent c3.b)
	      (namespace-mark-inconsistent c3.c))
	     nil)))
  (should (namespace--inconsistent
	   (namespace--find-namespace-or-lose 'c3.b)))
  (should (namespace--inconsistent
	   (namespace--find-namespace-or-lose 'c3.c)))
  )

(ert-deftest test-conflict.4 ()
  "Redefine c3.a so that it causes conflicts with c3.d's internal h."
  (should (equal
	   (namespace--collect-warnings
	    (eval '(define-namespace c3.a
		       ((:export f h)))))
	   '(((namespace-previously-exported c3.a (c3.a-g))
	      (namespace-export-conflict c3.a
					 ((c3.d
					   ((c3.a-h . c3.d--h)))))
	      (namespace-mark-inconsistent c3.d))
	     nil))))

(ert-deftest test-conflict.5 ()
  "Incosistent namespaces can't be used as imports."
  (should (namespace--inconsistent (namespace--find-namespace-or-lose 'c3.b)))
  (should (equal
	   (condition-case e
	       (eval '(define-namespace c5.a
			  ((:import c3.b))))
	     (namespace-inconsistent (cdr e)))
	   '(c3.b))))

(define-namespace c6.a
    ((:export f)))

(define-namespace c6.b
    ((:export g)))

(define-namespace c6.c
    ((:import c6.a c6.b)))

(ert-deftest test-conflict.6 ()
  "Redefine c6.c so that it no longer uses c6.b.
That used to trigger an failed assertion during validate because
namespace--users was not being updated properly."
  (should (equal
	   (eval '(define-namespace c6.c
		      ((:import c6.a))))
	   nil)))

;;; namespace-test.el ends here
