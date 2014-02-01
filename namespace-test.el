;; namespace-test.el --- tests for namespace.el

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

;;; Code:

(require 'namespace)
(require 'ert)
(require 'cl-lib)
(require 'cl-ns)

(define-namespace a
    ((:use )
     (:export a c d))

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
    ((:use a)
     (:export e f))

  (defun e (x y)
    (a (list 'b-e x) y))

  (defun f (x)
    (funcall #'d (list 'b-f x)))
  )

(define-namespace c
    ((:use a)
     (:shadow a c)
     (:export a))

  (defun a (x y)
    (a-a (list 'c-a x) y))

  (defun c (x)
    (funcall #'d (list 'c--c x)))
  )

(define-namespace d
    ((:use a)
     (:shadowing-import-from c a)
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

  (cl-defstruct a x y)

  (cl-defstruct (b (:conc-name b/))
    x y)

  (cl-defstruct (c (:predicate c?))
    x y)

  )

(define-namespace f
    ((:export make-a
	      a-x
	      make-c
	      c
	      c-x))

  (cl-defstruct a x)

  (cl-defstruct (c (:predicate c?))
    x)

  )

(define-namespace g
    ((:use e)
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
    ((:use cl)
     (:export a b c d e))

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
  ;;:expected-result :failed
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


;;; namespace-test.el ends here
