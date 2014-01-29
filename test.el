;; test.el --- tests for namespace.el

(require 'namespace)
(require 'ert)
(require 'cl-lib)

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
