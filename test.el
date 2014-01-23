
(require 'namespace)
(require 'ert)

;; (namespace--delete 'a)

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

(ert-deftest namespace-test-a.0 ()
  (should (equal (a-a 1 2) '(a--b (a-a 2 1)))))

(ert-deftest namespace-test-a.1 ()
  (should (equal (a--b 1) '(a--b 1))))

(ert-deftest namespace-test-a.2 ()
  (should (equal (funcall (elt (a-c) 0) 1 2) '(a--b (a-a 2 1)))))

(ert-deftest namespace-test-a.3 ()
  (should (equal (funcall (elt (a-c) 1) 1) '(a--b 1))))

(ert-deftest namespace-test-a.4 ()
  (should (equal (a-d 1) '(a--b (a-d 1)))))

(ert-deftest namespace-test-b.1 ()
  (should (equal (b-e 1 2) '(a--b (a-a 2 (b-e 1))))))

(ert-deftest namespace-test-b.2 ()
  (should (equal (b-f 1) '(a--b (a-d (b-f 1))))))

(ert-deftest namespace-test-c.1 ()
  (should (equal (c-a 1 2) '(a--b (a-a 2 (c-a 1))))))

(ert-deftest namespace-test-c.2 ()
  (should (equal (c--c 1) '(a--b (a-d (c--c 1))))))

(ert-deftest namespace-test-d.1 ()
  (should (equal (d-b 1 2) '(a--b (a-a 2 (c-a (d-b 1)))))))
