;;; namespace.el --- Namespaces as macro             -*- lexical-binding: t -*-

;; Copyright (C) 2014 Helmut Eller

;; Author: Helmut Eller <eller.helmut@gmail.com>
;; Version: 0.1

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

(require 'cl-lib)
(eval-when-compile
  (require 'pcase))

;; A bit of terminology:
;; qsym: a qualified symbol (a distinct type)
;; csym: a concatenated symbol (has type symbol)

(cl-defstruct (namespace--qsym (:constructor namespace--qsym (ns name))
			       (:copier nil))
  (ns (error "Required argument missing") :type namespace :read-only t)
  (name (error "Required argument missing") :type string :read-only t))

(cl-defstruct (namespace (:constructor namespace)
			 (:predicate namespacep)
			 (:copier nil)
			 (:conc-name namespace--))
  (name (error "Required argument missing") :type symbol :read-only t)
  ;; namespaces used by this namespace
  (exporters '() :type list)
  ;; namespaces using this namespace
  (importers '() :type list)
  ;; string -> qsym
  (internal (make-hash-table :test 'equal) :type hash-table :read-only t)
  ;; string -> qsym
  (external (make-hash-table :test 'equal) :type hash-table :read-only t)
  ;; string -> qsym
  ;; these names are also in either internal or external.
  (shadows (make-hash-table :test 'equal) :type hash-table :read-only t)
  )

(defmacro namespace--ct (&rest decls)
  `(progn
     ,@(cl-loop for decl in decls collect
		(cl-destructuring-bind (type &rest names) decl
		  `(progn
		     ,@(cl-loop for name in names collect
				`(cl-check-type ,name ,type)))))))

(defun namespace--lookup (ns name)
  (namespace--ct (namespace ns) (string name))
  (or (let ((qsym (gethash name (namespace--internal ns))))
	(and qsym
	     (cons :internal qsym)))
      (let ((qsym (gethash name (namespace--external ns))))
	(and qsym
	     (cons :external qsym)))
      (cl-dolist (e (namespace--exporters ns))
	(let ((qsym (gethash name (namespace--external e))))
	  (when qsym
	    (cl-return (cons :inherited qsym)))))))

(defun namespace--intern (ns name)
  (let ((existing (namespace--lookup ns name)))
    (cond (existing (cdr existing))
	  (t (let ((qsym (namespace--qsym ns name)))
	       (puthash name qsym (namespace--internal ns))
	       qsym)))))

(defun namespace--hash-values (table)
  (let ((result '()))
    (maphash (lambda (_ val) (push val result))
	     table)
    result))

(defun namespace--shadowing-qsyms (ns)
  (namespace--hash-values (namespace--shadows ns)))

(defun namespace--exported-qsyms (ns)
  (namespace--hash-values (namespace--external ns)))

(defun namespace--accessible-qsyms (ns)
  (namespace--ct (namespace ns))
  (let ((internal (namespace--internal ns))
	(external (namespace--external ns)))
    (append (namespace--hash-values internal)
	    (namespace--hash-values external)
	    (let ((sh (make-hash-table :test 'equal)))
	      (cl-loop for e in (namespace--exporters ns)
		       do (maphash (lambda (name qsym)
				     (unless (or (gethash name sh)
						 (gethash name internal)
						 (gethash name external))
				       (puthash name qsym sh)))
				   (namespace--external e)))
	      (namespace--hash-values sh)))))

(defun namespace--map-qsyms (ns fun)
  (maphash (lambda (_ qsym) (funcall fun qsym))
	   (namespace--internal ns))
  (maphash (lambda (_ qsym) (funcall fun qsym))
	   (namespace--external ns)))

(defun namespace--shadow (ns names)
  (let ((internal (namespace--internal ns))
	(shadows (namespace--shadows ns)))
    (dolist (name names)
      (let ((qsym (pcase (namespace--lookup ns name)
		    ((or `nil
			 `(:inherited . ,qsym))
		     (let ((qsym (namespace--qsym ns name)))
		       (puthash name qsym internal)
		       qsym))
		    ((or `(:internal . ,qsym)
			 `(:external . ,qsym))
		     qsym)
		    (_ (error "bug")))))
	(puthash name qsym shadows)))))

(defun namespace--shadowing-import (ns qsym)
  (namespace--ct (namespace ns) (namespace--qsym qsym))
  (let ((internal (namespace--internal ns))
	(shadows (namespace--shadows ns))
	(name (namespace--qsym-name qsym)))
    (let* ((probe (namespace--lookup ns name)))
      (pcase probe
	(`nil
	 (puthash name qsym internal))
	((and (or `(:internal . ,qsym2)
		  `(:external . ,qsym2))
	      (guard (eq qsym2 qsym)))
	 qsym2)
	((or `(:internal . ,qsym2)
	     `(:external . ,qsym2))
	 (namespace--unintern ns qsym2)
	 (puthash name qsym internal))
	(_
	 (puthash name qsym internal)))
      (puthash name qsym shadows))))

(defun namespace--conflicts-to-string (conflicts)
  (cl-loop for (q1 . q2) in conflicts
	   concat (format "%s %s\n"
			  (namespace--qsym-to-csym q1)
			  (namespace--qsym-to-csym q2))))

(defun namespace--conflicts-to-csyms (conflicts)
  (cl-loop for (q1 . q2) in conflicts
	   collect (cons (namespace--qsym-to-csym q1)
			 (namespace--qsym-to-csym q2))))

(define-error 'namespace-error "Namespace error")
(define-error 'namespace-conflict "Name conflict" 'namespace-error)
(define-error 'namespace-use-conflict "Namespace :use conflict"
  'namespace-conflict)
(define-error 'namespace-import-conflict "Namespace :import-from conflict"
  'namespace-conflict)
(define-error 'namespace-export-conflict "Namespace :export conflict"
  'namespace-conflict)
(define-error 'namespace-unintern-conflict "Conflict revealed by unintern"
  'namespace-conflict)

(defun namespace--error (type &rest args)
  (signal type args))

(defun namespace--conflict (type ns conflicts)
  (namespace--error type
		    (namespace--name ns)
		    (namespace--conflicts-to-csyms conflicts)))

(defun namespace--check-use-conflicts (ns ns2)
  (let ((conflicts '()))
    (maphash
     (lambda (name qsym2)
       (pcase (namespace--lookup ns name)
	 (`(,_ . ,qsym)
	  (when (and (not (eq qsym qsym2))
		     (not (gethash name (namespace--shadows ns))))
	    (push (cons qsym qsym2) conflicts)))
	 (`nil)
	 (_ (error "bug"))))
     (namespace--external ns2))
    (when conflicts
      (namespace--conflict 'namespace-use-conflict ns conflicts))))

(defun namespace--use1 (ns ns2)
  (unless (member ns2 (namespace--exporters ns))
    (namespace--check-use-conflicts ns ns2)
    (push ns2 (namespace--exporters ns))
    (push ns (namespace--importers ns2))))

(defun namespace--use (ns nss)
  (dolist (ns2 nss)
    (namespace--use1 ns ns2)))

(defun namespace--unuse1 (ns ns2)
  (setf (namespace--exporters ns) (remove ns2 (namespace--exporters ns)))
  (setf (namespace--importers ns2) (remove ns (namespace--importers ns2))))

(defun namespace--unuse (ns nss)
  (dolist (ns2 nss)
    (namespace--unuse1 ns ns2)))

(defun namespace--check-import-conflicts (ns qsyms)
  (let ((imports '())
	(conflicts '()))
    (dolist (qsym qsyms)
      (let* ((name (namespace--qsym-name qsym))
	     (found (cl-find name imports :key #'namespace--qsym-name
			     :test #'equal)))
	(unless (eq found qsym)
	  (when found
	    (push (cons qsym found) conflicts))
	  (pcase (namespace--lookup ns name)
	    (`nil
	     (push qsym imports))
	    ((and `(,_ . ,qsym2)
		  (guard (not (eq qsym qsym2))))
	     (push (cons qsym qsym2) conflicts))
	    (`(:inherited . ,_)
	     (push qsym imports))
	    (_ (error "bug"))))))
    (when conflicts
      (namespace--conflict 'namespace-import-conflict ns conflicts))
    imports))

(defun namespace--import (ns qsyms)
  (let ((imports (namespace--check-import-conflicts ns qsyms))
	(internal (namespace--internal ns)))
    (dolist (qsym imports)
      (puthash (namespace--qsym-name qsym) qsym internal))))

(defun namespace--check-export-conflicts (ns qsyms)
  (let ((importers (namespace--importers ns))
	(conflicts '()))
    (dolist (qsym qsyms)
      (let ((name (namespace--qsym-name qsym)))
	(dolist (ns2 importers)
	  (pcase (namespace--lookup ns2 name)
	    (`(,_ . ,qsym2)
	     (cond ((eq qsym2 qsym))
		   ((gethash name (namespace--shadows ns2)))
		   (t (push (cons qsym qsym2) conflicts))))
	    (_ (error "bug"))))))
    (when conflicts
      (namespace--conflict 'namespace-export-conflict ns conflicts)))
  (let ((missing '())
	(imports '()))
    (dolist (qsym qsyms)
      (pcase (namespace--lookup ns (namespace--qsym-name qsym))
	(`nil (push qsym missing))
	((and `(,_ . ,qsym2) (guard (eq qsym2 qsym))) qsym2)
	(`(:inherited . ,qsym2) qsym2 (push qsym imports))
	(_ (error "bug?"))))
    (when missing
      (error "Export of non-accessible symbols from namespace %s: %S"
	     (namespace--name ns) missing))
    imports))

(defun namespace--export (ns qsyms)
  (let* ((internal (namespace--internal ns))
	 (external (namespace--external ns))
	 (qsyms (cl-loop for qsym in qsyms
			 unless (gethash (namespace--qsym-name qsym) external)
			 collect qsym))
	 (imports (namespace--check-export-conflicts ns qsyms)))
    (namespace--import ns imports)
    (dolist (qsym qsyms)
      (let ((name (namespace--qsym-name qsym)))
	(remhash name internal)
	(puthash name qsym external)))))

(defun namespace--check-unintern-conflict (ns qsym)
  (let ((name (namespace--qsym-name qsym)))
    (when (gethash name (namespace--shadows ns))
      (let ((conflicts '()))
	(dolist (ns2 (namespace--importers ns))
	  (let ((qsym2 (gethash name (namespace--external ns2))))
	    (when qsym2
	      (cl-pushnew qsym2 conflicts))))
	(when (cdr conflicts)
	  (namespace--error 'namespace-unintern-conflict
			    (namespace--name ns)
			    (mapcar #'namespace--qsym-to-csym conflicts)))))))

(defun namespace--unintern (ns qsym)
  (let ((name (namespace--qsym-name qsym)))
    (when (or (eq (gethash name (namespace--internal ns)) qsym)
	      (eq (gethash name (namespace--external ns)) qsym))
      (namespace--check-unintern-conflict ns qsym)
      (remhash name (namespace--shadows ns))
      (remhash name (namespace--internal ns))
      (remhash name (namespace--external ns)))))


;; symbol -> namespace
(defvar namespace--table (make-hash-table))

(defun namespace-find-namespace (name)
  (namespace--ct (symbol name))
  (gethash name namespace--table))

(defun namespace--find-or-make-namespace (name)
  (namespace--ct (symbol name))
  (or (gethash name namespace--table)
      (setf (gethash name namespace--table)
	    (namespace :name name))))

(defun namespace--find-namespace-or-lose (name)
  (or (namespace-find-namespace name)
      (error "Namespace doesn't exist: %S" name)))

(defun namespace--delete-qsym (qsym)
  (maphash (lambda (_ ns) (namespace--unintern ns qsym))
	   namespace--table))

(defun namespace--read-namespace-name (&optional prompt)
  (read (completing-read (or prompt "Namespace: ")
			 namespace--table nil t)))

(defun namespace-delete-namespace (name)
  (interactive (list (namespace--read-namespace-name "Delete namespace: ")))
  (namespace--ct (symbol name))
  (namespace--validate)
  (let* ((ns (namespace--find-namespace-or-lose name)))
    (when (namespace--importers ns)
      (warn "Namespace %s used by: %s." (namespace--name ns)
	    (mapcar #'namespace--name (namespace--importers ns)))
      (dolist (ns2 (namespace--importers ns))
	(namespace--unuse1 ns2 ns)))
    (namespace--unuse ns (namespace--exporters ns))
    (maphash (lambda (_ ns2)
	       (namespace--map-qsyms ns2
				     (lambda (qsym)
				       (when (eq (namespace--qsym-ns qsym) ns)
					 (namespace--unintern ns2 qsym)))))
	     namespace--table)
    (remhash name namespace--table)
    (namespace--validate)))


(defun namespace--stringify-names (names)
  (mapcar #'symbol-name names))

(defun namespace--symconc (prefix separator name)
  (intern (concat (symbol-name prefix)
		  (symbol-name separator)
		  name)))

(defun namespace--proper-list-p (x)
  (while (consp x)
    (setq x (cdr x)))
  (null x))

(defun namespace--list-of-symbols-p (x)
  (and (namespace--proper-list-p x)
       (cl-every #'symbolp x)))

(defun namespace--validate-unique (ns)
  (cl-assert (eq (namespace-find-namespace (namespace--name ns))
		 ns)))

(defun namespace--validate-qsym (qsym)
  (namespace--validate-unique (namespace--qsym-ns qsym)))

(defun namespace--validate-ns (ns)
  (namespace--validate-unique ns)
  (dolist (ns2 (namespace--exporters ns))
    (namespace--validate-unique ns2)
    (cl-assert (not (eq ns2 ns)))
    (cl-assert (member ns (namespace--importers ns2))))
  (dolist (ns2 (namespace--importers ns))
    (namespace--validate-unique ns2)
    (cl-assert (not (eq ns2 ns)))
    (cl-assert (member ns (namespace--exporters ns2))))
  (maphash (lambda (name qsym)
	     (cl-assert
	      (or (let ((qsym2 (gethash name (namespace--internal ns))))
		    (and qsym2 (eq qsym2 qsym)))
		  (let ((qsym2 (gethash name (namespace--external ns))))
		    (and qsym2 (eq qsym2 qsym))))))
	   (namespace--shadows ns))
  (maphash (lambda (name qsym)
	     (namespace--validate-qsym qsym)
	     (cl-assert (not (gethash name (namespace--external ns)))))
	   (namespace--internal ns))
  (maphash (lambda (name qsym)
	     (namespace--validate-qsym qsym)
	     (cl-assert (not (gethash name (namespace--internal ns)))))
	   (namespace--external ns)))

(defun namespace--validate ()
  (maphash (lambda (_ ns) (namespace--validate-ns ns))
	   namespace--table))


(defun namespace--check-disjoint (&rest args)
  ;; Check whether all given arguments specify disjoint sets of names.
  ;; Each argument is of the form (:key . set).
  (cl-loop for ((key1 . set1) . as) on args do
	   (cl-loop for (key2 . set2) in as do
		    (let ((common (cl-intersection set1 set2 :test #'equal)))
		      (when common
			(error
			 "Parameters %S and %S not disjoint. Common items: %S"
			 key1 key2 common))))))

(defun namespace--parse-options (options)
  (let (shadows shadowing-imports use imports interns exports)
    (dolist (option options)
      (pcase option
	((and `(:shadow . ,names)
	      (guard (namespace--list-of-symbols-p names)))
	 (setq shadows (append shadows (namespace--stringify-names names))))
	((and `(:shadowing-import-from ,ns . ,names)
	      (guard (and (symbolp ns)
			  (namespace--list-of-symbols-p names))))
	 (let ((assoc (assoc ns shadowing-imports))
	       (names (namespace--stringify-names names)))
	   (cond (assoc (setcdr assoc (append (cdr assoc) names)))
		 (t (push (cons ns names) shadowing-imports)))))
	((and `(:use . ,nss)
	      (guard (namespace--list-of-symbols-p nss)))
	 (setq use (cl-remove-duplicates (append nss use))))
	((and `(:import-from ,ns . ,names)
	      (guard (and (symbolp ns)
			  (namespace--list-of-symbols-p names))))
	 (let ((assoc (assoc ns imports))
	       (names (namespace--stringify-names names)))
	   (cond (assoc (setcdr assoc (append (cdr assoc) names)))
		 (t (push (cons ns names) imports)))))
	((and `(:intern . ,names)
	      (guard (namespace--list-of-symbols-p names)))
	 (setq interns (append interns (namespace--stringify-names names))))
	((and `(:export . ,names)
	      (guard (namespace--list-of-symbols-p names)))
	 (setq exports (append exports (namespace--stringify-names names))))
	(_ (error "Invalid namespace option: %S" option))))
    (namespace--check-disjoint
     (cons :intern interns)
     (cons :export exports))
    (namespace--check-disjoint
     (cons :intern interns)
     (cons :shadow shadows)
     (cons :import-from (cl-loop for (nil . names) in imports append names))
     (cons :shadowing-import-from
	   (cl-loop for (nil . names) in shadowing-imports
		    append names)))
    (list shadows shadowing-imports use imports interns exports)))



;; like namespace--intern but warns about interning
(defun namespace--find-or-make-qsym (ns name)
  (let ((existing (namespace--lookup ns name)))
    (cond (existing (cdr existing))
	  (t
	   (warn "Interning %s in namespace %s" name
		 (namespace--name ns))
	   (namespace--intern ns name)))))

(defun namespace--add-shadows (ns shadows shadowing-imports)
  (let ((old-shadows (namespace--shadowing-qsyms ns)))
    (namespace--shadow ns shadows)
    (dolist (name shadows)
      (setq old-shadows (remove (namespace--lookup ns name) old-shadows)))
    (cl-loop for (other-ns . names) in shadowing-imports do
	     (let ((other-ns (namespace--find-namespace-or-lose other-ns)))
	       (dolist (name names)
		 (let ((qsym (namespace--find-or-make-qsym other-ns name)))
		   (namespace--shadowing-import ns qsym)
		   (setq old-shadows (remove qsym old-shadows))))))
    (when old-shadows
      (warn "Namespace %s also shadows the following symbols: %S"
	    (namespace--name ns)
	    (mapcar #'namespace--qsym-to-csym old-shadows)))))

(defun namespace--add-use (ns use)
  (let ((old-exporters (namespace--exporters ns))
	(new-exporters (mapcar #'namespace--find-namespace-or-lose use)))
    (namespace--use ns new-exporters)
    (let ((unused (cl-set-difference old-exporters new-exporters)))
      (when unused
	(namespace--unuse ns unused)
	(warn "Namespace %s previously used: %s"
	      (namespace--name ns)
	      (mapcar #'namespace--name unused))))))

(defun namespace--find-qsym-or-lose (ns name)
  (namespace--ct (namespace ns) (string name))
  (let ((x (namespace--lookup ns name)))
    (cond ((not x)
	   (error "Name %s no accessible in %s" name (namespace--name ns)))
	  (t
	   (cdr x)))))

(defun namespace--add-imports (ns imports)
  (cl-loop for (other-ns . names) in imports do
	   (let* ((other-ns (namespace--find-namespace-or-lose other-ns))
		  (qsyms (mapcar (lambda (name)
				   (namespace--find-qsym-or-lose other-ns
								 name))
				 names)))
	     (namespace--import ns qsyms))))

(defun namespace--add-exports (ns exports)
  (let* ((old-exports (namespace--exported-qsyms ns))
	 (new-exports (cl-loop for name in exports
			       collect (namespace--intern ns name)))
	 (diff (cl-set-difference old-exports new-exports)))
    (namespace--export ns new-exports)
    (when diff
      (warn "Namspace %s also exports: %s" (namespace--name ns) diff))))

;; FIXME: clearing out internal symbols is probably a good idea but
;; conflict dections may need updjustments.
(defun namespace--define (name shadows shadowing-imports use
			       imports interns exports)
  (namespace--validate)
  (let* ((ns (namespace--find-or-make-namespace name)))
    (clrhash (namespace--internal ns))
    (clrhash (namespace--shadows ns))
    (namespace--add-shadows ns shadows shadowing-imports)
    (namespace--add-use ns use)
    (dolist (name interns)
      (namespace--intern ns name))
    (namespace--add-imports ns imports)
    (namespace--add-exports ns exports))
  (namespace--validate)
  name)

(defmacro define-namespace (name options &rest body)
  (declare (indent 2))
  (cl-destructuring-bind
      (shadows shadowing-imports use imports interns exports)
      (namespace--parse-options options)
    `(progn
       (eval-and-compile
	 (namespace--define ',name ',shadows ',shadowing-imports
			    ',use ',imports ',interns ',exports))
       (namespace--progn ,name . ,body))))


;;;; warning about unused a seems like bug
;;(defun foo (x)
;;  (pcase x
;;    ((and `(,a ,b)
;;	  (guard (symbolp a)))
;;     b)))

(defun namespace--name-external-p (ns name)
  (pcase (namespace--lookup ns name)
    (`(:external . ,_) t)
    (_ nil)))

(defun namespace--qsym-to-csym (qsym)
  (let* ((name (namespace--qsym-name qsym))
	 (ns (namespace--qsym-ns qsym))
	 (nsname (namespace--name ns)))
    (namespace--symconc nsname
			(if (namespace--name-external-p ns name) '- '--)
			name)))

(defun namespace-resolve (ns name)
  (namespace--ct (namespace ns) (string name))
  (let ((existing (namespace--lookup ns name)))
    (cond (existing (namespace--qsym-to-csym (cdr existing)))
	  (t (intern-soft name)))))

(defvar namespace--rewriters (make-hash-table))

(cl-defmacro namespace-define-rewriter (name (env form) &body body)
  (declare (indent 2))
  (let ((fname (intern (format "namespace--rw-%s" name))))
    `(progn
       (defun ,fname (,env ,form)
	 ,env ;; ignorable
	 . ,body)
       (puthash ',name #',fname namespace--rewriters))))

(defun namespace--constantly (value)
  (apply-partially #'identity value))

(defun namespace--fsyms (ns)
  (cl-loop for qsym in (namespace--accessible-qsyms ns)
	   collect (cons (intern (namespace--qsym-name qsym))
			 qsym)))

;; Expand and rewrite FORMS.
;; Return (FSYMS BODY) where FSYMS is an alist (SYMBOL . QSYM).
;;
;; FIXME: remove duplicates in FSYMS
(defun namespace--walk-toplevel (ns env forms)
  (let ((fsyms (cons nil (namespace--fsyms ns)))
	(rbody '())
	(env (cons (cons 'namespace--ns (namespace--constantly ns))
		   env)))
    (while (not (null forms))
      (let ((form (pop forms)))
	(pcase form
	  ((and `(,op . ,_)
		(let rewrite (gethash op namespace--rewriters))
		(guard rewrite))
	   op
	   (cl-destructuring-bind (fss rform recursep)
	       (funcall rewrite env form)
	     (setcdr fsyms (append fss (cdr fsyms)))
	     (cond (recursep (push rform forms))
		   (t (push rform rbody)))))
	  (`(progn . ,rest)
	   (setq forms (append rest forms)))
	  ((and `(,_ . ,_)
		(let expansion (macroexpand form env))
		(guard (not (eq expansion form))))
	   (push expansion forms))
	  (_
	   (push form rbody)))))
    (list (cdr fsyms)
	  (reverse rbody))))

(defun namespace--add-internal (namespace names)
  (let ((ns (namespace--find-namespace-or-lose namespace)))
    (dolist (name names)
      (namespace--intern ns name)))
  (namespace--validate))

(defun namespace--internal-fsyms (ns fsyms)
  (cl-loop for (nil . qsym) in fsyms
	   when (let ((name (namespace--qsym-name qsym))
		      (ns2 (namespace--qsym-ns qsym)))
		  (and (eq ns2 ns)
		       (not (namespace--name-external-p ns name))
		       name))
	   collect it))

;; This similar is to cl--labels-convert; both are hacks to shadow the
;; (function X) special form.  This exploits the fact that macroexpand
;; stops macroexpanding when an expander returns the same form twice.
(defun namespace--function-expander (sym env)
  (or (let* ((fsyms (macroexpand '(namespace--fsyms) env))
	     (probe (assoc sym fsyms)))
	(and probe `(symbol-function ',(cdr probe))))
      (let ((cache (macroexpand '(namespace--fcache) env)))
	(cond ((eq (car cache) sym)
	       (cdr cache))
	      (t
	       (let ((form `(function ,sym)))
		 (setcar cache sym)
		 (setcdr cache form)
		 form))))))

;;(cl-defmacro namespace--macrolet (fsyms &body body)
;;  (declare (indent 1))
;;  `(cl-macrolet ((namespace--fsyms () ',fsyms)
;;		 (namespace--fcache () ',(cons nil nil))
;;		 (function (sym &environment env)
;;			   (namespace--function-expander sym env))
;;		 ,@(cl-loop for (sym . csym) in fsyms
;;			    collect `(,sym (&rest args)
;;					   `(,',csym . ,args))))
;;     . ,body))


;; This does the same as te above macro but more efficiently.  The
;; above version creates lots of macrolets which will be expanded in a
;; recursive fashion that can overfow the stack quickly.  This version
;; uses the stack space more efficiently.
(cl-defmacro namespace--macrolet (fsyms &body body &environment env)
  (declare (indent 1))
  (let ((env2 (append
	       `((namespace--fsyms . (lambda () ',fsyms))
		 (namespace--fcache . (lambda () ',(cons nil nil)))
		 (function . (lambda (sym)
			       (let ((env macroexpand-all-environment))
				 (namespace--function-expander sym env))))
		 ,@(cl-loop for (sym . csym) in fsyms
			    collect `(,sym . (lambda (&rest args)
					       `(,',csym . ,args)))))
	       env)))
    (macroexpand-all `(progn . ,body) env2)))

(defun namespace--fsyms-to-csyms (fsyms)
  (cl-loop for (sym . qsym) in fsyms
	   collect (cons sym (namespace--qsym-to-csym qsym))))

(cl-defmacro namespace--progn (namespace &body body &environment env)
  (declare (indent 1))
  (let ((ns (namespace--find-namespace-or-lose namespace)))
    (cl-destructuring-bind (fsyms body) (namespace--walk-toplevel ns env body)
      `(progn
	 (namespace--add-internal ',namespace
				  ',(namespace--internal-fsyms ns fsyms))
	 (namespace--macrolet ,(namespace--fsyms-to-csyms fsyms)
	   . ,body)))))


;;; defun and friends
;;

(defun namespace--defun-like (env form)
  (let ((ns (funcall (cdr (assoc 'namespace--ns env)))))
    (pcase form
      (`(,op ,sym . ,rest)
       (let* ((qsym (namespace--intern ns (symbol-name sym)))
	      (csym (namespace--qsym-to-csym qsym)))
	 (list `((,sym . ,qsym))
	       `(,op ,csym . ,rest)
	       nil)))
      (_ (error "syntax error: %S" form)))))

(defmacro namespace--define-defun-like (&rest names)
  `(progn
     ,@(cl-loop for name in names collect
		`(namespace-define-rewriter ,name (env form)
		   (namespace--defun-like env form)))))

(namespace--define-defun-like
 defun defun* cl-defun
 defmacro defmacro*  cl-defmacro
 define-compiler-macro cl-define-compiler-macro)


;;; defstruct
;;
;; Dealing with defstruct is fairly tricky.  The basic idea is to
;; translate
;;
;;   (defstruct foo x)
;;
;; to
;;
;;   (defstruct (ns--foo (:constructor ns--make-foo)
;;			 (:predicate ns--foo-p)
;;			 (:copier ns--copy-foo))
;;     x)
;;
;; and if needed, i.e. if foo-x is exported:
;;
;;   (defalias 'ns-foo-x 'ns--foo-x)
;;
;; If the type name, foo, is exported we also declare a type alias:
;;
;;   (deftype ns-foo () 'ns--foo)
;;
;; deftype is not lexically scoped and we can't use the macrolet
;; tricks as for functions; therefore type names must always be fully
;; qualified.

(defun namespace--defstruct-options (ns options name)
  (let* ((string (symbol-name name))
	 (fs '())
	 (opts '())
	 (options
	  (append options
		  (unless (assoc :constructor options)
		    `((:constructor ,(namespace--symconc 'make '- string))))
		  (unless (assoc ':predicate options)
		    `((:predicate ,(namespace--symconc name '- "p"))))
		  (unless (assoc :copier options)
		    `((:copier ,(namespace--symconc 'copy '- string)))))))
    (while options
      (pcase (pop options)
	((and `(,op ,name . ,args)
	      (guard (memq op '(:constructor :predicate :copier)))
	      (guard (and (symbolp name) (not (eq name nil)))))
	 (let* ((qsym (namespace--intern ns (symbol-name name)))
		(csym (namespace--qsym-to-csym qsym)))
	   (push (cons name qsym) fs)
	   (push `(,op ,csym . ,args) opts)))
	(`(:conc-name ,prefix)
	 (let* ((qsym (namespace--intern ns (symbol-name prefix)))
		(csym (namespace--qsym-to-csym qsym)))
	   (push `(:conc-name ,csym) opts)))
	(o (push o opts))))
    (list fs opts)))

(defun namespace--slot-aliases (ns slots name options)
  (let ((fs '())
	(aliases '())
	(conc-name (or (cadr (assoc :conc-name options))
		       (namespace--symconc name '- ""))))
    (dolist (slot slots)
      (let* ((n (pcase slot
		  (`(,n . ,_) n)
		  ((and n (guard (symbolp n))) n)
		  (_ (error "bug"))))
	     (n (concat (symbol-name conc-name) (symbol-name n)))
	     (qsym (namespace--intern ns n)))
	(push (cons (intern n) qsym) fs)
	(when (namespace--name-external-p ns n)
	  (let ((in (namespace--symconc (namespace--name ns) '-- n))
		(csym (namespace--qsym-to-csym qsym)))
	    (push `(defalias ',csym ',in) aliases)
	    (push `(gv-define-setter ,csym (value struct)
		     `(setf (,',in ,struct) ,value))
		  aliases)))))
    (list fs aliases)))

(defun namespace--defstruct-like (env form)
  (pcase-let* ((`(,name ,options ,slots)
		(pcase form
		  ((and `(,_ ,name . ,slots) (guard (symbolp name)))
		   (list name '() slots))
		  ((and `(,_ (,name . ,options) . ,slots))
		   (list name options slots))
		  (_ (error "syntax error in defstruct: %S" form))))
	       (ns (funcall (cdr (assoc 'namespace--ns env))))
	       (string (symbol-name name))
	       (nsname (namespace--name ns))
	       (tname (namespace--symconc nsname '-- string))
	       (extern (eq (car (namespace--lookup ns string))
			   :external))
	       (`(,fs1 ,options2) (namespace--defstruct-options
				   ns options name))
	       (`(,fs2 ,aliases) (namespace--slot-aliases ns slots
							  name options))
	       (def `(progn
		       (cl-defstruct (,tname . ,options2) . ,slots)
		       ,@(when extern
			   `((cl-deftype ,(namespace--symconc nsname '- string)
					 () ',tname)))
		       . ,aliases)))
    (list (append fs1 fs2) def nil)))

(namespace-define-rewriter defstruct (env form)
  (namespace--defstruct-like env form))

(namespace-define-rewriter cl-defstruct (env form)
  (namespace--defstruct-like env form))

(namespace-define-rewriter defadvice (env form)
  form
  (error "defadvice not allowed in namespace"))


(defun namespace--macro-function (name)
  (pcase (symbol-function name)
    (`(macro . ,fun) fun)
    (_ nil)))

(cl-defmacro namespace-global ((name &rest args))
  (let ((macro (namespace--macro-function name)))
    (cond (macro (apply macro args))
	  (t `(funcall ',name . ,args)))))


(defun namespace-eval-in-namespace (ns form &optional lexical)
  (let ((fsyms (namespace--fsyms ns)))
    (eval `(namespace--macrolet ,(namespace--fsyms-to-csyms fsyms) ,form)
	  lexical)))


(defun namespace-map-accessible (ns fun)
  (dolist (qsym (namespace--accessible-qsyms ns))
    (let ((name (namespace--qsym-name qsym)))
      (funcall fun name))))

(provide 'namespace)

;; namespace.el ends here
