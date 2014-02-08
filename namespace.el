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
;; qsym: qualified symbol (a distinct type)
;; csym: concatenated symbol (has type symbol)
;; id: unqualfied symbol (represented as string)

(cl-defstruct (namespace--qsym (:constructor namespace--qsym (ns id))
			       (:copier nil))
  (ns (error "Required argument missing") :type namespace :read-only t)
  (id (error "Required argument missing") :type string :read-only t))


(cl-defstruct (namespace--table (:constructor nil) (:copier nil))
  (table (error "Required argument missing") :read-only t
	 :type (or namespace namespace--table)))

(cl-defstruct (namespace--except-table (:include namespace--table)
				       (:constructor namespace--except-table
						     (table ids))
				       (:copier nil))
  (ids '() :type list :read-only t))

(cl-defstruct (namespace--only-table (:include namespace--table)
				     (:constructor namespace--only-table
						   (table alist))
				     (:copier nil))
  (alist '() :type list :read-only t))


(cl-defstruct (namespace (:constructor namespace)
			 (:predicate namespacep)
			 (:copier nil)
			 (:conc-name namespace--))
  (name (error "Required argument missing") :type symbol :read-only t)
  ;; namespaces (or namespace-tables) used by this namespace
  (imports '() :type list)
  ;; namespaces using this namespace
  (users '() :type list)
  ;; id -> qsym
  (internal (make-hash-table :test 'equal) :type hash-table :read-only t)
  ;; id -> qsym
  (external (make-hash-table :test 'equal) :type hash-table :read-only t)
  ;; set if we had an export conflict
  (inconsistent nil :type boolean)
  )

(defmacro namespace--ct (&rest decls)
  `(progn
     ,@(cl-loop for decl in decls collect
		(cl-destructuring-bind (type &rest names) decl
		  (let ((type (cl-case type
				(id 'string)
				(qsym 'namespace--qsym)
				(t type))))
		    `(progn
		       ,@(cl-loop for name in names collect
				  `(cl-check-type ,name ,type))))))))


(defun namespace--table-lookup (table id)
  (cl-etypecase table
    (namespace
     (gethash id (namespace--external table)))
    (namespace--except-table
     (cond ((member id (namespace--except-table-ids table)) nil)
	   (t (namespace--table-lookup (namespace--except-table-table table)
				       id))))
    (namespace--only-table
     (cdr (assoc id (namespace--only-table-alist table))))))

(defun namespace--hash-values (table)
  (let ((result '()))
    (maphash (lambda (_ val) (push val result))
	     table)
    result))

(defun namespace--table-map (table fun)
  (cl-etypecase table
    (namespace (maphash fun (namespace--external table)))
    (namespace--except-table
     (let ((ids (namespace--except-table-ids table)))
       (namespace--table-map (namespace--except-table-table table)
			     (lambda (id qsym)
			       (unless (member id ids)
				 (funcall fun id qsym))))))
    (namespace--only-table
     (cl-loop for (id . qsym) in (namespace--only-table-alist table)
	      do (funcall fun id qsym)))))

(defun namespace--table-values (table)
  (cl-etypecase table
    (namespace
     (namespace--hash-values (namespace--external table)))
    ((or namespace--except-table
	 namespace--only-table)
     (let ((result '()))
       (namespace--table-map table
			     (lambda (_id qsym)
			       (push qsym result)))
       result))))

(defun namespace--table-ns (table)
  (cl-etypecase table
    (namespace table)
    (namespace--table (namespace--table-ns (namespace--table-table table)))))


(defun namespace--find-internal (ns id)
  (namespace--ct (namespace ns) (id id))
  (gethash id (namespace--internal ns)))

(defun namespace--find-external (ns id)
  (namespace--ct (namespace ns) (id id))
  (gethash id (namespace--external ns)))

(defun namespace--find-imported (ns id)
  (namespace--ct (namespace ns) (id id))
  (cl-dolist (table (namespace--imports ns))
    (let ((qsym (namespace--table-lookup table id)))
      (when qsym
	(cl-return qsym)))))

(defun namespace--lookup (ns id)
  (namespace--ct (namespace ns) (string id))
  (cl-macrolet ((tag (tag test)
		     `(let ((qsym ,test))
			(and qsym (cons ,tag qsym)))))
    (or (tag :internal (namespace--find-internal ns id))
	(tag :external (namespace--find-external ns id))
	(tag :imported (namespace--find-imported ns id)))))

(defun namespace--intern (ns id)
  (let ((existing (namespace--lookup ns id)))
    (cond (existing (cdr existing))
	  (t (let ((qsym (namespace--qsym ns id)))
	       (puthash id qsym (namespace--internal ns))
	       qsym)))))

(defun namespace--exported-qsyms (ns)
  (namespace--hash-values (namespace--external ns)))

(defun namespace--accessible-qsyms (ns)
  (namespace--ct (namespace ns))
  (let ((internal (namespace--internal ns))
	(external (namespace--external ns)))
    (append (namespace--hash-values internal)
	    (namespace--hash-values external)
	    (cl-loop for e in (namespace--imports ns)
		     append (namespace--table-values e)))))

(defun namespace--map-qsyms (ns fun)
  (maphash (lambda (_ qsym) (funcall fun qsym))
	   (namespace--internal ns))
  (maphash (lambda (_ qsym) (funcall fun qsym))
	   (namespace--external ns)))


(defun namespace--conflicts-to-string (conflicts)
  (cl-loop for ((q1 . q2) . more) on conflicts
	   concat (format "%s %s"
			  (namespace--qsym-to-csym q1)
			  (namespace--qsym-to-csym q2))
	   when more concat "\n"))

(defun namespace--conflicts-to-csyms (conflicts)
  (cl-loop for (q1 . q2) in conflicts
	   collect (cons (namespace--qsym-to-csym q1)
			 (namespace--qsym-to-csym q2))))

(define-error 'namespace-error "Namespace error")
(define-error 'namespace-conflict "Name conflict" 'namespace-error)
(define-error 'namespace-import-conflict "Namespace :import conflict"
  'namespace-conflict)
(define-error 'namespace-export-conflict "Namespace :export conflict"
  'namespace-conflict)
(define-error 'namespace-inconsistent "Namespace marked as inconsistent"
  'namespace-error)

(defun namespace--error (type &rest args)
  (cl-ecase type
    (namespace-import-conflict
     (cl-destructuring-bind (ns conflicts) args
       (signal type (list (namespace--name ns)
			  (namespace--conflicts-to-csyms conflicts)))))
    (namespace-inconsistent
     (cl-destructuring-bind (ns) args
       (signal type (list (namespace--name ns)))))))

(defvar namespace--warning-handler nil)

(defun namespace--warning-to-sexp (w)
  (cons
   (car w)
   (cl-ecase (car w)
     (namespace-export-conflict
      (cl-destructuring-bind (ns cs) (cdr w)
	(list (namespace--name ns)
	      (cl-loop for (ns2 . cs2) in cs collect
		       (list (namespace--name ns2)
			     (namespace--conflicts-to-csyms cs2))))))
     (namespace-mark-inconsistent
      (cl-destructuring-bind (ns) (cdr w)
	(list (namespace--name ns))))
     (namespace-previously-exported
      (cl-destructuring-bind (ns qsyms) (cdr w)
	(list (namespace--name ns)
	      (mapcar #'namespace--qsym-to-csym qsyms)))))))

(defun namespace--warning-to-string (w)
  (pcase w
    (`(namespace-export-conflict ,ns ,cs)
     (concat
      (format "Export conflicts after re-defining namespace %s."
	      (namespace--name ns))
      (cl-loop for (ns2 . cs2) in cs concat
	       (format "\n   In namespace %s: %s"
		       (namespace--name ns2)
		       (namespace--conflicts-to-csyms cs2)))))
    (`(namespace-mark-inconsistent ,ns)
     (format "Marking namespace %s as inconsistent" (namespace--name ns)))
    (`(namespace-previously-exported ,ns ,qsyms)
      (format "Namespace %s previously exported: %s"
	      (namespace--name ns) (mapcar #'namespace--qsym-to-csym qsyms)))
    (_ (error "no pcase match: %s" w))))

(defun namespace--warn (type &rest args)
  (cond (namespace--warning-handler
	 (funcall namespace--warning-handler (cons type args)))
	(t
	 (display-warning 'namespace
	  (namespace--warning-to-string (cons type args))))))

(defmacro namespace--collect-warnings (form)
  `(let* ((fun (lambda () ,form))
	  (warnings '())
	  (namespace--warning-handler (lambda (w)
					(push (namespace--warning-to-sexp w)
					      warnings)))
	  (result (funcall fun)))
     (list (reverse warnings) result)))

(defun namespace--imports-conflicts (imports)
  (let ((conflicts '()))
    (cl-loop for (is . rest) on imports do
	     (namespace--table-map
	      is
	      (lambda (id qsym)
		(pcase (cl-loop for is2 in rest
				when (namespace--table-lookup is2 id)
				return it)
		  (`nil)
		  ((and qsym2 (guard (eq qsym qsym2))))
		  (qsym2 (push (cons qsym qsym2) conflicts))))))
    conflicts))

(defun namespace--check-import-conflicts (ns iss)
  (let ((conflicts (namespace--imports-conflicts iss)))
    (when conflicts
      (namespace--error 'namespace-import-conflict ns conflicts))))

;; Make NS and recursively its users as inconsistent
(defun namespace--mark-inconsistent (ns)
  (unless (namespace--inconsistent ns)
    (setf (namespace--inconsistent ns) t)
    (namespace--warn 'namespace-mark-inconsistent ns))
  (dolist (ns2 (namespace--users ns))
    (namespace--mark-inconsistent ns2)))

(defun namespace--check-export-conflicts (ns)
  (cl-assert (zerop (hash-table-count (namespace--internal ns))))
  (let ((conflicts '()))
    (dolist (ns2 (namespace--users ns))
      (let ((cs (namespace--imports-conflicts (namespace--imports ns2))))
	(when cs
	  (push (cons ns2 cs) conflicts)))
      (maphash (lambda (id qsym)
		 (let ((qsym2 (namespace--find-imported ns2 id)))
		   (when qsym2
		     (push (cons ns2 (list (cons qsym2 qsym))) conflicts))))
	       (namespace--internal ns2))
      (maphash (lambda (id qsym)
		 (let ((qsym2 (namespace--find-imported ns2 id)))
		   (when (and qsym2
			      (not (eq qsym qsym2)))
		     (push (cons ns2 (list (cons qsym2 qsym))) conflicts))))
	       (namespace--external ns2)))
    (when conflicts
      (namespace--warn 'namespace-export-conflict ns conflicts)
      (cl-loop for (ns2) in conflicts
	       do (namespace--mark-inconsistent ns2)))))


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

(defun namespace--read-namespace-name (&optional prompt)
  (read (completing-read (or prompt "Namespace: ")
			 namespace--table nil t)))


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
  (dolist (is (namespace--imports ns))
    (let ((ns2 (namespace--table-ns is)))
      (namespace--validate-unique ns2)
      (cl-assert (not (eq ns2 ns)))
      (cl-assert (member ns (namespace--users ns2)))))
  (dolist (ns2 (namespace--users ns))
    (namespace--validate-unique ns2)
    (cl-assert (not (eq ns2 ns)))
    (cl-assert (member ns (mapcar #'namespace--table-ns
				  (namespace--imports ns2)))))
  (maphash (lambda (id qsym)
	     (namespace--ct (id id) (qsym qsym))
	     (namespace--validate-qsym qsym)
	     (cl-assert (not (namespace--find-external ns id)))
	     (cl-assert (not (namespace--find-imported ns id))))
	   (namespace--internal ns))
  (maphash (lambda (id qsym)
	     (namespace--ct (id id) (qsym qsym))
	     (namespace--validate-qsym qsym)
	     (cl-assert (not (gethash id (namespace--internal ns)))))
	   (namespace--external ns)))

(defun namespace--validate ()
  (maphash (lambda (_ ns)
	     (unless (namespace--inconsistent ns)
	       (namespace--validate-ns ns)))
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

(defun namespace--sexp-filter-p (sexp)
  (pcase sexp
    ((and `(:only . ,syms)
	  (guard (namespace--list-of-symbols-p syms)))
     syms ; ignorable
     t)
    ((and `(:except . ,syms)
	  (guard (namespace--list-of-symbols-p syms)))
     syms ; ignorable
     t)
    (_ nil)))

(defun namespace--parse-filter (sexp)
  (pcase sexp
    ((and `(:only . ,syms)
	  (guard (namespace--list-of-symbols-p syms)))
     `(:only . ,(mapcar #'symbol-name syms)))
    ((and `(:except . ,syms)
	  (guard (namespace--list-of-symbols-p syms)))
     `(:except . ,(mapcar #'symbol-name syms)))
    (_ (error "bug"))))

(defun namespace--sexp-import-set-p (sexp)
  (pcase sexp
    ((and sym (guard (symbolp sym)))
     sym ; ignorable
     t)
    ((and `(,sym . ,filters)
	  (guard (symbolp sym))
	  (guard (cl-every #'namespace--sexp-filter-p filters)))
     sym filters ; ignorable
     t)
    (_ nil)))

(defun namespace--parse-import-set (sexp)
  (pcase sexp
    ((and sym (guard (symbolp sym)))
     (list sym))
    ((and `(,sym . ,filters)
	  (guard (symbolp sym))
	  (guard (cl-every #'namespace--sexp-filter-p filters)))
     (cons sym (mapcar #'namespace--parse-filter filters)))
    (_ (error "bug"))))

(defun namespace--parse-options (options)
  (let (import-sets exports)
    (dolist (option options)
      (pcase option
	((and `(:import . ,iss)
	      (guard (cl-every #'namespace--sexp-import-set-p iss)))
	 (setq import-sets
	       (append import-sets
		       (mapcar #'namespace--parse-import-set iss))))
	((and `(:export . ,names)
	      (guard (namespace--list-of-symbols-p names)))
	 (setq exports (append exports (namespace--stringify-names names))))
	(_ (error "Invalid namespace option: %S" option))))
    (namespace--check-disjoint
     (cons :export exports))
    (list import-sets exports)))



;; like namespace--intern but warns about interning
(defun namespace--find-or-make-qsym (ns name)
  (let ((existing (namespace--lookup ns name)))
    (cond (existing (cdr existing))
	  (t
	   (warn "Interning %s in namespace %s" name
		 (namespace--name ns))
	   (namespace--intern ns name)))))

(defun namespace--find-qsym-or-lose (ns name)
  (namespace--ct (namespace ns) (string name))
  (let ((x (namespace--lookup ns name)))
    (cond ((not x)
	   (error "Name %s no accessible in %s" name (namespace--name ns)))
	  (t
	   (cdr x)))))

(defun namespace--apply-filters (is filters)
  (pcase filters
    (`nil is)
    (`((:only . ,ids) . ,more)
     (let ((alist (cl-loop for id in ids collect
			   (cons id (or (namespace--table-lookup is id)
					(error "Id %S not exported in %S"
					       id (car filters)))))))
     (namespace--apply-filters (namespace--only-table is alist)
			       more)))
    (`((:except . ,ids) . ,more)
     (namespace--apply-filters (namespace--except-table is ids)
			       more))))

(defun namespace--set-imports (ns iss)
  (let ((iss (cl-loop for is in iss collect
		      (cl-destructuring-bind (ns . filters) is
			(let ((ns (namespace--find-namespace-or-lose ns)))
			  (when (namespace--inconsistent ns)
			    (namespace--error 'namespace-inconsistent ns))
			  (namespace--apply-filters ns filters))))))
    (namespace--check-import-conflicts ns iss)
    (let ((old-imports (namespace--imports ns)))
      (setf (namespace--imports ns) iss)
      (let ((old-using (mapcar #'namespace--table-ns old-imports))
	    (new-using (mapcar #'namespace--table-ns iss)))
	(dolist (ns2 old-using)
	  (unless (memq ns2 new-using)
	    (setf (namespace--users ns2)
		  (delete ns (namespace--users ns2)))))
	(dolist (ns2 new-using)
	  (cl-pushnew ns (namespace--users ns2)))))))


(defun namespace--set-exports (ns exports)
  (cl-assert (zerop (hash-table-count (namespace--internal ns))))
  (let* ((old-exports (namespace--exported-qsyms ns))
	 (new-exports (cl-loop for id in exports collect
			       (or (namespace--find-external ns id)
				   (namespace--find-imported ns id)
				   (namespace--qsym ns id))))
	 (diff (cl-set-difference old-exports new-exports)))
    (when diff
      (namespace--warn 'namespace-previously-exported ns diff))
    (let ((external (namespace--external ns)))
      (clrhash external)
      (dolist (qsym new-exports)
	(puthash (namespace--qsym-id qsym) qsym external)))
    (namespace--check-export-conflicts ns)))

;; FIXME: clearing out internal symbols is probably a good idea but
;; conflict dections may need updjustments.
(defun namespace--define (name imports exports)
  (namespace--validate)
  (let* ((ns (namespace--find-or-make-namespace name)))
    (namespace--set-imports ns imports)
    ;; no errors should happend after this point
    (clrhash (namespace--internal ns))
    (namespace--set-exports ns exports)
    (setf (namespace--inconsistent ns) nil)
    (namespace--validate-ns ns))
  (namespace--validate)
  name)

(defmacro define-namespace (name options &rest body)
  (declare (indent 2))
  (cl-destructuring-bind (import-sets exports)
      (namespace--parse-options options)
    `(progn
       (eval-and-compile
	 (namespace--define ',name ',import-sets ',exports))
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
  (let* ((name (namespace--qsym-id qsym))
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
	   collect (cons (intern (namespace--qsym-id qsym))
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
	   when (let ((name (namespace--qsym-id qsym))
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

(defun namespace--macrolet-env (fsyms env)
  (append `((namespace--fsyms . (lambda () ',fsyms))
	    (namespace--fcache . (lambda () ',(cons nil nil)))
	    (function . (lambda (sym)
			  (let ((env macroexpand-all-environment))
			    (namespace--function-expander sym env))))
	    ,@(cl-loop for (sym . csym) in fsyms
		       collect `(,sym . (lambda (&rest args)
					  `(,',csym . ,args)))))
	  env))

;; This does the same as te above macro but more efficiently.  The
;; above version creates lots of macrolets which will be expanded in a
;; recursive fashion that can overfow the stack quickly.  This version
;; uses the stack space more efficiently.
(cl-defmacro namespace--macrolet (fsyms &body body &environment env)
  (declare (indent 1))
  (macroexpand-all `(progn . ,body)
		   (namespace--macrolet-env fsyms env)))

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


(defun namespace--macroexpand-all-env (ns)
  (let ((fsyms (namespace--fsyms ns)))
    (namespace--macrolet-env (namespace--fsyms-to-csyms fsyms) nil)))

(defun namespace-macroexpand-all (ns form)
  (macroexpand-all form (namespace--macroexpand-all-env ns)))

(defun namespace-macroexpand1 (ns form)
  (let ((env (namespace--macroexpand-all-env ns)))
    (cond ((and (consp form)
		(symbolp (car form)))
	   (let* ((sym (car form))
		  (macro (or (cdr (assq sym env))
			     (namespace--macro-function sym)))
		  (macroexpand-all-environment env))
	     (cond (macro (apply macro (cdr form)))
		   (t form))))
	  (t (let ((macroexpand-all-environment env))
	       (macroexpand form))))))

(defun namespace-macroexpand (ns form)
  (let ((env (namespace--macroexpand-all-env ns)))
    (macroexpand form env)))

(defun namespace-eval-in-namespace (ns form &optional lexical)
  (eval (namespace-macroexpand-all ns form)
	lexical))


(defun namespace-map-accessible (ns fun)
  (dolist (qsym (namespace--accessible-qsyms ns))
    (let ((name (namespace--qsym-id qsym)))
      (funcall fun name))))

(provide 'namespace)

;; namespace.el ends here
