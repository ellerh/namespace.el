;;; namespace-tools.el --- Namespace aware M-.          -*-lexical-binding:t-*-

;; Copyright (C) 2014 Helmut Eller
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Helmut Eller <eller.helmut@gmail.com>

;;; Commentary:

;; Namespace aware M-., indentation, eldoc and similar commands for
;; ELisp programming.  Proof of concept trying to show that many
;; commands would only need minor adaptions to make them namespace
;; aware.

;;; Code:

(eval-when-compile
  (require 'pcase))
(require 'cl-ns)
(require 'etags)
(require 'find-func)
(require 'eldoc)
(require 'pp)
(require 'thingatpt)
(require 'lisp-mode)
(require 'nadvice)

(define-namespace namespace
    ((:export find-namespace
	      resolve
	      global
	      eval-in-namespace
	      map-accessible)))

(define-namespace namespace-tools
    ((:import cl namespace)
     (:export find-definition
	      eval-last-sexp
	      activate))

  (defun push-tag-mark ()
    (ring-insert find-tag-marker-ring (point-marker)))

  (defun extract-namespace-name ()
    (save-excursion
      (down-list)
      (forward-sexp 2)
      (let ((end (point)))
	(backward-sexp)
	(buffer-substring (point) end))))

  ;; Return the namespace at point (or nil).
  (defun current-namespace ()
    (save-excursion
      (and (re-search-backward "^(" nil t)
	   (looking-at "(define-namespace")
	   (let ((name (intern (extract-namespace-name))))
	     (find-namespace name)))))

  (defun namespace-names-in-file (file)
    (with-current-buffer (find-file-noselect file)
      (save-excursion
	(goto-char (point-min))
	(loop while (re-search-forward "^(define-namespace" nil t)
	      collect (save-excursion
			(goto-char (match-beginning 0))
			(extract-namespace-name))))))

  ;; Try to guess the namespace for symbol SYM.  Return either nil or
  ;; (NS . NAME) where NS is a namespace and NAME is a string.
  (defun guess-namespace (sym)
    (let* ((file (symbol-file sym 'defun)))
      (when file
	(let ((nsnames (namespace-names-in-file (find-library-name file)))
	      (name (symbol-name sym)))
	  (dolist (nsname nsnames)
	    (when (string-match (concat "^" nsname "--?") name)
	      (let ((ns (find-namespace (intern nsname)))
		    (n (substring name (match-end 0))))
		(when (and ns (namespace--lookup ns n))
		  (return (cons ns n))))))))))

  ;; skip over whitespace to next ( if needed
  (defun align-loc (loc)
    (pcase loc
      ((or `nil `(,_ . nil)) loc)
      (`(,buffer . ,pos)
       (with-current-buffer buffer
	 (save-excursion
	   (goto-char pos)
	   (cond ((looking-at "\s +(")
		  (goto-char (1- (match-end 0)))
		  (cons buffer (point)))
		 (t loc)))))))

  (defun locate-name (name rsym)
    (let* ((fun (symbol-function rsym))
	   (file (cond ((autoloadp fun) (nth 1 fun))
		       ((subrp fun) (help-C-file-name fun 'subr))
		       (t (symbol-file rsym 'defun)))))
      (or file (error "Can't determine file for: %s" rsym))
      (save-excursion
	(align-loc (find-function-search-for-symbol (make-symbol name)
						    nil file)))))

  (defun locate-symbol (sym)
    (let ((loc (save-excursion (find-function-noselect sym))))
      (pcase loc
	(`(,_ . nil)
	 (let ((guess (guess-namespace sym)))
	   (pcase guess
	     (`(,_ . ,name) (locate-name name sym))
	     (_ loc))))
	(_ loc))))

  (defun find-definition (name)
    "Jump to the definition of the function (or variable) at point."
    (interactive (list (cond (current-prefix-arg
			      (read-from-minibuffer "Name: "))
			     (t (or (thing-at-point 'symbol)
				    (error "No symbol at point"))))))
    (let* ((ns (current-namespace))
	   (rsym (and ns (resolve ns name)))
	   (sym (intern-soft name))
	   (loc (cond ((fboundp rsym) (locate-symbol rsym))
		      ((fboundp sym) (locate-symbol sym))
		      ((boundp sym) (find-variable-noselect sym))
		      (t (error "Symbol not bound: %s" name))))
	   (buffer (car loc))
	   (point (cdr loc)))
      (cond (point
	     (push-tag-mark)
	     (switch-to-buffer buffer)
	     (goto-char point)
	     (recenter 1))
	    (t
	     (error "Found no definition for %s in %s"
		    (substring-no-properties name) buffer)))))

  (defun search-for-symbol-advice (next symbol type library)
    (let ((loc (funcall next symbol type library)))
      (pcase loc
	(`(,_ . nil)
	 (let ((guess (namespace-tools--guess-namespace symbol)))
	   (pcase guess
	     (`(,_ . ,name)
	      (let ((loc2 (namespace-tools--locate-name name symbol)))
		(pcase loc2
		  ((or `nil `(,_ . nil)) loc)
		  (`(,_buffer . ,_pos)
		   loc2))))
	     (_ loc))))
	(_ loc))))

  (defun call-name-at-point ()
    (ignore-errors
      (save-excursion
	(backward-up-list 1)
	(down-list 1)
	(thing-at-point 'symbol))))

  (defun resolve-fbound (name ns)
    (or (and ns
	     (let ((rsym (resolve ns name)))
	       (and (fboundp rsym)
		    rsym)))
	(let ((sym (intern-soft name)))
	  (and sym
	       (fboundp sym)
	       sym))))

  (defun function-symbol-at-point ()
    (let ((ns (current-namespace)))
      (or (let ((name (thing-at-point 'symbol)))
	    (and name (resolve-fbound name ns)))
	  (let ((name (call-name-at-point)))
	    (and name (resolve-fbound name ns))))))

  (defun function-called-at-point-advice (next)
    (or (namespace-tools--function-symbol-at-point)
	(funcall next)))

  (defun arglist (sym)
    (let ((fun (symbol-function sym))
	  (doc (help-split-fundoc (documentation sym t) nil)))
      (cond ((consp doc) (cdr (read (car doc))))
	    (t (help-function-arglist fun)))))

  (defun eldoc ()
    (let ((sym (function-symbol-at-point)))
      (and sym
	   (let* ((args (arglist sym))
		  (idx (cadr (eldoc-fnsym-in-current-sexp)))
		  (argstring (eldoc-function-argstring
			      (loop for a in args collect (format "%s" a)))))
	     (eldoc-highlight-function-argument sym argstring idx)))))

  (defun completions-for (ns prefix)
    (let ((result '()))
      (map-accessible ns (lambda (name)
			   (when (string-prefix-p prefix name)
			     (push name result))))
      result))

  ;; Return completions at point.
  (defun completions ()
    (let ((ns (current-namespace))
	  (bounds (bounds-of-thing-at-point 'symbol)))
      (and ns
	   bounds
	   (let* ((start (car bounds))
		  (end (point))
		  (prefix (buffer-substring-no-properties start end))
		  (list (append (completions-for ns prefix)
				(all-completions prefix obarray
						 (lambda (sym)
						   (or (fboundp sym)
						       (boundp sym)))))))
	     (list start end list :exclusive 'no)))))

  ;;FIXME: make tab completion in minibuffer work
  (defun eval-expression (namespace exp)
    (interactive
     (let* ((ns (current-namespace))
	    (prompt (format "%s> " (cond (ns (namespace--name ns))
					 (t "(global)"))))
	    (exp (read--expression prompt)))
       (list ns exp)))
    (let ((ns namespace))
      (global (eval-expression
	       (cond (ns `(namespace-eval-in-namespace ',ns ',exp))
		     (t exp))))))

  (defun eval-sexp-add-defvars-advice (next exp &rest rest)
    (let ((exp (let ((ns (current-namespace)))
		 (cond (ns (namespace-macroexpand-all ns exp))
		       (t exp)))))
      (apply next exp rest)))

  (defun pp (sexp)
    (with-output-to-temp-buffer "*Pp Eval Output*"
      (global (pp sexp))
      (with-current-buffer standard-output
	(emacs-lisp-mode))))

  (defun pp-last-sexp-advice (next)
    (let* ((sexp (funcall next))
	   (ns (current-namespace)))
      (cond (ns `(namespace-eval-in-namespace ',ns ',sexp lexical-binding))
	    (t sexp))))

  (defun macroexpand1 (ns form)
    (cond (ns (namespace-macroexpand1 ns form))
	  (t (let ((macro (and (consp form)
			       (symbolp (car form))
			       (symbol-function (car form)))))
	       (cond ((and macro (consp macro)
			   (eq (car macro) 'macro))
		      (apply (cdr macro) (cdr form)))
		     (t form))))))

  (defun macroexpand (ns form)
    (cond (ns (namespace-macroexpand ns form))
	  (t (global (macroexpand form)))))

  (defun macroexpand-all (ns form)
    (cond (ns (namespace-macroexpand-all ns form))
	  (t (global (macroexpand-all form)))))

  (defun expand (&optional repeatedly)
    (interactive "P")
    (let* ((form (form-at-point 'sexp))
	   (ns (current-namespace))
	   (expanded (cond (repeatedly (macroexpand ns form))
			   (t (macroexpand1 ns form)))))
      (pp expanded)))

  (defun expand-all (form)
    (interactive (list (form-at-point 'sexp)))
    (pp (macroexpand-all (current-namespace) form)))

  ;; The indendation code is copied from lisp-mode's
  ;; lisp-indent-function and refactored a bit.  lookup-indent-method
  ;; is the key bit that figures out how to indent a particular
  ;; symbol.

  (defvar calculate-lisp-indent-last-sexp)

  (defun indent-non-call-form ()
    (when (not (> (save-excursion (forward-line 1) (point))
		  calculate-lisp-indent-last-sexp))
      (goto-char calculate-lisp-indent-last-sexp)
      (beginning-of-line)
      (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t))
    ;; Indent under the list or under the first sexp on the same
    ;; line as calculate-lisp-indent-last-sexp.  Note that first
    ;; thing on that line has to be complete sexp since we are
    ;; inside the innermost containing sexp.
    (backward-prefix-chars)
    (current-column))

  (defun lookup-indent-method (symbol-name)
    (let* ((ns (current-namespace))
	   (sym (cond (ns (resolve ns symbol-name))
		      (t (intern-soft symbol-name)))))
      (or (function-get sym 'lisp-indent-function)
	  (get sym 'lisp-indent-hook)
	  (and (> (length symbol-name) 3)
	       (string-match "\\`def" symbol-name)))))

  (defun indent-call-form (indent-point state normal-indent)
    (let* ((function (buffer-substring-no-properties
		      (point) (progn (forward-sexp 1) (point))))
	   (method (lookup-indent-method function)))
      (cond ((eq method 'defun)
	     (lisp-indent-defform state indent-point))
	    ((integerp method)
	     (lisp-indent-specform method state indent-point normal-indent))
	    (method
	     (funcall method indent-point state)))))

  (defun lisp-indent-function (indent-point state)
    (let ((normal-indent (current-column)))
      (goto-char (1+ (elt state 1)))
      (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
      (cond ((and (elt state 2)
		  (not (looking-at "\\sw\\|\\s_")))
	     ;; car of form doesn't seem to be a symbol
	     (indent-non-call-form))
	    (t
	     (indent-call-form indent-point state normal-indent)))))

  (defun lisp-indent-function-advice (next indent-point state)
    (cond ((current-namespace)
	   (lisp-indent-function indent-point state))
	  (t
	   (funcall next indent-point state))))

  (defun elisp-mode-hook ()
    (add-hook 'completion-at-point-functions
	      'namespace-tools--completions
	      nil 'local))

  (defmacro advice* (&rest body)
    `(progn
       ,@(cl-loop for (name kind fun) in body
		  collect `(advice-add ',name ,kind (function ,fun)
				       '((name . namespace-tools))))))

  (defun deactivate ()
    (interactive)
    (progn
      (dolist (sym '(find-function-search-for-symbol
		     function-called-at-point
		     eval-sexp-add-defvars
		     pp-last-sexp
		     lisp-indent-function))
	(advice-remove sym 'namespace-tools))))

  (defun activate ()
    "Activate advice and bind keys."
    (interactive)
    (advice*
     (find-function-search-for-symbol :around search-for-symbol-advice)
     (function-called-at-point :around function-called-at-point-advice)
     (eval-sexp-add-defvars :around eval-sexp-add-defvars-advice)
     (pp-last-sexp :around pp-last-sexp-advice)
     (lisp-indent-function :around lisp-indent-function-advice))
    (setq eldoc-documentation-function #'eldoc)
    (let ((map emacs-lisp-mode-map))
      (define-key map (kbd "M-.") (fsym find-definition))
      (define-key map (kbd "M-,") 'pop-tag-mark)
      (define-key map (kbd "C-c :") (fsym eval-expression))
      (define-key map (kbd "C-c m") (fsym expand))
      (define-key map (kbd "C-c M") (fsym expand-all)))
    (add-hook 'emacs-lisp-mode-hook (fsym elisp-mode-hook))
    ;; enable completion in all elisp buffers
    (dolist (b (buffer-list))
      (with-current-buffer b
	(when (eq major-mode 'emacs-lisp-mode)
	  (elisp-mode-hook)))))

  )

;;;###autoload
(progn
  (autoload 'namespace-tools-activate "namespace-tools" nil t))

(provide 'namespace-tools)

;; namespace-tools.el ends here
