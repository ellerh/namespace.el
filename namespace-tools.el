;;; namespace-tools.el --- Namespace aware M-.     -*- lexical-binding: t -*-

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

;; Namespace aware M-. command and related support code.

;;; Code:

(require 'cl-ns)
(require 'etags)
(require 'find-func)
(eval-when-compile
  (require 'pcase))

(define-namespace namespace
    ((:export find-namespace
	      resolve
	      global
	      eval-in-namespace)))

(define-namespace namespace-tools
    ((:use cl namespace)
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

  (defun guess-namespace (sym)
    (let* ((file (symbol-file sym 'defun)))
      (when file
	(let ((nsnames (namespace-names-in-file (find-library-name file)))
	      (name (symbol-name sym)))
	  (dolist (nsname nsnames)
	    (when (string-match (concat "^" nsname "--?") name)
	      (let ((ns (find-namespace (intern nsname)))
		    (n (substring name (match-end 0))))
		(when (and ns (namespace--find-name ns n))
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

  (defun eval-sexp (ns sexp insert-result)
    (let ((standard-output (if insert-result (current-buffer) t)))
      ;; Setup the lexical environment if lexical-binding is enabled.
      (eval-last-sexp-print-value
       (eval-in-namespace ns sexp lexical-binding)
       insert-result)))

  (defun eval-last-sexp (insert-result)
    (interactive "P")
    (let ((ns (current-namespace)))
      (cond (ns
	     (eval-sexp ns (preceding-sexp) insert-result))
	    (t
	     (global (eval-last-sexp insert-result))))))

  (defun activate ()
    (interactive)
    (ad-activate 'find-function-search-for-symbol)
    (define-key emacs-lisp-mode-map (kbd "M-.")
      'namespace-tools-find-definition)
    (define-key emacs-lisp-mode-map (kbd "M-,") 'pop-tag-mark)
    (define-key emacs-lisp-mode-map (kbd "C-x C-e")
      'namespace-tools-eval-last-sexp)
    )

  )

(defadvice find-function-search-for-symbol (after namespace--unqualified)
  (let ((loc ad-return-value)
	(sym (ad-get-arg 0)))
    (pcase loc
      (`(,_ . nil)
       (let ((guess (namespace-tools--guess-namespace sym)))
	 (pcase guess
	   (`(,_ . ,name)
	    (let ((loc2 (namespace-tools--locate-name name sym)))
	      (pcase loc2
		((or `nil `(,_ . nil)) loc)
		(`(,buffer . ,pos)
		 (setq ad-return-value loc2)))))
	   (_ loc))))
      (_ loc))))

;;;###autoload(autoload 'namespace-tools-activate "namespace-tools" nil t)

(provide 'namespace-tools)

;; namespace-tools.el ends here