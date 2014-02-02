;;; namespace-srcloc.el --- Namespace aware M-.     -*- lexical-binding: t -*-

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

(define-namespace namespace-srcloc
    ((:use cl)
     (:export find-definition bind-keys))

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
	     (namespace-find-namespace name)))))

  (defun locate-name (name rsym)
    (let* ((fun (symbol-function rsym))
	   (file (cond ((autoloadp fun) (nth 1 fun))
		       ((subrp fun) (help-C-file-name fun 'subr))
		       (t (symbol-file rsym 'defun)))))
      (or file (error "Can't determine file for: %s" rsym))
      (save-excursion
	(find-function-search-for-symbol (make-symbol name) nil file))))

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
	  (loop for nsname in nsnames
		when (string-match (concat "^" nsname "--?") name)
		return (cons (namespace-find-namespace (intern nsname))
			     (substring name (match-end 0))))))))

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
    (interactive (list (thing-at-point 'symbol)))
    (unless name
      (error "No symbol at point"))
    (let* ((ns (current-namespace))
	   (rsym (and ns (namespace-resolve ns name)))
	   (sym (intern-soft name))
	   (loc (cond ((fboundp rsym) (locate-name name rsym))
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

  (defun bind-keys ()
    (define-key emacs-lisp-mode-map (kbd "M-.")
      'namespace-srcloc-find-definition)
    (define-key emacs-lisp-mode-map (kbd "M-,") 'pop-tag-mark))

  )

(provide 'namespace-srcloc)

;; namespace-srcloc.el ends here
