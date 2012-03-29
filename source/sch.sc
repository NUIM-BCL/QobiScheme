;;; LaHaShem HaAretz U'Mloah
;;; Copyright 1993, 1994, and 1995 University of Toronto. All rights reserved.
;;; Copyright 1996 Technion. All rights reserved.
;;; Copyright 1996 and 1997 University of Vermont. All rights reserved.
;;; Copyright 1997, 1998, 1999, 2000, and 2001 NEC Research Institute, Inc. All
;;; rights reserved.
;;; Copyright 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, and 2011
;;; Purdue University. All rights reserved.

(module sch (with QobiScheme xlib) (main main))

(include "QobiScheme.sch")

(set! *program* "sch")
(set! *panic?* #t)

;;; Procedures

(define (proper-module-declaration main? pathname pathnames)
 (if main?
     `(module ,(string->symbol
		(string-upcase (strip-directory (strip-extension pathname))))
	      (with ,@(append '(QobiScheme xlib)
			      (map (lambda (pathname)
				    (string->symbol
				     (string-upcase
				      (strip-directory
				       (strip-extension pathname)))))
				   pathnames)))
	      (main main))
     `(module ,(string->symbol
		(string-upcase
		 (strip-directory (strip-extension pathname)))))))

(define (module-declaration pathname)
 (call-with-input-file (default-extension pathname "sc")
  (lambda (input-port)
   (let ((expression (read input-port)))
    (and (not (eof-object? expression))
	 (list? expression)
	 (>= (length expression) 1)
	 (eq? (first expression) 'module)
	 expression)))))

(define (enforce-proper-module-declaration main? pathname pathnames)
 (let ((module-declaration (module-declaration pathname)))
  (unless (and module-declaration
	       (equal? module-declaration
		       (proper-module-declaration main? pathname pathnames)))
   (let ((lines (call-with-input-file (default-extension pathname "sc")
		 (lambda (input-port)
		  (when module-declaration
		   (read input-port)
		   ;; To get rid of the newline at the end of the module
		   ;; declaration.
		   (read-line input-port))
		  (let loop ((lines '()) (line (read-line input-port)))
		   (if (eof-object? line)
		       (reverse lines)
		       (loop (cons line lines) (read-line input-port))))))))
    (call-with-output-file (default-extension pathname "sc")
     (lambda (output-port)
      (pp (proper-module-declaration main? pathname pathnames) output-port)
      (newline output-port)
      (for-each (lambda (line)
		 (display line output-port)
		 (newline output-port))
		lines)))))))

(define (symbols-in-file pathname)
 (let ((symbols '()))
  (define (walk expression)
   (cond
    ((symbol? expression)
     (unless (memq expression symbols)
      (set! symbols (cons expression symbols))))
    ((pair? expression) (walk (first expression)) (walk (rest expression)))))
  (call-with-input-file (default-extension pathname "sc")
   (lambda (input-port)
    (let loop ()
     (let ((expression (read input-port)))
      (unless (eof-object? expression) (walk expression) (loop))))))
  symbols))

(define (needed-definitions-in-file pathname symbols)
 (unless (equal? (module-declaration pathname)
		 (proper-module-declaration #f pathname '()))
  (panic "~a contains a missing or improper MODULE declaration" pathname))
 (call-with-input-file (default-extension pathname "sc")
  (lambda (input-port)
   (let ((module (second (read input-port))))
    (let loop ()
     (let ((definition (read input-port)))
      (if (eof-object? definition)
	  '()
	  (cond
	   ((and (pair? definition)
		 (eq? (first definition) 'define)
		 (pair? (rest definition))
		 (memq (if (pair? (second definition))
			   (first (second definition))
			   (second definition))
		       symbols))
	    (cons `(define-external ,(second definition) ,module) (loop)))
	   ((and (pair? definition)
		 (eq? (first definition) 'define-structure)
		 (or (memq (string->symbol
			    (string-append
			     "MAKE-"
			     (symbol->string (second definition))))
			   symbols)
		     (memq (string->symbol
			    (string-append
			     (symbol->string (second definition))
			     "?"))
			   symbols)
		     (some (lambda (slot)
			    (or (memq (string->symbol
				       (string-append
					(symbol->string (second definition))
					"-"
					(symbol->string slot)))
				      symbols)
				(memq (string->symbol
				       (string-append
					"SET-"
					(symbol->string (second definition))
					"-"
					(symbol->string slot)
					"!"))
				      symbols)
				(memq (string->symbol
				       (string-append
					"LOCAL-SET-"
					(symbol->string (second definition))
					"-"
					(symbol->string slot)
					"!"))
				      symbols)))
			   (rest (rest definition)))))
	    (cons `(define-structure-external ,module ,@(rest definition))
		  (loop)))
	   (else (loop))))))))))

(define (needed-definitions-in-files pathnames symbols)
 (reduce append
	 (map (lambda (pathname) (needed-definitions-in-file pathname symbols))
	      pathnames)
	 '()))

(define (read-target pathname)
 (call-with-input-file pathname
  (lambda (input-port)
   (let loop ()
    (let ((definition (read input-port)))
     (if (eof-object? definition) '() (cons definition (loop))))))))

;;; Top Level

(define-command (main (at-most-one ("main" main?))
		      (required (pathname "pathname" string-argument))
		      (rest (pathnames "pathname" string-argument)))
 ;; needs work: This is needed because of a bug in QobiScheme.
 (let ((pathnames (reverse pathnames)))
  (enforce-proper-module-declaration main? pathname pathnames)
  (for-each
   (lambda (pathname) (enforce-proper-module-declaration #f pathname '()))
   pathnames)
  (let* ((symbols (symbols-in-file pathname))
	 (target-pathname (replace-extension pathname "sch"))
	 (new-definitions (needed-definitions-in-files pathnames symbols)))
   (when (or (not (can-open-file-for-input? target-pathname))
	     (not (equal? new-definitions (read-target target-pathname))))
    (call-with-output-file target-pathname
     (lambda (output-port)
      (for-each (lambda (definition)
		 (write definition output-port)
		 (newline output-port))
		new-definitions)))))))

;;; Tam V'Nishlam Shevah L'El Borei Olam
