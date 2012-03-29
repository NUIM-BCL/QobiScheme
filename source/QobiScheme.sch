;;; LaHaShem HaAretz U'Mloah
;;; Copyright 1993, 1994, and 1995 University of Toronto. All rights reserved.
;;; Copyright 1996 Technion. All rights reserved.
;;; Copyright 1996 and 1997 University of Vermont. All rights reserved.
;;; Copyright 1997, 1998, 1999, 2000, and 2001 NEC Research Institute, Inc. All
;;; rights reserved.
;;; Copyright 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, and 2011
;;; Purdue University. All rights reserved.

(include "xlib.sch")

;;; System Conditionalization

(define-external *cpu-type* QobiScheme)
(define-external (cpu-type) QobiScheme)
(define-external *os-type* QobiScheme)
(define-external (os-type) QobiScheme)
(define-external *os-version* QobiScheme)
(define-external (os-version) QobiScheme)
(define-external *os-major-version* QobiScheme)
(define-external (os-major-version) QobiScheme)
(define-external *os-minor-version* QobiScheme)
(define-external (os-minor-version) QobiScheme)
(define-external *os-sub-version* QobiScheme)
(define-external (os-sub-version) QobiScheme)

;;; Sugar

(eval-when (compile)
 (define (first x) (car x))
 (define (second x) (cadr x))
 (define (third x) (caddr x))
 (define (fourth x) (cadddr x))
 (define (fifth x) (car (cddddr x)))
 (define (sixth x) (cadr (cddddr x)))
 (define (seventh x) (caddr (cddddr x)))
 (define (eighth x) (cadddr (cddddr x)))
 (define (ninth x) (car (cddddr (cddddr x))))
 (define (tenth x) (cadr (cddddr (cddddr x))))
 (define (eleventh x) (caddr (cddddr (cddddr x))))
 (define (twelfth x) (cadddr (cddddr (cddddr x))))
 (define (rest x) (cdr x)))

(define-external (first x) QobiScheme)
(define-external (second x) QobiScheme)
(define-external (third x) QobiScheme)
(define-external (fourth x) QobiScheme)
(define-external (fifth x) QobiScheme)
(define-external (sixth x) QobiScheme)
(define-external (seventh x) QobiScheme)
(define-external (eighth x) QobiScheme)
(define-external (ninth x) QobiScheme)
(define-external (tenth x) QobiScheme)
(define-external (eleventh x) QobiScheme)
(define-external (twelfth x) QobiScheme)
(define-external (rest x) QobiScheme)
(define-external (last x) QobiScheme)
(define-external (sqr x) QobiScheme)
(define-external (xor a b) QobiScheme)
(define-external (identity x) QobiScheme)
(define-external (nan? x) QobiScheme)

(define-macro while
 (lambda (form expander)
  (let ((loop (string->uninterned-symbol "loop")))
   (expander
    `(begin (define (,loop) (when ,(second form) ,@(rest (rest form)) (,loop)))
	    (,loop))
    expander))))

(define-external *panic?* QobiScheme)
(define-external *program* QobiScheme)
(define-external (panic message . &rest) QobiScheme)
(define-external (fuck-up) QobiScheme)
(define-external (usage format-string argv) QobiScheme)
(define-external (compose . fs) QobiScheme)
(define-external (rounded-number->string x . digits-of-precision) QobiScheme)
(define-external (number->string-of-length number length) QobiScheme)
(define-external (number->padded-string-of-length number length) QobiScheme)
(define-external
 (number->string-of-length-and-precision number length precision)
 QobiScheme)
(define-external (time format-string thunk) QobiScheme)
(define-external (getenv string) QobiScheme)
(define-external (archive-date) QobiScheme)
(define-external (getpid) QobiScheme)
(define-external (username) QobiScheme)
(define-external (unique-temporary-file file) QobiScheme)
(define-external (with-temporary-file prefix f) QobiScheme)

;;; Structures

(define-macro define-structure
 ;; needs work: To check that SLOTS is disjoint.
 (lambda (form expander)
  (unless (and (>= (length form) 3)
	       (let loop ((x (cdr form)))
		(or (null? x) (and (symbol? (car x)) (loop (cdr x))))))
   (error 'define-structure "Improper DEFINE-STRUCTURE: ~s" form))
  (expander
   (let ((type (second form))
	 (slots (rest (rest form)))
	 (v (string->uninterned-symbol "v")))
    `(begin
      (define (,(string->symbol (string-append "MAKE-" (symbol->string type)))
	       ,@slots)
       (let ((,v (make-vector ,(+ (length slots) 1))))
	(vector-set! ,v 0 ',type)
	,@(map-indexed (lambda (s i) `(vector-set! ,v ,(+ i 1) ,s)) slots)
	,v))
      (define (,(string->symbol (string-append (symbol->string type) "?"))
	       obj)
       (and (vector? obj)
	    (= (vector-length obj) ,(- (length form) 1))
	    (eq? (vector-ref obj 0) ',type)))
      ,@(map-indexed
	 (lambda (slot i)
	  `(begin
	    (define (,(string->symbol
		       (string-append "LOCAL-SET-"
				      (symbol->string type)
				      "-"
				      (symbol->string slot)
				      "!"))
		     type
		     obj)
	     (unless (,(string->symbol
			(string-append (symbol->string type) "?"))
		      type)
	      (panic ,(string-append "LOCAL-SET-"
				     (symbol->string type)
				     "-"
				     (symbol->string slot)
				     "! error")))
	     (local-vector-set! type ,(+ i 1) obj))
	    (define (,(string->symbol
		       (string-append (symbol->string type)
				      "-"
				      (symbol->string slot)))
		     type)
	     (unless (,(string->symbol
			(string-append (symbol->string type) "?"))
		      type)
	      (panic ,(string-append (symbol->string type)
				     "-"
				     (symbol->string slot)
				     " error")))
	     (vector-ref type ,(+ i 1)))
	    (define (,(string->symbol
		       (string-append "SET-"
				      (symbol->string type)
				      "-"
				      (symbol->string slot)
				      "!"))
		     type
		     obj)
	     (unless (,(string->symbol
			(string-append (symbol->string type) "?"))
		      type)
	      (panic ,(string-append "SET-"
				     (symbol->string type)
				     "-"
				     (symbol->string slot)
				     "! error")))
	     (vector-set! type ,(+ i 1) obj))))
	 slots)))
   expander)))

(define-macro define-structure-external
 ;; needs work: To check that SLOTS is disjoint.
 (lambda (form expander)
  (unless (and (>= (length form) 3)
	       (let loop ((x (cdr form)))
		(or (null? x) (and (symbol? (car x)) (loop (cdr x))))))
   (error 'define-structure-external
	  "Improper DEFINE-STRUCTURE-EXTERNAL: ~s" form))
  (expander
   (let ((module (second form))
	 (type (third form))
	 (slots (rest (rest (rest form)))))
    `(begin
      (define-external
       (,(string->symbol (string-append "MAKE-" (symbol->string type)))
	,@slots)
       ,module)
      (define-external
       (,(string->symbol (string-append (symbol->string type) "?")) obj)
       ,module)
      ,@(map-indexed
	 (lambda (slot i)
	  `(begin
	    (define-external (,(string->symbol
				(string-append "LOCAL-SET-"
					       (symbol->string type)
					       "-"
					       (symbol->string slot)
					       "!"))
			      type
			      obj)
	     ,module)
	    (define-external (,(string->symbol
				(string-append (symbol->string type)
					       "-"
					       (symbol->string slot)))
			      type)
	     ,module)
	    (define-external (,(string->symbol
				(string-append "SET-"
					       (symbol->string type)
					       "-"
					       (symbol->string slot)
					       "!"))
			      type
			      obj)
	     ,module)))
	 slots)))
   expander)))

;;; Sequences

(define-external (vector-append . vs) QobiScheme)
(define-external (list-set! l i x) QobiScheme)
(define-external (list-insert l i x) QobiScheme)
(define-external (list-remove l i) QobiScheme)
(define-external (list-replace l i x) QobiScheme)
(define-external (but-last x) QobiScheme)
(define-external (sublist list start end) QobiScheme)
(define-external (subvector vector start end) QobiScheme)
(define-external (reduce f l i) QobiScheme)
(define-external (reduce-n f n i) QobiScheme)
(define-external (reduce-vector f v i) QobiScheme)
(define-external (map-reduce g i f l . ls) QobiScheme)
(eval-when (compile)
 (define (map-reduce g i f l . ls)
  (if (null? l)
      i
      (apply map-reduce
	     g
	     (g i (apply f (car l) (map car ls)))
	     f
	     (cdr l)
	     (map cdr ls)))))
(define-external (map-reduce-n g i f n) QobiScheme)
(define-external (map-reduce-vector g i f v . vs) QobiScheme)
(define-external (sum f n) QobiScheme)
(define-external (product f n) QobiScheme)
(define-external (factorial n) QobiScheme)
(define-external (choose n m) QobiScheme)
(define-external (some p l . &rest) QobiScheme)
(define-external (some-n p n) QobiScheme)
(define-external (some-vector p v . &rest) QobiScheme)
(define-external (every p l . &rest) QobiScheme)
(eval-when (compile)
 (define (every p l . &rest)
  (let loop ((l l) (&rest &rest))
   (or (null? l)
       (and (apply p (first l) (map first &rest))
	    (loop (rest l) (map rest &rest)))))))
(define-external (every-n p n) QobiScheme)
(define-external (every-vector p v . &rest) QobiScheme)
(define-external (one p l . &rest) QobiScheme)
(define-external (one-n p n) QobiScheme)
(define-external (one-vector p v . &rest) QobiScheme)
(define-external (for-each-indexed f l) QobiScheme)
(define-external (for-each-n f n) QobiScheme)
(define-external (for-each-from-a-up-to-b f a b) QobiScheme)
(define-external (for-each-n-decreasing f n) QobiScheme)
(define-external (for-each-vector f v . &rest) QobiScheme)
(define-external (map-indexed f l) QobiScheme)
(eval-when (compile)
 (define (map-indexed f l)
  ;; needs work: To eliminate REVERSE.
  (let loop ((i 0) (l l) (c '()))
   (if (null? l)
       (reverse c)
       (loop (+ i 1) (rest l) (cons (f (first l) i) c))))))
(define-external (map-n f n) QobiScheme)
(define-external (map-vector f v . &rest) QobiScheme)
(define-external (map-n-vector f n) QobiScheme)
(define-external (enumerate n) QobiScheme)
(define-external (enumerate-vector n) QobiScheme)
(define-external (memp p x l) QobiScheme)
(define-external (assp p x alist) QobiScheme)
(define-external (pairwise? p l) QobiScheme)
(define-external (adjoinq x l) QobiScheme)
(define-external (adjoinv x l) QobiScheme)
(define-external (adjoin x l) QobiScheme)
(define-external (adjoinp p x l) QobiScheme)
(define-external (removeq x l) QobiScheme)
(define-external (removev x l) QobiScheme)
(define-external (removep p x l) QobiScheme)
(define-external (remove-if p l) QobiScheme)
(define-external (remove-if-not p l) QobiScheme)
(define-external (positionq x l) QobiScheme)
(define-external (positionv x l) QobiScheme)
(define-external (position x l) QobiScheme)
(define-external (positionp p x l) QobiScheme)
(define-external (position-if p l) QobiScheme)
(define-external (position-if-not p l) QobiScheme)
(define-external (findq x l) QobiScheme)
(define-external (findv x l) QobiScheme)
(define-external (find x l) QobiScheme)
(define-external (findp p x l) QobiScheme)
(define-external (find-if p l) QobiScheme)
(define-external (find-if-not p l) QobiScheme)
(define-external (countq x l) QobiScheme)
(define-external (countv x l) QobiScheme)
(define-external (count x l) QobiScheme)
(define-external (countp p x l) QobiScheme)
(define-external (count-if p l) QobiScheme)
(define-external (count-if-not p l) QobiScheme)
(define-external (subsetq? x y) QobiScheme)
(define-external (subsetv? x y) QobiScheme)
(define-external (subset? x y) QobiScheme)
(define-external (subsetp? p x y) QobiScheme)
(define-external (set-equalq? x y) QobiScheme)
(define-external (set-equalv? x y) QobiScheme)
(define-external (set-equal? x y) QobiScheme)
(define-external (set-equalp? p x y) QobiScheme)
(define-external (unionq x y) QobiScheme)
(define-external (unionv x y) QobiScheme)
(define-external (union x y) QobiScheme)
(define-external (unionp p x y) QobiScheme)
(define-external (intersectionq x y) QobiScheme)
(define-external (intersectionv x y) QobiScheme)
(define-external (intersection x y) QobiScheme)
(define-external (intersectionp p x y) QobiScheme)
(define-external (set-differenceq x y) QobiScheme)
(define-external (set-differencev x y) QobiScheme)
(define-external (set-difference x y) QobiScheme)
(define-external (set-differencep p x y) QobiScheme)
(define-external (remove-duplicatesq x) QobiScheme)
(define-external (remove-duplicatesv x) QobiScheme)
(define-external (remove-duplicates x) QobiScheme)
(define-external (remove-duplicatesp p x) QobiScheme)
(define-external (equivalence-classesq x) QobiScheme)
(define-external (equivalence-classesv x) QobiScheme)
(define-external (equivalence-classes x) QobiScheme)
(define-external (transitive-equivalence-classesp p x) QobiScheme)
(define-external (equivalence-classesp p x) QobiScheme)
(define-external (topological-sort p l) QobiScheme)
(define-external (every-other list) QobiScheme)
(define-external (merge list1 list2 predicate key) QobiScheme)
(define-external (sort list predicate key) QobiScheme)
(define-external (minp p l) QobiScheme)
(define-external (unionvt x y) QobiScheme)
(define-external (intersectionvt x y) QobiScheme)
(define-external (set-differencevt x y) QobiScheme)
(define-external (subsetvt? x y) QobiScheme)
(define-external (lexicographically<? <? =?) QobiScheme)
(define-external (minimal-membersp <? =? l) QobiScheme)

;;; Sleep

(define-c-external (c-usleep int) int "usleep")
(define-external (usleep n) QobiScheme)

;;; Random

(define-c-external (c-rand) int "rand")
(define-external (rand) QobiScheme)
(define-external *rand-max* QobiScheme)
(define-external (random-real) QobiScheme)
(define-external (random-integer n) QobiScheme)
(define-external (random-boolean) QobiScheme)
(define-external (random-member l) QobiScheme)
(define-external (n-random-elements-without-replacement n x) QobiScheme)
(define-external (deal list) QobiScheme)
(define-external (random-partition-of-size k x) QobiScheme)

;;; Gamma Function

(define-external (gamma n) QobiScheme)
(define-external (log-gamma n) QobiScheme)

;;; Numerical Integration

(define-external (integrate f a b n) QobiScheme)

;;; Schemer

(define-external *fail?* QobiScheme)

(define-macro either
 (lambda (form expander)
  (expander
   (cond
    ((null? (rest form)) '(fail))
    ((null? (rest (rest form))) (second form))
    (else `(if (a-boolean) ,(second form) (either ,@(rest (rest form))))))
   expander)))

(define-external fail QobiScheme)
(define-external (set-fail! procedure) QobiScheme)

(define-macro for-effects
 (lambda (form expander)
  (let ((return (string->uninterned-symbol "return"))
	(old-fail (string->uninterned-symbol "old-fail")))
   (expander `(call-with-current-continuation
	       (lambda (,return)
		(let ((,old-fail fail))
		 (set-fail! (lambda () (set-fail! ,old-fail) (,return #f)))
		 (begin ,@(rest form))
		 (fail))))
	     expander))))

(define-macro one-value
 (lambda (form expander)
  (unless (or (= (length form) 2) (= (length form) 3))
   (error 'one-value "Improper ONE-VALUE: ~s" form))
  (let ((form1 (second form))
	(form2 (if (= (length form) 2) '(fail) (third form)))
	(return (string->uninterned-symbol "return"))
	(old-fail (string->uninterned-symbol "old-fail")))
   (expander `(call-with-current-continuation
	       (lambda (,return)
		(let ((,old-fail fail))
		 (set-fail! (lambda () (set-fail! ,old-fail) (,return ,form2)))
		 (let ((v ,form1))
		  (set-fail! ,old-fail)
		  v))))
	     expander))))

(define-macro local-one-value
 ;; needs work: *FAIL?* can potentially be captured.
 (lambda (form expander)
  (unless (or (= (length form) 2) (= (length form) 3))
   (error 'local-one-value "Improper LOCAL-ONE-VALUE: ~s" form))
  (let ((form1 (second form))
	(form2 (if (= (length form) 2) '(fail) (third form)))
	(return (string->uninterned-symbol "return"))
	(old-fail (string->uninterned-symbol "old-fail"))
	(v (string->uninterned-symbol "v")))
   (expander
    `(call-with-current-continuation
      (lambda (,return)
       (let ((,v #f)
	     (,old-fail fail))
	(set-fail!
	 (lambda ()
	  (set-fail! ,old-fail)
	  (,return (cond (*fail?* ,form2) (else (set! *fail?* #t) ,v)))))
	(set! ,v ,form1)
	(set! *fail?* #f)
	(fail))))
    expander))))

(define-macro all-values
 ;; needs work: To eliminate REVERSE.
 (lambda (form expander)
  (let ((values (string->uninterned-symbol "values")))
   (expander
    `(let ((,values '()))
      (for-effects (set! ,values (cons (begin ,@(rest form)) ,values)))
      (reverse ,values))
    expander))))

(define-macro possibly?
 (lambda (form expander)
  (let ((return (string->uninterned-symbol "return"))
	(old-fail (string->uninterned-symbol "old-fail"))
	(v (string->uninterned-symbol "v")))
   (expander
    `(call-with-current-continuation
      (lambda (,return)
       (let ((,old-fail fail))
	(set-fail! (lambda () (set-fail! ,old-fail) (,return #f)))
	(let ((,v (begin ,@(rest form))))
	 (unless ,v (fail))
	 (set-fail! ,old-fail)
	 ,v))))
    expander))))

(define-macro necessarily?
 (lambda (form expander)
  (let ((return (string->uninterned-symbol "return"))
	(old-fail (string->uninterned-symbol "old-fail"))
	(v (string->uninterned-symbol "v"))
	(u (string->uninterned-symbol "u")))
   (expander
    `(call-with-current-continuation
      (lambda (,return)
       (let ((,old-fail fail)
	     (,u #t))
	(set-fail! (lambda () (set-fail! ,old-fail) (,return ,u)))
	(let ((,v (begin ,@(rest form))))
	 (when ,v (set! ,u ,v) (fail))
	 (set-fail! ,old-fail)
	 #f))))
    expander))))

(define-macro upon-failure
 (lambda (form expander)
  (let ((old-fail (string->uninterned-symbol "old-fail")))
   (expander
    `(let ((,old-fail fail))
      (set-fail! (lambda () (set-fail! ,old-fail) ,@(rest form) (fail))))
    expander))))

(define-external (unwind-trail) QobiScheme)
(define-external (unwedge-trail) QobiScheme)

(define-macro local-set!
 (lambda (form expander)
  (unless (= (length form) 3)
   (error 'local-set! "Improper LOCAL-SET!: ~s" form))
  (let ((p (string->uninterned-symbol "p")))
   (expander `(begin
	       (let ((,p ,(second form)))
		(upon-failure (set! ,(second form) ,p)))
	       (set! ,(second form) ,(third form)))
	     expander))))

(define-external (local-set-car! x y) QobiScheme)
(define-external (local-set-cdr! x y) QobiScheme)
(define-external (local-string-set! s i x) QobiScheme)
(define-external (local-vector-set! v i x) QobiScheme)
(define-external (a-boolean) QobiScheme)
(define-external (an-integer) QobiScheme)
(define-external (an-integer-above i) QobiScheme)
(define-external (an-integer-below i) QobiScheme)
(define-external (an-integer-between i j) QobiScheme)
(define-external (a-member-of s) QobiScheme)
(define-external (a-subset-of l) QobiScheme)
(define-external (a-split-of l) QobiScheme)
(define-external (a-permutation-of l) QobiScheme)
(define-external (a-partition-of x) QobiScheme)
(define-external (a-partition-of-size k x) QobiScheme)
(define-structure-external QobiScheme logic-variable binding name noticers)
(define-external *logic-variable-counter* QobiScheme)
(define-external (create-logic-variable) QobiScheme)
(define-external (attach-noticer! x noticer) QobiScheme)
(define-external (value-of x) QobiScheme)
(define-external (ground? x) QobiScheme)
(define-external (known?-equalv x y) QobiScheme)
(define-external (assert!-equalv x y) QobiScheme)
(define-external (assert!-notv-equalv x y) QobiScheme)

;;; Memoization

(define-structure-external QobiScheme entry arguments continuations results)
(define-external (memoize f) QobiScheme)

;;; Strings

(define-external (prefix? prefix string) QobiScheme)
(define-external (string-reverse string) QobiScheme)
(define-external (suffix? suffix string) QobiScheme)
(define-external (directory-prefix? prefix string) QobiScheme)
(define-external (string-downcase string) QobiScheme)
(define-external (string-upcase string) QobiScheme)
(define-external (symbol-downcase symbol) QobiScheme)
(define-external (pad-left string n) QobiScheme)
(define-external (pad-right string n) QobiScheme)
(define-external (substring? s1 s2) QobiScheme)
(define-external (substring-ci? s1 s2) QobiScheme)
(define-external (slashify string) QobiScheme)
(define-external (string-insert-character character) QobiScheme)
(define-external (string-beginning-of-line string position) QobiScheme)
(define-external (string-backward-char string position) QobiScheme)
(define-external (string-delete-char string position) QobiScheme)
(define-external (string-end-of-line string position) QobiScheme)
(define-external (string-forward-char string position) QobiScheme)
(define-external (string-kill-line string position) QobiScheme)
(define-external (string-backward-delete-char string position) QobiScheme)
(define-external (char-alphanumeric? char) QobiScheme)
(define-external (beginning-of-word? string position) QobiScheme)
(define-external (end-of-word? string position) QobiScheme)
(define-external (string-backward-word string position) QobiScheme)
(define-external (string-kill-word string position) QobiScheme)
(define-external (string-forward-word string position) QobiScheme)
(define-external (string-backward-kill-word string position) QobiScheme)

;;; Fields

(define-external (number-of-fields string) QobiScheme)
(define-external (field-ref string n) QobiScheme)
(define-external (fields string) QobiScheme)

;;; Context-Free Grammars

(define-macro lazy
 (lambda (form expander)
  (let ((args (string->uninterned-symbol "args")))
   (expander `(lambda ,args (apply ,(second form) ,args)) expander))))

(define-external (terminal x) QobiScheme)
(define-external (seq . &rest) QobiScheme)
(define-external (alt . &rest) QobiScheme)
(define-external (opt a) QobiScheme)
(define-external (k* a) QobiScheme)
(define-external (recognize? s words) QobiScheme)

;;; Line and Whole-File I/O

(define-external (read-line . port) QobiScheme)
(define-external (read-file pathname) QobiScheme)
(define-external (write-file lines pathname) QobiScheme)
(define-external (read-object-from-file pathname) QobiScheme)
(define-external (write-object-to-file object pathname) QobiScheme)
(define-external (read-from-string string) QobiScheme)

;;; Pathnames

(define-external (has-directory? pathname) QobiScheme)
(define-external (directory pathname) QobiScheme)
(define-external (strip-directory pathname) QobiScheme)
(define-external (has-extension? pathname) QobiScheme)
(define-external (extension pathname) QobiScheme)
(define-external (strip-extension pathname) QobiScheme)
(define-external (default-extension pathname extension) QobiScheme)
(define-external (replace-extension pathname extension) QobiScheme)

;;; Temporary files

(define-external *tmp* QobiScheme)
(define-external (tmp pathname) QobiScheme)

;;; Directory/File operations

(define-external (can-open-file-for-input? pathname) QobiScheme)
(define-external *system-V?* QobiScheme)
(define-external (quotify string) QobiScheme)
(define-external (file-exists? pathname) QobiScheme)
(define-external (directory-list pattern) QobiScheme)
(define-external (recursive-directory-list pathname) QobiScheme)
(define-external (file-info pathname id?) QobiScheme)
(define-external (file-permission-flags pathname) QobiScheme)
(define-external (directory? pathname) QobiScheme)
(define-external (symlink? pathname) QobiScheme)
(define-external (file? pathname) QobiScheme)
(define-external (file-number-of-links pathname) QobiScheme)
(define-external (file-userid pathname) QobiScheme)
(define-external (file-uid pathname) QobiScheme)
(define-external (file-groupid pathname) QobiScheme)
(define-external (file-gid pathname) QobiScheme)
(define-external (file-length pathname) QobiScheme)
(define-external (file-mtime-month pathname) QobiScheme)
(define-external (file-mtime-date pathname) QobiScheme)
(define-external (file-mtime-time/year pathname) QobiScheme)
(define-external (symlink-target pathname) QobiScheme)
(define-external (symlink target pathname) QobiScheme)
(define-external (mkdir pathname) QobiScheme)
(define-external (rm-if-necessary pathname) QobiScheme)
(define-external (rm pathname) QobiScheme)
(define-external (mkfifo pathname) QobiScheme)
(define-external (create-directory-and-parents-if-necessary target) QobiScheme)
(define-external (same-contents? pathname1 pathname2) QobiScheme)
(define-external (compressed? pathname) QobiScheme)
(define-external (compressed pathname) QobiScheme)
(define-external (compress pathname) QobiScheme)
(define-external (uncompress pathname) QobiScheme)
(define-external (ld file) QobiScheme)

;;; Tries

(define-structure-external
 QobiScheme trie n char->integer integer->char initial-value trie-node)
(define-structure-external QobiScheme trie-node table value)
(define-external (create-trie n char->integer integer->char . initial-value)
 QobiScheme)
(define-external (trie-ref trie string) QobiScheme)
(define-external (trie-set! trie string value) QobiScheme)
(define-external (for-each-trie-entry p trie) QobiScheme)
(define-external (trie->alist trie) QobiScheme)
(define-external
 (alist->trie alist n char->integer integer->char . initial-value)
 QobiScheme)

;;; Vectors

(define-structure-external QobiScheme line-segment p q)
(define-external (p l) QobiScheme)
(define-external (q l) QobiScheme)
(define-external (x v) QobiScheme)
(define-external (y v) QobiScheme)
(define-external (z v) QobiScheme)
(define-external (dot u v) QobiScheme)
(define-external (cross-2d u v) QobiScheme)
(define-external (cross u v) QobiScheme)
(define-external (v+ u v) QobiScheme)
(define-external (v- u v) QobiScheme)
(define-external (k*v k u) QobiScheme)
(define-external (v= u v) QobiScheme)
(define-external (rotate-90 u) QobiScheme)
(define-external (rotate-180 u) QobiScheme)
(define-external (rotate-270 u) QobiScheme)
(define-external (perpendicular? u v) QobiScheme)
(define-external (parallel? u v) QobiScheme)
(define-external (magnitude-squared v) QobiScheme)
(define-external (magnitude v) QobiScheme)
(define-external (unit v) QobiScheme)
(define-external (distance-squared u v) QobiScheme)
(define-external (distance u v) QobiScheme)
(define-external (line-tangent l) QobiScheme)
(define-external (normal-2d l) QobiScheme)
(define-external (line-segment-length l) QobiScheme)
(define-external (collinear? l1 l2) QobiScheme)
(define-external (point-on-line-segment? r l) QobiScheme)
(define-external (intersection-point l1 l2) QobiScheme)
(define-external (cross? l1 l2) QobiScheme)
(define-external (intersect? l1 l2) QobiScheme)
(define-external (read-line-segments-from-file pathname) QobiScheme)
(define-external (write-line-segments-to-file line-segments pathname)
 QobiScheme)

;;; Matricies

(define-external (make-matrix m n . &rest) QobiScheme)
(define-external (make-3-by-3-matrix a11 a12 a13 a21 a22 a23 a31 a32 a33)
 QobiScheme)
(define-external (matrix-copy m) QobiScheme)
(define-external (matrix-rows a) QobiScheme)
(define-external (matrix-columns a) QobiScheme)
(define-external (matrix-ref a i j) QobiScheme)
(define-external (matrix-set! a i j x) QobiScheme)
(define-external (matrix-row-ref a i) QobiScheme)
(define-external (matrix-column-ref a j) QobiScheme)
(define-external (matrix-row-set! a i v) QobiScheme)
(define-external (vector->row-matrix v) QobiScheme)
(define-external (vector->column-matrix v) QobiScheme)
(define-external (m+ a b) QobiScheme)
(define-external (m- a b) QobiScheme)
(define-external (m*v a v) QobiScheme)
(define-external (transpose a) QobiScheme)
(define-external (outer-product f u v) QobiScheme)
(define-external (self-outer-product f v) QobiScheme)
(define-external (m* a b) QobiScheme)
(define-external (v*m v a) QobiScheme)
(define-external (k*m k m) QobiScheme)
(define-external (determinant m) QobiScheme)
(define-external (invert-matrix a) QobiScheme)
(define-external *epsilon* QobiScheme)
(define-external (simplex a m1 m2 m3) QobiScheme)
(define-external (quadratic1 a b c) QobiScheme)
(define-external (quadratic2 a b c) QobiScheme)
(define-external (jacobi a) QobiScheme)
(define-external (eigenvalues a) QobiScheme)
(define-external (eigenvectors a) QobiScheme)
(define-external (vector->diagonal-matrix v) QobiScheme)
(define-external (identity-matrix n) QobiScheme)
(define-external (clip-eigenvalues a v) QobiScheme)
(define-external (eigenvector-angle1 m) QobiScheme)
(define-external (eigenvector-angle2 m) QobiScheme)

;;; Sparse Matrices

(define-structure-external QobiScheme sparse-matrix row column blank)
(define-structure-external QobiScheme sparse-matrix-row element i up down)
(define-structure-external
 QobiScheme sparse-matrix-column element j left right)
(define-structure-external
 QobiScheme sparse-matrix-element value i up down j left right)
(define-external (create-sparse-matrix blank) QobiScheme)
(define-external (sparse-matrix-ref sparse-matrix i j) QobiScheme)

;;; Arrays

(define-external (make-array l . &rest) QobiScheme)
(define-external (array-ref a . &rest) QobiScheme)
(define-external (array-set! a v . &rest) QobiScheme)

;;; 3D Geometry

(define-structure-external QobiScheme transform translation rotation)
(define-external pi QobiScheme)
(define-external half-pi QobiScheme)
(define-external two-pi QobiScheme)
(define-external minus-pi QobiScheme)
(define-external two-pi/360 QobiScheme)
(define-external three-sixty/two-pi QobiScheme)
(define-external (degrees->radians angle) QobiScheme)
(define-external (radians->degrees angle) QobiScheme)
(define-external (normalize-rotation rotation) QobiScheme)
(define-external (rotation+ x y) QobiScheme)
(define-external (rotation- x y) QobiScheme)
(define-external (angle-separation x y) QobiScheme)
(define-external (rotation-matrix-2d theta) QobiScheme)
(define-external (mean-angle angles) QobiScheme)
(define-external (create-transform theta phi psi x y z) QobiScheme)
(define-external (compose-transforms t1 t2) QobiScheme)
(define-external (apply-transform t v) QobiScheme)
(define-external (project v focal-length) QobiScheme)

;;; Ellipses

(define-structure-external QobiScheme ellipse x0 y0 t0 a b)
(define-external (ellipse-center ellipse) QobiScheme)
(define-external (ellipse-area ellipse) QobiScheme)
(define-external (ellipse-eccentricity ellipse) QobiScheme)
(define-external (radial-distance theta phi) QobiScheme)
(define-external (point-on-ellipse? p ellipse tolerance) QobiScheme)
(define-external (draw-ellipse display drawable gc ellipse) QobiScheme)

;;; Convex Hull

(define-external (same-angle? u v) QobiScheme)
(define-external (clockwise-angle? u v w) QobiScheme)
(define-external (clockwise-or-same-angle? u v w) QobiScheme)
(define-external (convex-hull points) QobiScheme)
(define-external (concave-hull points delta) QobiScheme)
(define-external (clockwise? p q r) QobiScheme)
(define-external (crossing? points) QobiScheme)
(define-external (triangulate points) QobiScheme)
(define-external (perimeter-of-polygon points) QobiScheme)
(define-external (hero p q r) QobiScheme)
(define-external (area-of-polygon points) QobiScheme)
(define-external (point-inside-triangle? p u v w) QobiScheme)
(define-external (point-inside-or-on-triangle? p u v w) QobiScheme)
(define-external (outline-polygon points) QobiScheme)
(define-external (fill-polygon points u v) QobiScheme)

;;; Log Space Addition

(define-external log-math-precision QobiScheme)
(define-external minus-infinity QobiScheme)
(define-external infinity QobiScheme)
(define-external nan QobiScheme)
(define-external (add-exp e1 e2) QobiScheme)
(define-external (log-sum f n) QobiScheme)

;;; Images

(define-external *max-red* QobiScheme)
(define-external *max-green* QobiScheme)
(define-external *max-blue* QobiScheme)
(define-external *max-grey* QobiScheme)
(define-external *max-hue* QobiScheme)
(define-external *max-saturation* QobiScheme)
(define-external *max-value* QobiScheme)
(define-external (rgb->hsv rgb) QobiScheme)
(define-external (hsv->rgb hsv) QobiScheme)
(define-external (rgb->cd rgb) QobiScheme)
(define-structure-external QobiScheme pbm raw? bitmap)
(define-structure-external QobiScheme pgm raw? maxval grey)
(define-structure-external QobiScheme ppm raw? maxval red green blue)
(define-external (pnm-width pnm) QobiScheme)
(define-external (pnm-height pnm) QobiScheme)
(define-external (read-pnm pathname) QobiScheme)
(define-external (write-pnm pnm pathname) QobiScheme)
(define-external (pnm-movie-frame-pathname pathname i) QobiScheme)
(define-external (pnm-movie-length pathname) QobiScheme)
(define-external (read-pnm-movie pathname) QobiScheme)
(define-external (write-pnm-movie pnm-movie pathname) QobiScheme)
(define-external (read-mpeg pathname) QobiScheme)
(define-external (write-mpeg pnm-movie pathname) QobiScheme)
(define-external
 (clip-mpeg input-pathname output-pathname first-frame last-frame)
 QobiScheme)
(define-external (ppm-hue ppm) QobiScheme)
(define-external (ppm-saturation ppm) QobiScheme)
(define-external (ppm-value ppm) QobiScheme)
(define-external (pbm-and pbm1 pbm2) QobiScheme)
(define-external (pbm-or pbm1 pbm2) QobiScheme)
(define-external (pbm-not pbm) QobiScheme)
(define-external (pbm-xor pbm1 pbm2) QobiScheme)
(define-external (pgm-absolute-difference pgm1 pgm2) QobiScheme)
(define-external (empty-pnm? pnm) QobiScheme)
(define-external (pbm->pgm pbm) QobiScheme)
(define-external (pgm->ppm pgm) QobiScheme)
(define-external (pbm->ppm pbm) QobiScheme)
(define-external (ppm->pgm ppm) QobiScheme)
(define-external (pgm->pbm pgm threshold) QobiScheme)
(define-external (ppm->pbm ppm threshold) QobiScheme)
(define-external (pbm-constant width height bit) QobiScheme)
(define-external (pgm-constant width height grey) QobiScheme)
(define-external (ppm-constant width height red green blue) QobiScheme)
(define-external (pbm-left-vertical-stripe width height left) QobiScheme)
(define-external (overlay-pbm-on-pnm pbm pnm) QobiScheme)
(define-external (pgm-smooth pgm sigma) QobiScheme)
(define-external (normal-flow-magnitude pgm1 pgm2 epsilon sigma sensitivity)
 QobiScheme)
(define-external
 (threshold-normal-flow-magnitude pgm1 pgm2 epsilon sigma threshold)
 QobiScheme)
(define-external (pbm-proximity-clusterer pbm threshold) QobiScheme)
(define-external (pbm-bloat pbm n) QobiScheme)
(define-external (pnm-shift pnm delta) QobiScheme)
(define-external (pnm-copy pnm) QobiScheme)
(define-external (pnm-black-window pnm upper-left lower-right) QobiScheme)
(define-external (pnm-white-window pnm upper-left lower-right) QobiScheme)
(define-external (points->pbm-of-size points height width) QobiScheme)
(define-external (pbm-ppm-and pbm ppm) QobiScheme)
(define-external (pnm-rotate pnm) QobiScheme)
(define-external (pnm-flip pnm) QobiScheme)

;;; An API to libjpeg.a and the UCB mpeg_encode and mpeg_play programs.

(define-external (video-type-enum video-type) QobiScheme)
(define-external (open-video-input-file pathname video-type) QobiScheme)
(define-external (get-video-width video-input-port) QobiScheme)
(define-external (get-video-height video-input-port) QobiScheme)
(define-external (read-ppm-from-video-input-port video-input-port) QobiScheme)
(define-external (read-pixmap-from-video-input-port video-input-port)
 QobiScheme)
(define-external (close-video-input-port video-input-port) QobiScheme)
(define-external (call-with-video-input-file pathname video-type procedure)
 QobiScheme)
(define-external (video-file-length pathname video-type) QobiScheme)
(define-external (video-file-frame->ppm pathname video-type frame) QobiScheme)
(define-external (video-file-frame->pixmap pathname video-type frame)
 QobiScheme)
(define-external (video-file->pnm-movie pathname video-type) QobiScheme)
(define-external (video-file->pixmaps pathname video-type) QobiScheme)
(define-external (open-video-output-file pathname video-type frames)
 QobiScheme)
(define-external (write-pnm-to-video-output-port pnm video-output-port)
 QobiScheme)
(define-external (close-video-output-port video-output-port) QobiScheme)
(define-external
 (call-with-video-output-file pathname video-type frames procedure)
 QobiScheme)
(define-external (pnm-movie->video-file pnm-movie pathname video-type)
 QobiScheme)
(define-external (map-adjacent-video-file input-pathname
				          input-video-type
				          output-pathname
				          output-video-type
				          processor)
 QobiScheme)
(define-external (map-adjacent-video-file-to-object-file input-pathname
						         input-video-type
						         output-pathname
						         processor)
 QobiScheme)

;;; Multivariate Nonlinear Optimization

(define-external *itmax-powell* QobiScheme)
(define-external *itmax-brent* QobiScheme)
(define-external *tol* QobiScheme)
(define-external *glimit* QobiScheme)
(define-external *tiny* QobiScheme)
(define-external *zeps* QobiScheme)
(define-external (brent ax bx cx f tol k) QobiScheme)
(define-external (mnbrak ax bx func k) QobiScheme)
(define-external (linmin p xi func) QobiScheme)
(define-external (powell p xi ftol func) QobiScheme)

;;; EM Clusterer

(define-structure-external QobiScheme model
 pi mu sigma log-pi sigma-inverse log-determinant-sigma)
(define-external (log-likelihood x model) QobiScheme)
(define-external (e-step x models) QobiScheme)
(define-external (m-step x z clip) QobiScheme)
(define-external
 (em x pi mu sigma clip em-kick-off-tolerance em-convergence-tolerance)
 QobiScheme)
(define-external (noise epsilon) QobiScheme)
(define-external (initial-z ii jj) QobiScheme)
(define-external (ems x clip em-kick-off-tolerance em-convergence-tolerance
		      ems-convergence-tolerance)
 QobiScheme)
(define-external (em-jj-clusterer
		  x jj clip em-kick-off-tolerance em-convergence-tolerance)
 QobiScheme)
(define-external
 (em-clusterer x clip em-kick-off-tolerance em-convergence-tolerance
	       ems-convergence-tolerance)
 QobiScheme)

;;; CPU time procedures courtesy of James Rootham

(define-c-external (clock) int "clock")
(define-external *clock-time* QobiScheme)
(define-external *clock-in-time-out* QobiScheme)
(define-external *start-time-out* QobiScheme)
(define-external *time-out-time* QobiScheme)
(define-external fix QobiScheme)
(define-external bad QobiScheme)
(define-external *clocks-per-second* QobiScheme)
(define-external (read-clock) QobiScheme)
(define-external (clock-reset) QobiScheme)
(define-external (clock-sample) QobiScheme)
(define-external (clock-time-out) QobiScheme)
(define-external (clock-time-in) QobiScheme)

;;; An API to C arrays courtesy of Richard Mann

(define-external (make-byte-array n . initial-value) QobiScheme)
(define-external (byte-array? x) QobiScheme)
(define-external (byte-array-ref array index) QobiScheme)
(define-external (byte-array-set! array index value) QobiScheme)
(define-external (byte-array-length array) QobiScheme)
(define-external (list->byte-array values) QobiScheme)
(define-external (byte-array->list array) QobiScheme)
(define-external (make-shortunsigned-array n . initial-value) QobiScheme)
(define-external (shortunsigned-array? x) QobiScheme)
(define-external (shortunsigned-array-ref array index) QobiScheme)
(define-external (shortunsigned-array-set! array index value) QobiScheme)
(define-external (shortunsigned-array-length array) QobiScheme)
(define-external (list->shortunsigned-array values) QobiScheme)
(define-external (shortunsigned-array->list array) QobiScheme)
(define-external (make-int-array n . initial-value) QobiScheme)
(define-external (int-array? x) QobiScheme)
(define-external (int-array-ref array index) QobiScheme)
(define-external (int-array-set! array index value) QobiScheme)
(define-external (int-array-length array) QobiScheme)
(define-external (list->int-array values) QobiScheme)
(define-external (int-array->list array) QobiScheme)
(define-external (make-longunsigned-array n . initial-value) QobiScheme)
(define-external (longunsigned-array? x) QobiScheme)
(define-external (longunsigned-array-ref array index) QobiScheme)
(define-external (longunsigned-array-set! array index value) QobiScheme)
(define-external (longunsigned-array-length array) QobiScheme)
(define-external (list->longunsigned-array values) QobiScheme)
(define-external (longunsigned-array->list array) QobiScheme)
(define-external (make-float-array n . initial-value) QobiScheme)
(define-external (float-array? x) QobiScheme)
(define-external (float-array-ref array index) QobiScheme)
(define-external (float-array-set! array index value) QobiScheme)
(define-external (float-array-length array) QobiScheme)
(define-external (list->float-array values) QobiScheme)
(define-external (float-array->list array) QobiScheme)
(define-external (make-double-array n . initial-value) QobiScheme)
(define-external (double-array? x) QobiScheme)
(define-external (double-array-ref array index) QobiScheme)
(define-external (double-array-set! array index value) QobiScheme)
(define-external (double-array-length array) QobiScheme)
(define-external (list->double-array values) QobiScheme)
(define-external (double-array->list array) QobiScheme)

;;; Sclim

(define-external return QobiScheme)
(define-external escape QobiScheme)
(define-external delete QobiScheme)
(define-external *display* QobiScheme)
(define-external *screen* QobiScheme)
(define-external *root-window* QobiScheme)
(define-external *button-width* QobiScheme)
(define-external *button-height* QobiScheme)
(define-external *background-color* QobiScheme)
(define-external *foreground-color* QobiScheme)
(define-external *background* QobiScheme)
(define-external *foreground* QobiScheme)
(define-external *white-pixel* QobiScheme)
(define-external *black-pixel* QobiScheme)
(define-external *roman-font* QobiScheme)
(define-external *bold-font* QobiScheme)
(define-external *roman-height* QobiScheme)
(define-external *bold-height* QobiScheme)
(define-external *text-height* QobiScheme)
(define-external *roman-baseline* QobiScheme)
(define-external *bold-baseline* QobiScheme)
(define-external *text-baseline* QobiScheme)
(define-external *display-pane-width* QobiScheme)
(define-external *display-pane-height* QobiScheme)
(define-external *transcript-pane-height* QobiScheme)
(define-external *echo-pane-height* QobiScheme)
(define-external *who-line-height* QobiScheme)
(define-external *status-pane-width* QobiScheme)
(define-external *window* QobiScheme)
(define-external *buttons* QobiScheme)
(define-external *regions* QobiScheme)
(define-external *display-pane* QobiScheme)
(define-external *transcript-pane* QobiScheme)
(define-external *echo-pane* QobiScheme)
(define-external *status-pane* QobiScheme)
(define-external *message-pane* QobiScheme)
(define-external *thin-gc* QobiScheme)
(define-external *thin-flipping-gc* QobiScheme)
(define-external *medium-gc* QobiScheme)
(define-external *medium-flipping-gc* QobiScheme)
(define-external *thick-gc* QobiScheme)
(define-external *thick-flipping-gc* QobiScheme)
(define-external *dashed-gc* QobiScheme)
(define-external *dashed-flipping-gc* QobiScheme)
(define-external *roman-gc* QobiScheme)
(define-external *bold-gc* QobiScheme)
(define-external *bold-flipping-gc* QobiScheme)
(define-external *light-gray* QobiScheme)
(define-external *light-gray-gc* QobiScheme)
(define-external *gray* QobiScheme)
(define-external *gray-gc* QobiScheme)
(define-external *red* QobiScheme)
(define-external *red-gc* QobiScheme)
(define-external *dark-red* QobiScheme)
(define-external *dark-red-gc* QobiScheme)
(define-external *green* QobiScheme)
(define-external *green-gc* QobiScheme)
(define-external *dark-green* QobiScheme)
(define-external *dark-green-gc* QobiScheme)
(define-external *blue* QobiScheme)
(define-external *blue-gc* QobiScheme)
(define-external *yellow* QobiScheme)
(define-external *yellow-gc* QobiScheme)
(define-external *violet* QobiScheme)
(define-external *violet-gc* QobiScheme)
(define-external *orange* QobiScheme)
(define-external *orange-gc* QobiScheme)
(define-external *dark-orange* QobiScheme)
(define-external *dark-orange-gc* QobiScheme)
(define-external *color-gc* QobiScheme)
(define-external *window-methods* QobiScheme)
(define-external *transcript* QobiScheme)
(define-external *input* QobiScheme)
(define-external *input-position* QobiScheme)
(define-external *abort-button* QobiScheme)
(define-external *abort-key* QobiScheme)
(define-external *comtab* QobiScheme)
(define-external *help-comtab* QobiScheme)
(define-external *prefix* QobiScheme)
(define-external *status* QobiScheme)
(define-external *message* QobiScheme)
(define-external *pause?* QobiScheme)
(define-external *redraw-procedure* QobiScheme)
(define-external *quit-continuation* QobiScheme)
(define-external *abort-continuation* QobiScheme)
(define-external *color-cube* QobiScheme)
(define-external *reds* QobiScheme)
(define-external *greens* QobiScheme)
(define-external *blues* QobiScheme)
(define-external *dither?* QobiScheme)
(define-external *help?* QobiScheme)
(define-external *help* QobiScheme)
(define-external *first-help-line* QobiScheme)
(define-external *clear-display-pane?* QobiScheme)
(define-external *display-name* QobiScheme)
(define-external *roman-font-name* QobiScheme)
(define-external *bold-font-name* QobiScheme)
(define-external *window-position?* QobiScheme)
(define-external *window-position-x* QobiScheme)
(define-external *window-position-y* QobiScheme)
(define-external *post-initialize-procedure* QobiScheme)
(define-external *enable-background-task* QobiScheme)
(define-external *disable-background-task* QobiScheme)
(define-external (character->pretty-name character) QobiScheme)
(define-external (prefix-string prefix) QobiScheme)
(define-external (set-window-method! window event-type method) QobiScheme)
(define-external (send window event-type . &rest) QobiScheme)
(define-external (redraw-buttons) QobiScheme)
(define-external (redraw-display-pane) QobiScheme)
(define-external (redraw-transcript-pane) QobiScheme)
(define-external (redraw-echo-pane) QobiScheme)
(define-external (redraw-status-pane) QobiScheme)
(define-external (redraw-message-pane) QobiScheme)
(define-structure-external QobiScheme
 region button state-mask state x y width height method)
(define-external (define-region x y width height method) QobiScheme)
(define-external (define-button-specific-region
		  button state-mask state x y width height method)
 QobiScheme)
(define-external (region-handler x y button state) QobiScheme)
(define-external (set-background-task-enabler! procedure) QobiScheme)
(define-external (set-background-task-disabler! procedure) QobiScheme)
(define-external (abort?) QobiScheme)
(define-external (process-events) QobiScheme)
(define-external (quit) QobiScheme)
(define-external abort QobiScheme)
(define-external (control character) QobiScheme)
(define-external (meta character) QobiScheme)
(define-external (define-key character documentation command) QobiScheme)
(define-external (define-button x y text-procedure bold?-procedure method)
 QobiScheme)

(define-macro define-toggle-button
 (lambda (form expander)
  (unless (and (= (length form) 6) (symbol? (fifth form)))
   (error 'define-toggle-button "Improper DEFINE-TOGGLE-BUTTON: ~s" form))
  (expander `(define-button ,(second form) ,(third form) ,(fourth form)
	      (lambda () ,(fifth form))
	      (lambda ()
	       (set! ,(fifth form) (not ,(fifth form)))
	       (redraw-buttons)
	       (,(sixth form))))
	    expander)))

(define-macro define-radio-buttons
 (lambda (form expander)
  (unless (and (>= (length form) 3)
	       (symbol? (second form))
	       (every (lambda (element)
		       (and (list? element)
			    (= (length element) 4)
			    (symbol? (third element))))
		      (rest (rest (rest form)))))
   (error 'define-radio-buttons "Improper DEFINE-RADIO-BUTTONS: ~s" form))
  (expander `(begin
	      ,@(map (lambda (element)
		      `(define-button ,(first element) ,(second element)
			,(fourth element)
			(lambda () (eq? ,(second form) ',(third element)))
			(lambda ()
			 (set! ,(second form) ',(third element))
			 (redraw-buttons)
			 (,(third form)))))
		     (rest (rest (rest form)))))
	    expander)))

(define-macro define-cycle-button
 (lambda (form expander)
  (unless (and (>= (length form) 6)
	       (symbol? (fourth form))
	       (every (lambda (element)
		       (and (list? element)
			    (= (length element) 2)
			    (symbol? (first element))))
		      (rest (rest (rest (rest (rest form)))))))
   (error 'define-cycle-button "Improper DEFINE-CYCLE-BUTTON: ~s" form))
  (let ((symbols (map first (rest (rest (rest (rest (rest form))))))))
   (expander
    `(define-button ,(second form) ,(third form)
      (lambda ()
       (case ,(fourth form)
	,@(map (lambda (element) `((,(first element)) ,(second element)))
	       (rest (rest (rest (rest (rest form))))))
	(else (fuck-up))))
      #f
      (lambda ()
       (set! ,(fourth form)
	     (case ,(fourth form)
	      ,@(map (lambda (s1 s2) `((,s1) (set! ,(fourth form) ',s2)))
		     symbols
		     (append (rest symbols) (list (first symbols))))
	      (else (fuck-up))))
       (redraw-buttons)
       (,(fifth form))))
    expander))))

(define-macro define-integer-range-buttons
 (lambda (form expander)
  (unless (and (= (length form) 11) (symbol? (sixth form)))
   (error 'define-integer-range-buttons
	  "Improper DEFINE-INTEGER-RANGE-BUTTONS: ~s" form))
  (expander `(begin (define-button ,(second form) ,(third form) ,(ninth form)
		     #f
		     (lambda ()
		      (when (= ,(sixth form) ,(seventh form)) (abort))
		      (set! ,(sixth form) (- ,(sixth form) 1))
		      (redraw-buttons)
		      (,(eleventh form))))
		    (define-button ,(fourth form) ,(fifth form) ,(tenth form)
		     #f
		     (lambda ()
		      (when (= ,(sixth form) ,(eighth form)) (abort))
		      (set! ,(sixth form) (+ ,(sixth form) 1))
		      (redraw-buttons)
		      (,(eleventh form)))))
	    expander)))

(define-external (say string) QobiScheme)
(define-external (status string) QobiScheme)
(define-external message QobiScheme)
(define-external (set-pause! p) QobiScheme)
(define-external (pause) QobiScheme)
(define-external (tracking-pointer twice? press? procedure) QobiScheme)
(define-external kill-application QobiScheme)
(define-external (set-kill-application! procedure) QobiScheme)
(define-external (allocate-color-cube! reds greens blues) QobiScheme)
(define-external (draw-pixmap pixmap x y) QobiScheme)
(define-external *frame-rate* QobiScheme)
(define-external (draw-pixmaps pixmaps x y) QobiScheme)
(define-external (free-pixmap pixmap) QobiScheme)
(define-external (free-pixmaps pixmaps) QobiScheme)
(define-external (pnm->pixmap pnm) QobiScheme)
(define-external (pnm-movie->pixmaps pnm-movie) QobiScheme)
(define-structure-external QobiScheme
 tree-node width height offset text bold? procedure daughters)
(define-external (tree->tree-node tree) QobiScheme)
(define-external (draw-tree-node tree-node x y) QobiScheme)
(define-external (tree-height tree) QobiScheme)
(define-external (draw-tree tree x y) QobiScheme)
(define-structure-external QobiScheme
 alist-node width height offset keys values)
(define-external (alist->alist-node alist) QobiScheme)
(define-external (draw-alist-node alist-node x y) QobiScheme)
(define-external (alist-height alist) QobiScheme)
(define-external (draw-alist alist x y) QobiScheme)
(define-external abort-command QobiScheme)
(define-external (help-command) QobiScheme)
(define-external (help-scroll-up-line-command) QobiScheme)
(define-external (help-scroll-down-line-command) QobiScheme)
(define-external (help-scroll-up-page-command) QobiScheme)
(define-external (help-scroll-down-page-command) QobiScheme)
(define-external (help-scroll-beginning-command) QobiScheme)
(define-external (help-scroll-end-command) QobiScheme)
(define-external (echo-pane-command editor) QobiScheme)
(define-external (echo-pane-insert-character-command character) QobiScheme)
(define-external (echo-pane-beginning-of-line-command) QobiScheme)
(define-external (echo-pane-backward-char-command) QobiScheme)
(define-external (echo-pane-delete-char-command) QobiScheme)
(define-external (echo-pane-end-of-line-command) QobiScheme)
(define-external (echo-pane-forward-char-command) QobiScheme)
(define-external (echo-pane-kill-line-command) QobiScheme)
(define-external (echo-pane-backward-delete-char-command) QobiScheme)
(define-external (echo-pane-backward-word-command) QobiScheme)
(define-external (echo-pane-kill-word-command) QobiScheme)
(define-external (echo-pane-forward-word-command) QobiScheme)
(define-external (echo-pane-backward-kill-word-command) QobiScheme)

(define-macro define-display-pane-application
 ;; (DEFINE-DISPLAY-PANE-APPLICATION
 ;;  NAME
 ;;  DISPLAY-PANE-WIDTH
 ;;  DISPLAY-PANE-HEIGHT
 ;;  PRE-INITIALIZE-PROCEDURE
 ;;  POST-INITIALIZE-PROCEDURE
 ;;  FINALIZE-PROCEDURE
 ;;  REDRAW-PROCEDURE)
 (lambda (form expander)
  (expander
   `(define (,(second form) arguments)
     (let ((stalin? #f)
	   (display-pane-width ,(third form))
	   (display-pane-height ,(fourth form))
	   (pre-initialize-procedure ,(fifth form))
	   (post-initialize-procedure ,(sixth form))
	   (finalize-procedure ,(seventh form))
	   (redraw-procedure ,(eighth form)))
      (set! *post-initialize-procedure* post-initialize-procedure)
      (set! *transcript-pane* #f)
      (set! *echo-pane* #f)
      (set! *status-pane* #f)
      (set! *message-pane* #f)
      (set! *display* (xopendisplay *display-name*))
      (when (null-pointer? *display*)
       (panic "Cannot connect to X server: ~a" (xdisplayname *display-name*)))
      (set! *screen* (xdefaultscreen *display*))
      (set! *root-window* (xrootwindow *display* *screen*))
      (set! *button-width* 0)
      (set! *button-height* 0)
      (cond
       (stalin?
	(set! *white-pixel* (xwhitepixel *display* *screen*))
	(set! *black-pixel* (xblackpixel *display* *screen*)))
       (else
	(set! *background*
	      (xallocnamedcolor *display*
				(xdefaultcolormap *display* *screen*)
				*background-color*))
	(unless (= (first *background*) 1)
	 (panic "Can't allocate background colorcell"))
	(set! *foreground*
	      (xallocnamedcolor *display*
				(xdefaultcolormap *display* *screen*)
				*foreground-color*))
	(unless (= (first *foreground*) 1)
	 (panic "Can't allocate foreground colorcell"))))
      (set! *roman-font* (xloadqueryfont *display* *roman-font-name*))
      (when (null-pointer? *roman-font*)
       (panic "Cannot open font: ~a" *roman-font-name*))
      (set! *bold-font* (xloadqueryfont *display* *bold-font-name*))
      (when (null-pointer? *bold-font*)
       (panic "Cannot open font: ~a" *bold-font-name*))
      (set! *roman-height*
	    (+ (xfontstruct-ascent *roman-font*)
	       (xfontstruct-descent *roman-font*)))
      (set! *bold-height*
	    (+ (xfontstruct-ascent *bold-font*)
	       (xfontstruct-descent *bold-font*)))
      (set! *text-height*
	    (+ (max (xfontstruct-ascent *roman-font*)
		    (xfontstruct-ascent *bold-font*))
	       (max (xfontstruct-descent *roman-font*)
		    (xfontstruct-descent *bold-font*))))
      (set! *roman-baseline* (xfontstruct-descent *roman-font*))
      (set! *bold-baseline* (xfontstruct-descent *bold-font*))
      (set! *text-baseline* (max *roman-baseline* *bold-baseline*))
      (set! *display-pane-width* display-pane-width)
      (set! *display-pane-height* display-pane-height)
      (set! *who-line-height* 0)
      (set! *window*
	    (xcreatesimplewindow
	     *display* *root-window*
	     *window-position-x* *window-position-y*
	     *display-pane-width* *display-pane-height*
	     1
	     (if stalin?
		 *black-pixel*
		 (xcolor-pixel (second *foreground*)))
	     (if stalin?
		 *white-pixel*
		 (xcolor-pixel (second *background*)))))
      (xstorename *display* *window* *program*)
      (xseticonname *display* *window* *program*)
      (set! *display-pane* *window*)
      (xselectinput *display*
		    *display-pane*
		    (bit-or exposuremask
			    pointermotionmask
			    buttonpressmask
			    buttonreleasemask
			    keypressmask))
      (set! *thin-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *thin-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetforeground *display* *thin-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetlineattributes
       *display* *thin-gc* 0 linesolid capround joinround)
      (set! *thin-flipping-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *thin-flipping-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetforeground *display* *thin-flipping-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetlineattributes
       *display* *thin-flipping-gc* 0 linesolid capround joinround)
      (xsetfunction *display* *thin-flipping-gc* gxxor)
      (set! *medium-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *medium-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetforeground *display* *medium-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetlineattributes
       *display* *medium-gc* 2 linesolid capround joinround)
      (set! *medium-flipping-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *medium-flipping-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetforeground *display* *medium-flipping-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetlineattributes
       *display* *medium-flipping-gc* 2 linesolid capround joinround)
      (xsetfunction *display* *medium-flipping-gc* gxxor)
      (set! *thick-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *thick-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetforeground *display* *thick-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetlineattributes
       *display* *thick-gc* 5 linesolid capround joinround)
      (set! *thick-flipping-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *thick-flipping-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetforeground *display* *thick-flipping-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetlineattributes
       *display* *thick-flipping-gc* 5 linesolid capround joinround)
      (xsetfunction *display* *thick-flipping-gc* gxxor)
      (set! *dashed-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *dashed-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetforeground *display* *dashed-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetlineattributes
       *display* *dashed-gc* 0 lineonoffdash capround joinround)
      (set! *dashed-flipping-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *dashed-flipping-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetforeground *display* *dashed-flipping-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetlineattributes
       *display* *dashed-flipping-gc* 0 lineonoffdash capround joinround)
      (xsetfunction *display* *dashed-flipping-gc* gxxor)
      (set! *roman-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *roman-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetforeground *display* *roman-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetfont
       *display* *roman-gc* (xfontstruct-fid *roman-font*))
      (set! *bold-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *bold-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetforeground *display* *bold-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetfont
       *display* *bold-gc* (xfontstruct-fid *bold-font*))
      (set! *bold-flipping-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *bold-flipping-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetforeground *display* *bold-flipping-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetfont
       *display* *bold-flipping-gc* (xfontstruct-fid *bold-font*))
      (xsetlineattributes
       *display* *bold-flipping-gc* 0 linesolid capround joinround)
      (xsetfunction *display* *bold-flipping-gc* gxxor)
      (unless stalin?
       (set! *light-gray*
	     (xallocnamedcolor *display*
			       (xdefaultcolormap *display* *screen*)
			       "Light Gray"))
       (unless (= (first *light-gray*) 1)
	(panic "Can't allocate light gray colorcell"))
       (set! *light-gray-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *light-gray-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *light-gray-gc*
		       (xcolor-pixel (second *light-gray*)))
       (xsetlineattributes
	*display* *light-gray-gc* 0 linesolid capround joinround)
       (set! *gray*
	     (xallocnamedcolor *display*
			       (xdefaultcolormap *display* *screen*)
			       "Gray"))
       (unless (= (first *gray*) 1)
	(panic "Can't allocate gray colorcell"))
       (set! *gray-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *gray-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *gray-gc*
		       (xcolor-pixel (second *gray*)))
       (xsetlineattributes
	*display* *gray-gc* 0 linesolid capround joinround)
       (set! *red*
	     (xallocnamedcolor *display*
			       (xdefaultcolormap *display* *screen*)
			       "Red"))
       (unless (= (first *red*) 1)
	(panic "Can't allocate red colorcell"))
       (set! *red-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *red-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *red-gc*
		       (xcolor-pixel (second *red*)))
       (xsetfont
	*display* *red-gc* (xfontstruct-fid *roman-font*))
       (xsetlineattributes
	*display* *red-gc* 0 linesolid capround joinround)
       (set! *dark-red*
	     (xallocnamedcolor *display*
			       (xdefaultcolormap *display* *screen*)
			       "Dark Red"))
       (unless (= (first *dark-red*) 1)
	(panic "Can't allocate dark red colorcell"))
       (set! *dark-red-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *dark-red-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *dark-red-gc*
		       (xcolor-pixel (second *dark-red*)))
       (xsetfont
	*display* *dark-red-gc* (xfontstruct-fid *roman-font*))
       (xsetlineattributes
	*display* *dark-red-gc* 0 linesolid capround joinround)
       (set! *green*
	     (xallocnamedcolor *display*
			       (xdefaultcolormap *display* *screen*)
			       "Green"))
       (unless (= (first *green*) 1)
	(panic "Can't allocate green colorcell"))
       (set! *green-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *green-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *green-gc*
		       (xcolor-pixel (second *green*)))
       (xsetfont
	*display* *green-gc* (xfontstruct-fid *roman-font*))
       (xsetlineattributes
	*display* *green-gc* 0 linesolid capround joinround)
       (set! *dark-green*
	     (xallocnamedcolor *display*
			       (xdefaultcolormap *display* *screen*)
			       "Dark Green"))
       (unless (= (first *dark-green*) 1)
	(panic "Can't allocate dark green colorcell"))
       (set! *dark-green-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *dark-green-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *dark-green-gc*
		       (xcolor-pixel (second *dark-green*)))
       (xsetfont
	*display* *dark-green-gc* (xfontstruct-fid *roman-font*))
       (xsetlineattributes
	*display* *dark-green-gc* 0 linesolid capround joinround)
       (set! *blue*
	     (xallocnamedcolor *display*
			       (xdefaultcolormap *display* *screen*)
			       "Blue"))
       (unless (= (first *blue*) 1)
	(panic "Can't allocate blue colorcell"))
       (set! *blue-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *blue-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *blue-gc*
		       (xcolor-pixel (second *blue*)))
       (xsetfont
	*display* *blue-gc* (xfontstruct-fid *roman-font*))
       (xsetlineattributes
	*display* *blue-gc* 0 linesolid capround joinround)
       (set! *yellow*
	     (xallocnamedcolor *display*
			       (xdefaultcolormap *display* *screen*)
			       "Yellow"))
       (unless (= (first *yellow*) 1)
	(panic "Can't allocate yellow colorcell"))
       (set! *yellow-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *yellow-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *yellow-gc*
		       (xcolor-pixel (second *yellow*)))
       (xsetfont
	*display* *yellow-gc* (xfontstruct-fid *roman-font*))
       (xsetlineattributes
	*display* *yellow-gc* 0 linesolid capround joinround)
       (set! *violet*
	     (xallocnamedcolor *display*
			       (xdefaultcolormap *display* *screen*)
			       "Violet"))
       (unless (= (first *violet*) 1)
	(panic "Can't allocate violet colorcell"))
       (set! *violet-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *violet-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *violet-gc*
		       (xcolor-pixel (second *violet*)))
       (xsetfont
	*display* *violet-gc* (xfontstruct-fid *roman-font*))
       (xsetlineattributes
	*display* *violet-gc* 0 linesolid capround joinround)
       (set! *orange*
	     (xallocnamedcolor *display*
			       (xdefaultcolormap *display* *screen*)
			       "Orange"))
       (unless (= (first *orange*) 1)
	(panic "Can't allocate orange colorcell"))
       (set! *orange-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *orange-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *orange-gc*
		       (xcolor-pixel (second *orange*)))
       (xsetfont
	*display* *orange-gc* (xfontstruct-fid *roman-font*))
       (xsetlineattributes
	*display* *orange-gc* 0 linesolid capround joinround)
       (set! *dark-orange*
	     (xallocnamedcolor *display*
			       (xdefaultcolormap *display* *screen*)
			       "Dark Orange"))
       (unless (= (first *dark-orange*) 1)
	(panic "Can't allocate dark orange colorcell"))
       (set! *dark-orange-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *dark-orange-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *dark-orange-gc*
		       (xcolor-pixel (second *dark-orange*)))
       (xsetfont
	*display* *dark-orange-gc* (xfontstruct-fid *roman-font*))
       (xsetlineattributes
	*display* *dark-orange-gc* 0 linesolid capround joinround))
      (set! *color-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *color-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetforeground *display* *color-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetlineattributes
       *display* *color-gc* 0 linesolid capround joinround)
      (set! *window-methods* '())
      (set! *abort-button* #f)
      (set! *abort-key* #f)
      (set! *comtab* (make-vector 256 #f))
      (set! *help* '())
      (define-key (control #\h) "Help" help-command)
      (set! *help* '())
      (define-key (control #\n) "Scroll help window down one line"
       help-scroll-down-line-command)
      (define-key (control #\p) "Scroll help window up one line"
       help-scroll-up-line-command)
      (define-key (control #\v) "Scroll help window down one page"
       help-scroll-down-page-command)
      (define-key (meta #\v) "Scroll help window up one page"
       help-scroll-up-page-command)
      (define-key (meta #\<) "Scroll help window to beginning"
       help-scroll-beginning-command)
      (define-key (meta #\>) "Scroll help window to end"
       help-scroll-end-command)
      (set! *help-comtab* *comtab*)
      (set! *comtab* (make-vector 256 #f))
      (set! *prefix* '())
      (set! *status* "Tyi")
      (set! *message* "")
      (set! *redraw-procedure* redraw-procedure)
      (set! *buttons* '())
      (set! *pause?* #f)
      (set! *help?* #f)
      (set! *clear-display-pane?* #t)
      (let ((hints (make-xwmhints)))
       (xwmhints-input! hints 1)
       (xwmhints-flags! hints inputhint)
       (xsetwmhints *display* *window* hints))
      (let ((hints (make-xsizehints)))
       (when *window-position?*
	(xsizehints-x! hints *window-position-x*)
	(xsizehints-y! hints *window-position-y*))
       (xsizehints-min_width! hints *display-pane-width*)
       (xsizehints-max_width! hints *display-pane-width*)
       (xsizehints-min_height! hints *display-pane-height*)
       (xsizehints-max_height! hints *display-pane-height*)
       (xsizehints-flags! hints
			  (if *window-position?*
			      (+ usposition pposition pminsize pmaxsize)
			      (+ pminsize pmaxsize)))
       (xsetwmnormalhints *display* *window* hints))
      (pre-initialize-procedure)
      (set-window-method! *display-pane* 'expose redraw-display-pane)
      (set-window-method! *display-pane* 'buttonpress region-handler)
      (when *transcript-pane*
       (set-window-method!
	*transcript-pane* 'expose redraw-transcript-pane))
      (when *echo-pane*
       (set-window-method! *echo-pane* 'expose redraw-echo-pane))
      (set-kill-application!
       (lambda ()
	(set-kill-application! (lambda () #t))
	(finalize-procedure)
	(when *display*
	 (xfreegc *display* *thin-gc*)
	 (xfreegc *display* *thin-flipping-gc*)
	 (xfreegc *display* *medium-gc*)
	 (xfreegc *display* *medium-flipping-gc*)
	 (xfreegc *display* *thick-gc*)
	 (xfreegc *display* *thick-flipping-gc*)
	 (xfreegc *display* *dashed-gc*)
	 (xfreegc *display* *dashed-flipping-gc*)
	 (xfreegc *display* *roman-gc*)
	 (xfreegc *display* *bold-gc*)
	 (xfreegc *display* *bold-flipping-gc*)
	 (unless stalin?
	  (xfreegc *display* *light-gray-gc*)
	  (xfreegc *display* *gray-gc*)
	  (xfreegc *display* *red-gc*)
	  (xfreegc *display* *dark-red-gc*)
	  (xfreegc *display* *green-gc*)
	  (xfreegc *display* *dark-green-gc*)
	  (xfreegc *display* *blue-gc*)
	  (xfreegc *display* *yellow-gc*)
	  (xfreegc *display* *violet-gc*)
	  (xfreegc *display* *orange-gc*)
	  (xfreegc *display* *dark-orange-gc*)
	  (xfreegc *display* *color-gc*)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *background*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *foreground*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *light-gray*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *gray*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *red*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *dark-red*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *green*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *dark-green*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *blue*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *yellow*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *violet*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *orange*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *dark-orange*))))
		       1
		       0))
	 (xunloadfont *display* (xfontstruct-fid *roman-font*))
	 (xunloadfont *display* (xfontstruct-fid *bold-font*))
	 (xdestroywindow *display* *window*)
	 (xclosedisplay *display*)
	 (set! *display* #f))
	#t))
      (xmapsubwindows *display* *window*)
      (xmapraised *display* *window*)
      (process-events)
      (kill-application)))
   expander)))

(define-macro define-application
 ;; (DEFINE-APPLICATION
 ;;  NAME
 ;;  DISPLAY-PANE-WIDTH
 ;;  DISPLAY-PANE-HEIGHT
 ;;  TRANSCRIPT-LINES
 ;;  BUTTON-ROWS
 ;;  BUTTOM-COLUMNS
 ;;  PRE-INITIALIZE-PROCEDURE
 ;;  POST-INITIALIZE-PROCEDURE
 ;;  FINALIZE-PROCEDURE
 ;;  REDRAW-PROCEDURE
 ;;  LISTENER-PROCEDURE)
 (lambda (form expander)
  (expander
   `(define (,(second form) arguments)
     (let* ((stalin? #f)
	    (display-pane-width ,(third form))
	    (display-pane-height ,(fourth form))
	    (transcript-lines ,(fifth form))
	    (button-rows ,(sixth form))
	    (button-columns ,(seventh form))
	    (button-width
	     (if display-pane-width
		 (- (quotient (+ display-pane-width 4) button-columns)
		    4)
		 100))
	    (width (if display-pane-width
		       (+ display-pane-width 6)
		       (+ (* button-columns (+ button-width 4)) 2)))
	    (pre-initialize-procedure ,(eighth form))
	    (post-initialize-procedure ,(ninth form))
	    (finalize-procedure ,(tenth form))
	    (redraw-procedure ,(eleventh form))
	    (listener-procedure
	     ,(if (= (length form) 12) (twelfth form) '(lambda () #f))))
      (set! *post-initialize-procedure* post-initialize-procedure)
      (set! *transcript-pane* #f)
      (set! *echo-pane* #f)
      (set! *display* (xopendisplay *display-name*))
      (when (null-pointer? *display*)
       (panic "Cannot connect to X server: ~a" (xdisplayname *display-name*)))
      (set! *screen* (xdefaultscreen *display*))
      (set! *root-window* (xrootwindow *display* *screen*))
      (cond
       (stalin?
	(set! *white-pixel* (xwhitepixel *display* *screen*))
	(set! *black-pixel* (xblackpixel *display* *screen*)))
       (else
	(set! *background*
	      (xallocnamedcolor *display*
				(xdefaultcolormap *display* *screen*)
				*background-color*))
	(unless (= (first *background*) 1)
	 (panic "Can't allocate background colorcell"))
	(set! *foreground*
	      (xallocnamedcolor *display*
				(xdefaultcolormap *display* *screen*)
				*foreground-color*))
	(unless (= (first *foreground*) 1)
	 (panic "Can't allocate foreground colorcell"))))
      (set! *roman-font* (xloadqueryfont *display* *roman-font-name*))
      (when (null-pointer? *roman-font*)
       (panic "Cannot open font: ~a" *roman-font-name*))
      (set! *bold-font* (xloadqueryfont *display* *bold-font-name*))
      (when (null-pointer? *bold-font*)
       (panic "Cannot open font: ~a" *bold-font-name*))
      (set! *roman-height*
	    (+ (xfontstruct-ascent *roman-font*)
	       (xfontstruct-descent *roman-font*)))
      (set! *bold-height*
	    (+ (xfontstruct-ascent *bold-font*)
	       (xfontstruct-descent *bold-font*)))
      (set! *text-height*
	    (+ (max (xfontstruct-ascent *roman-font*)
		    (xfontstruct-ascent *bold-font*))
	       (max (xfontstruct-descent *roman-font*)
		    (xfontstruct-descent *bold-font*))))
      (set! *roman-baseline* (xfontstruct-descent *roman-font*))
      (set! *bold-baseline* (xfontstruct-descent *bold-font*))
      (set! *text-baseline* (max *roman-baseline* *bold-baseline*))
      (set! *button-width* button-width)
      (set! *button-height* (+ *text-height* 4))
      (set! *display-pane-width* (- width 6))
      (set! *display-pane-height* display-pane-height)
      (when transcript-lines
       (unless (zero? transcript-lines)
	(set! *transcript-pane-height*
	      (+ (* transcript-lines *text-height*) 4)))
       (set! *echo-pane-height* (+ *text-height* 4)))
      (set! *who-line-height* (+ *text-height* 4))
      (set! *status-pane-width*
	    (+ (max (xtextwidth *roman-font* "Tyi" 3)
		    (xtextwidth *roman-font* "Run" 3)
		    (xtextwidth *roman-font* "Pause" 5)
		    (xtextwidth *roman-font* "Track" 5))
	       4))
      (set! *window*
	    (xcreatesimplewindow
	     *display* *root-window*
	     *window-position-x* *window-position-y*
	     width
	     (if transcript-lines
		 (if (zero? transcript-lines)
		     (+ (* button-rows (+ *button-height* 4))
			*display-pane-height*
			*echo-pane-height*
			*who-line-height*
			14)
		     (+ (* button-rows (+ *button-height* 4))
			*display-pane-height*
			*transcript-pane-height*
			*echo-pane-height*
			*who-line-height*
			18))
		 (+ (* button-rows (+ *button-height* 4))
		    *display-pane-height*
		    *who-line-height*
		    10))
	     1
	     (if stalin?
		 *black-pixel*
		 (xcolor-pixel (second *foreground*)))
	     (if stalin?
		 *white-pixel*
		 (xcolor-pixel (second *background*)))))
      (xstorename *display* *window* *program*)
      (xseticonname *display* *window* *program*)
      (xselectinput *display*
		    *window*
		    (bit-or exposuremask
			    pointermotionmask
			    buttonpressmask
			    buttonreleasemask
			    keypressmask))
      (set! *display-pane*
	    (xcreatesimplewindow
	     *display* *window*
	     2 (+ (* button-rows (+ *button-height* 4)) 2)
	     *display-pane-width* *display-pane-height*
	     1
	     (if stalin?
		 *black-pixel*
		 (xcolor-pixel (second *foreground*)))
	     (if stalin?
		 *white-pixel*
		 (xcolor-pixel (second *background*)))))
      (xselectinput *display*
		    *display-pane*
		    (bit-or exposuremask
			    pointermotionmask
			    buttonpressmask
			    buttonreleasemask
			    keypressmask))
      (when transcript-lines
       (unless (zero? transcript-lines)
	(set! *transcript-pane*
	      (xcreatesimplewindow
	       *display* *window*
	       2
	       (+ (* button-rows (+ *button-height* 4))
		  *display-pane-height*
		  6)
	       *display-pane-width* *transcript-pane-height* 1
	       (if stalin?
		   *black-pixel*
		   (xcolor-pixel (second *foreground*)))
	       (if stalin?
		   *white-pixel*
		   (xcolor-pixel (second *background*)))))
	(xselectinput
	 *display* *transcript-pane* (bit-or exposuremask keypressmask)))
       (set! *echo-pane*
	     (xcreatesimplewindow
	      *display* *window*
	      2
	      (if (zero? transcript-lines)
		  (+ (* button-rows (+ *button-height* 4))
		     *display-pane-height*
		     6)
		  (+ (* button-rows (+ *button-height* 4))
		     *display-pane-height* *transcript-pane-height* 10))
	      *display-pane-width* *echo-pane-height* 1
	      (if stalin?
		  *black-pixel*
		  (xcolor-pixel (second *foreground*)))
	      (if stalin?
		  *white-pixel*
		  (xcolor-pixel (second *background*)))))
       (xselectinput
	*display* *echo-pane* (bit-or exposuremask keypressmask)))
      (set! *status-pane*
	    (xcreatesimplewindow
	     *display* *window*
	     2
	     (+ (* button-rows (+ *button-height* 4))
		*display-pane-height*
		(if transcript-lines
		    (if (zero? transcript-lines)
			(+ *echo-pane-height* 10)
			(+ *transcript-pane-height*
			   *echo-pane-height*
			   14))
		    6))
	     *status-pane-width* *who-line-height*
	     1
	     (if stalin?
		 *black-pixel*
		 (xcolor-pixel (second *foreground*)))
	     (if stalin?
		 *white-pixel*
		 (xcolor-pixel (second *background*)))))
      (xselectinput
       *display* *status-pane* (bit-or exposuremask keypressmask))
      (set! *message-pane*
	    (xcreatesimplewindow
	     *display* *window*
	     (+ *status-pane-width* 6)
	     (+ (* button-rows (+ *button-height* 4))
		*display-pane-height*
		(if transcript-lines
		    (if (zero? transcript-lines)
			(+ *echo-pane-height* 10)
			(+ *transcript-pane-height*
			   *echo-pane-height*
			   14))
		    6))
	     (- width *status-pane-width* 10) *who-line-height*
	     1
	     (if stalin?
		 *black-pixel*
		 (xcolor-pixel (second *foreground*)))
	     (if stalin?
		 *white-pixel*
		 (xcolor-pixel (second *background*)))))
      (xselectinput
       *display* *message-pane* (bit-or exposuremask keypressmask))
      (set! *thin-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *thin-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetforeground *display* *thin-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetlineattributes
       *display* *thin-gc* 0 linesolid capround joinround)
      (set! *thin-flipping-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *thin-flipping-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetforeground *display* *thin-flipping-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetlineattributes
       *display* *thin-flipping-gc* 0 linesolid capround joinround)
      (xsetfunction *display* *thin-flipping-gc* gxxor)
      (set! *medium-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *medium-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetforeground *display* *medium-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetlineattributes
       *display* *medium-gc* 2 linesolid capround joinround)
      (set! *medium-flipping-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *medium-flipping-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetforeground *display* *medium-flipping-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetlineattributes
       *display* *medium-flipping-gc* 2 linesolid capround joinround)
      (xsetfunction *display* *medium-flipping-gc* gxxor)
      (set! *thick-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *thick-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetforeground *display* *thick-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetlineattributes
       *display* *thick-gc* 5 linesolid capround joinround)
      (set! *thick-flipping-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *thick-flipping-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetforeground *display* *thick-flipping-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetlineattributes
       *display* *thick-flipping-gc* 5 linesolid capround joinround)
      (xsetfunction *display* *thick-flipping-gc* gxxor)
      (set! *dashed-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *dashed-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetforeground *display* *dashed-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetlineattributes
       *display* *dashed-gc* 0 lineonoffdash capround joinround)
      (set! *dashed-flipping-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *dashed-flipping-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetforeground *display* *dashed-flipping-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetlineattributes
       *display* *dashed-flipping-gc* 0 lineonoffdash capround joinround)
      (xsetfunction *display* *dashed-flipping-gc* gxxor)
      (set! *roman-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *roman-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetforeground *display* *roman-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetfont
       *display* *roman-gc* (xfontstruct-fid *roman-font*))
      (set! *bold-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *bold-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetforeground *display* *bold-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetfont
       *display* *bold-gc* (xfontstruct-fid *bold-font*))
      (set! *bold-flipping-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *bold-flipping-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetforeground *display* *bold-flipping-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetfont
       *display* *bold-flipping-gc* (xfontstruct-fid *bold-font*))
      (xsetlineattributes
       *display* *bold-flipping-gc* 0 linesolid capround joinround)
      (xsetfunction *display* *bold-flipping-gc* gxxor)
      (unless stalin?
       (set! *light-gray*
	     (xallocnamedcolor *display*
			       (xdefaultcolormap *display* *screen*)
			       "Light Gray"))
       (unless (= (first *light-gray*) 1)
	(panic "Can't allocate light gray colorcell"))
       (set! *light-gray-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *light-gray-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *light-gray-gc*
		       (xcolor-pixel (second *light-gray*)))
       (xsetlineattributes
	*display* *light-gray-gc* 0 linesolid capround joinround)
       (set! *gray*
	     (xallocnamedcolor *display*
			       (xdefaultcolormap *display* *screen*)
			       "Gray"))
       (unless (= (first *gray*) 1)
	(panic "Can't allocate gray colorcell"))
       (set! *gray-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *gray-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *gray-gc*
		       (xcolor-pixel (second *gray*)))
       (xsetlineattributes
	*display* *gray-gc* 0 linesolid capround joinround)
       (set! *red*
	     (xallocnamedcolor *display*
			       (xdefaultcolormap *display* *screen*)
			       "Red"))
       (unless (= (first *red*) 1)
	(panic "Can't allocate red colorcell"))
       (set! *red-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *red-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *red-gc*
		       (xcolor-pixel (second *red*)))
       (xsetfont
	*display* *red-gc* (xfontstruct-fid *roman-font*))
       (xsetlineattributes
	*display* *red-gc* 0 linesolid capround joinround)
       (set! *dark-red*
	     (xallocnamedcolor *display*
			       (xdefaultcolormap *display* *screen*)
			       "Dark Red"))
       (unless (= (first *dark-red*) 1)
	(panic "Can't allocate dark red colorcell"))
       (set! *dark-red-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *dark-red-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *dark-red-gc*
		       (xcolor-pixel (second *dark-red*)))
       (xsetfont
	*display* *dark-red-gc* (xfontstruct-fid *roman-font*))
       (xsetlineattributes
	*display* *dark-red-gc* 0 linesolid capround joinround)
       (set! *green*
	     (xallocnamedcolor *display*
			       (xdefaultcolormap *display* *screen*)
			       "Green"))
       (unless (= (first *green*) 1)
	(panic "Can't allocate green colorcell"))
       (set! *green-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *green-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *green-gc*
		       (xcolor-pixel (second *green*)))
       (xsetfont
	*display* *green-gc* (xfontstruct-fid *roman-font*))
       (xsetlineattributes
	*display* *green-gc* 0 linesolid capround joinround)
       (set! *dark-green*
	     (xallocnamedcolor *display*
			       (xdefaultcolormap *display* *screen*)
			       "Dark Green"))
       (unless (= (first *dark-green*) 1)
	(panic "Can't allocate dark green colorcell"))
       (set! *dark-green-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *dark-green-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *dark-green-gc*
		       (xcolor-pixel (second *dark-green*)))
       (xsetfont
	*display* *dark-green-gc* (xfontstruct-fid *roman-font*))
       (xsetlineattributes
	*display* *dark-green-gc* 0 linesolid capround joinround)
       (set! *blue*
	     (xallocnamedcolor *display*
			       (xdefaultcolormap *display* *screen*)
			       "Blue"))
       (unless (= (first *blue*) 1)
	(panic "Can't allocate blue colorcell"))
       (set! *blue-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *blue-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *blue-gc*
		       (xcolor-pixel (second *blue*)))
       (xsetfont
	*display* *blue-gc* (xfontstruct-fid *roman-font*))
       (xsetlineattributes
	*display* *blue-gc* 0 linesolid capround joinround)
       (set! *yellow*
	     (xallocnamedcolor *display*
			       (xdefaultcolormap *display* *screen*)
			       "Yellow"))
       (unless (= (first *yellow*) 1)
	(panic "Can't allocate yellow colorcell"))
       (set! *yellow-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *yellow-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *yellow-gc*
		       (xcolor-pixel (second *yellow*)))
       (xsetfont
	*display* *yellow-gc* (xfontstruct-fid *roman-font*))
       (xsetlineattributes
	*display* *yellow-gc* 0 linesolid capround joinround)
       (set! *violet*
	     (xallocnamedcolor *display*
			       (xdefaultcolormap *display* *screen*)
			       "Violet"))
       (unless (= (first *violet*) 1)
	(panic "Can't allocate violet colorcell"))
       (set! *violet-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *violet-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *violet-gc*
		       (xcolor-pixel (second *violet*)))
       (xsetfont
	*display* *violet-gc* (xfontstruct-fid *roman-font*))
       (xsetlineattributes
	*display* *violet-gc* 0 linesolid capround joinround)
       (set! *orange*
	     (xallocnamedcolor *display*
			       (xdefaultcolormap *display* *screen*)
			       "Orange"))
       (unless (= (first *orange*) 1)
	(panic "Can't allocate orange colorcell"))
       (set! *orange-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *orange-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *orange-gc*
		       (xcolor-pixel (second *orange*)))
       (xsetfont
	*display* *orange-gc* (xfontstruct-fid *roman-font*))
       (xsetlineattributes
	*display* *orange-gc* 0 linesolid capround joinround)
       (set! *dark-orange*
	     (xallocnamedcolor *display*
			       (xdefaultcolormap *display* *screen*)
			       "Dark Orange"))
       (unless (= (first *dark-orange*) 1)
	(panic "Can't allocate dark orange colorcell"))
       (set! *dark-orange-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *dark-orange-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *dark-orange-gc*
		       (xcolor-pixel (second *dark-orange*)))
       (xsetfont
	*display* *dark-orange-gc* (xfontstruct-fid *roman-font*))
       (xsetlineattributes
	*display* *dark-orange-gc* 0 linesolid capround joinround))
      (set! *color-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *color-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetforeground *display* *color-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetlineattributes
       *display* *color-gc* 0 linesolid capround joinround)
      (set! *window-methods* '())
      (set! *abort-button* #f)
      (set! *abort-key* #f)
      (set! *comtab* (make-vector 256 #f))
      (set! *help* '())
      (define-key (control #\h) "Help" help-command)
      (set! *help* '())
      (define-key (control #\n) "Scroll help window down one line"
       help-scroll-down-line-command)
      (define-key (control #\p) "Scroll help window up one line"
       help-scroll-up-line-command)
      (define-key (control #\v) "Scroll help window down one page"
       help-scroll-down-page-command)
      (define-key (meta #\v) "Scroll help window up one page"
       help-scroll-up-page-command)
      (define-key (meta #\<) "Scroll help window to beginning"
       help-scroll-beginning-command)
      (define-key (meta #\>) "Scroll help window to end"
       help-scroll-end-command)
      (set! *help-comtab* *comtab*)
      (set! *comtab* (make-vector 256 #f))
      (when transcript-lines
       (set! *transcript* '())
       (set! *input* "")
       (set! *input-position* 0)
       (let ((help *help*))
	(for-each
	 (lambda (character)
	  (define-key character
	   "Enter the typed character into the echo pane"
	   (lambda () (echo-pane-insert-character-command character))))
	 (append
	  (string->list
	   "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
	  (string->list
	   "1234567890-=\\`!@#$%^&*()_+|~[]{};':\",./<>? ")))
	(set! *help* help))
       (define-key (control #\a)
	"Move the cursor to the beginning of the echo pane"
	echo-pane-beginning-of-line-command)
       (define-key (control #\b)
	"Move the cursor backward one character in the echo pane"
	echo-pane-backward-char-command)
       (define-key (control #\d)
	"Delete the character after the cursor in the echo pane"
	echo-pane-delete-char-command)
       (define-key (control #\e)
	"Move the cursor to the end of the echo pane"
	echo-pane-end-of-line-command)
       (define-key (control #\f)
	"Move the cursor forward one character in the echo pane"
	echo-pane-forward-char-command)
       (define-key (control #\k)
	"Delete all characters after the cursor in the echo pane"
	echo-pane-kill-line-command)
       (define-key delete
	"Delete the character before the cursor in the echo pane"
	echo-pane-backward-delete-char-command)
       (define-key return
	"Process the input in the echo pane"
	(lambda ()
	 (set! *transcript* (cons (list 'user *input*) *transcript*))
	 (listener-procedure)
	 (set! *input* "")
	 (set! *input-position* 0)
	 (redraw-transcript-pane)
	 (redraw-echo-pane)))
       (define-key (meta #\b)
	"Move the cursor backward one word in the echo pane"
	echo-pane-backward-word-command)
       (define-key (meta #\d)
	"Delete the word after the cursor in the echo pane"
	echo-pane-kill-word-command)
       (define-key (meta #\f)
	"Move the cursor forward one word in the echo pane"
	echo-pane-forward-word-command)
       (define-key (meta delete)
	"Delete the word before the cursor in the echo pane"
	echo-pane-backward-kill-word-command))
      (set! *prefix* '())
      (set! *status* "Tyi")
      (set! *message* "")
      (set! *redraw-procedure* redraw-procedure)
      (set! *buttons* '())
      (set! *pause?* #f)
      (set! *help?* #f)
      (set! *clear-display-pane?* #t)
      (let ((hints (make-xwmhints)))
       (xwmhints-input! hints 1)
       (xwmhints-flags! hints inputhint)
       (xsetwmhints *display* *window* hints))
      (let ((hints (make-xsizehints))
	    (height (if transcript-lines
			(if (zero? transcript-lines)
			    (+ (* button-rows (+ *button-height* 4))
			       *display-pane-height*
			       *echo-pane-height*
			       *who-line-height*
			       14)
			    (+ (* button-rows (+ *button-height* 4))
			       *display-pane-height*
			       *transcript-pane-height*
			       *echo-pane-height*
			       *who-line-height*
			       18))
			(+ (* button-rows (+ *button-height* 4))
			   *display-pane-height*
			   *who-line-height*
			   10))))
       (when *window-position?*
	(xsizehints-x! hints *window-position-x*)
	(xsizehints-y! hints *window-position-y*))
       (xsizehints-min_width! hints width)
       (xsizehints-max_width! hints width)
       (xsizehints-min_height! hints height)
       (xsizehints-max_height! hints height)
       (xsizehints-flags! hints
			  (if *window-position?*
			      (+ usposition pposition pminsize pmaxsize)
			      (+ pminsize pmaxsize)))
       (xsetwmnormalhints *display* *window* hints))
      (pre-initialize-procedure)
      (set-window-method! *display-pane* 'expose redraw-display-pane)
      (set-window-method! *display-pane* 'buttonpress region-handler)
      (when *transcript-pane*
       (set-window-method!
	*transcript-pane* 'expose redraw-transcript-pane))
      (when *echo-pane*
       (set-window-method! *echo-pane* 'expose redraw-echo-pane))
      (set-window-method! *status-pane* 'expose redraw-status-pane)
      (set-window-method! *message-pane* 'expose redraw-message-pane)
      (set-kill-application!
       (lambda ()
	(set-kill-application! (lambda () #t))
	(finalize-procedure)
	(when *display*
	 (xfreegc *display* *thin-gc*)
	 (xfreegc *display* *thin-flipping-gc*)
	 (xfreegc *display* *medium-gc*)
	 (xfreegc *display* *medium-flipping-gc*)
	 (xfreegc *display* *thick-gc*)
	 (xfreegc *display* *thick-flipping-gc*)
	 (xfreegc *display* *dashed-gc*)
	 (xfreegc *display* *dashed-flipping-gc*)
	 (xfreegc *display* *roman-gc*)
	 (xfreegc *display* *bold-gc*)
	 (xfreegc *display* *bold-flipping-gc*)
	 (unless stalin?
	  (xfreegc *display* *light-gray-gc*)
	  (xfreegc *display* *gray-gc*)
	  (xfreegc *display* *red-gc*)
	  (xfreegc *display* *dark-red-gc*)
	  (xfreegc *display* *green-gc*)
	  (xfreegc *display* *dark-green-gc*)
	  (xfreegc *display* *blue-gc*)
	  (xfreegc *display* *yellow-gc*)
	  (xfreegc *display* *violet-gc*)
	  (xfreegc *display* *orange-gc*)
	  (xfreegc *display* *dark-orange-gc*)
	  (xfreegc *display* *color-gc*)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *background*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *foreground*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *light-gray*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *gray*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *red*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *dark-red*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *green*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *dark-green*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *blue*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *yellow*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *violet*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *orange*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *dark-orange*))))
		       1
		       0))
	 (xunloadfont *display* (xfontstruct-fid *roman-font*))
	 (xunloadfont *display* (xfontstruct-fid *bold-font*))
	 (xdestroywindow *display* *window*)
	 (xclosedisplay *display*)
	 (set! *display* #f))
	#t))
      (xmapsubwindows *display* *window*)
      (xmapraised *display* *window*)
      (process-events)
      (kill-application)))
   expander)))

;;; Debugger

(define-external (qobischeme-read-eval-print . &rest) QobiScheme)

;;; Command Processor

(define-external (string-argument string usage) QobiScheme)
(define-external (integer-argument string usage) QobiScheme)
(define-external (real-argument string usage) QobiScheme)

(define-macro define-command
 (lambda (form expander)
  (unless (and (list? form)
	       (>= (length form) 2)
	       (valid-command-arguments? (second form)))
   (error 'define-command "Improper DEFINE-COMMAND: ~s" form))
  (define (valid-command-arguments? l)
   (define (valid-optional-parameter? l)
    (and (list? l)
	 (= (length l) 4)
	 (symbol? (first l))
	 (string? (second l))))
   (define (valid-required-parameter? l)
    (and (list? l)
	 (= (length l) 3)
	 (symbol? (first l))
	 (string? (second l))))
   (define (order-ok-optional? l)
    (or (null? l)
	(and (eq? (first (first l)) 'optional)
	     (order-ok-optional? (rest l)))
	(and (eq? (first (first l)) 'rest)
	     (null? (rest l)))))
   (define (order-ok-required? l)
    (or (null? l)
	(and (eq? (first (first l)) 'required)
	     (order-ok-required? (rest l)))
	(and (eq? (first (first l)) 'optional)
	     (order-ok-optional? (rest l)))
	(and (eq? (first (first l)) 'rest)
	     (null? (rest l)))))
   (define (order-ok? l)
    (or (null? l)
	(and (or (eq? (first (first l)) 'any-number)
		 (eq? (first (first l)) 'at-least-one)
		 (eq? (first (first l)) 'at-most-one)
		 (eq? (first (first l)) 'exactly-one))
	     (order-ok? (rest l)))
	(and (eq? (first (first l)) 'required)
	     (order-ok-required? (rest l)))
	(and (eq? (first (first l)) 'optional)
	     (order-ok-optional? (rest l)))
	(and (eq? (first (first l)) 'rest)
	     (null? (rest l)))))
   (and
    (list? l)
    (>= (length l) 1)
    (symbol? (first l))
    (every
     (lambda (l)
      (and
       (list? l)
       (>= (length l) 1)
       (or (and (or (eq? (first l) 'exactly-one) (eq? (first l) 'at-most-one))
		(>= (length l) 2)
		(every
		 (lambda (l)
		  (and (list? l)
		       (>= (length l) 2)
		       (string? (first l))
		       (symbol? (second l))
		       (every valid-optional-parameter? (rest (rest l)))))
		 (rest l)))
	   (and (or (eq? (first l) 'at-least-one) (eq? (first l) 'any-number))
		(>= (length l) 2)
		(every
		 (lambda (l)
		  (and (list? l)
		       (>= (length l) 2)
		       (string? (first l))
		       (symbol? (second l))
		       (every valid-required-parameter? (rest (rest l)))))
		 (rest l)))
	   (and (or (eq? (first l) 'required) (eq? (first l) 'rest))
		(= (length l) 2)
		(valid-required-parameter? (second l)))
	   (and (eq? (first l) 'optional)
		(= (length l) 2)
		(valid-optional-parameter? (second l))))))
     (rest l))
    (order-ok? (rest l))))
  (define (command-usage l)
   (define (command-usage1 l)
    (let ((s (let loop ((l l))
	      (define (command-usage l)
	       (string-append
		"-"
		(first l)
		(let loop ((l (rest (rest l))))
		 (cond
		  ((null? l) "")
		  ((null? (rest l)) (string-append " " (second (first l))))
		  (else
		   (string-append " " (second (first l)) (loop (rest l))))))))
	      (if (null? (rest l))
		  (command-usage (first l))
		  (string-append
		   (command-usage (first l)) "|" (loop (rest l)))))))
     (if (= (length l) 1) s (string-append "[" s "]"))))
   (if (null? l)
       ""
       (case (first (first l))
	((any-number)
	 (string-append " ["
			(command-usage1 (rest (first l)))
			"]*"
			(command-usage (rest l))))
	((at-least-one)
	 (string-append " ["
			(command-usage1 (rest (first l)))
			"]+"
			(command-usage (rest l))))
	((at-most-one)
	 (string-append
	  " [" (command-usage1 (rest (first l))) "]" (command-usage (rest l))))
	((exactly-one)
	 (string-append
	  " " (command-usage1 (rest (first l))) (command-usage (rest l))))
	((required)
	 (string-append " "
			(second (second (first l)))
			(command-usage (rest l))))
	((optional)
	 (string-append " ["
			(second (second (first l)))
			(command-usage (rest l)) "]"))
	((rest) (string-append " [" (second (second (first l))) "]*"))
	(else (fuck-up)))))
  (define (command-bindings l)
   (if (null? l)
       '()
       (case (first (first l))
	((any-number at-least-one)
	 (append (map-reduce append
			     '()
			     (lambda (l)
			      (cons (list (second l) #f)
				    (map (lambda (l) (list (first l) ''()))
					 (rest (rest l)))))
			     (rest (first l)))
		 (command-bindings (rest l))))
	((at-most-one exactly-one)
	 (append (map-reduce
		  append
		  '()
		  (lambda (l)
		   (cons (list (second l) #f)
			 (map (lambda (l) (list (first l) (fourth l)))
			      (rest (rest l)))))
		  (rest (first l)))
		 (command-bindings (rest l))))
	((required) (cons (list (first (second (first l))) #f)
			  (command-bindings (rest l))))
	((optional) (cons (list (first (second (first l)))
				(fourth (second (first l))))
			  (command-bindings (rest l))))
	((rest) (cons (list (first (second (first l))) ''())
		      (command-bindings (rest l))))
	(else (fuck-up)))))
  (define (command-keyword-argument-parser l)
   (cons
    `(let loop ()
      (unless (null? arguments)
       (cond
	,@(let loop ((l l))
	   (if (null? l)
	       '(((string=? (first arguments) "-usage") (usage)))
	       (case (first (first l))
		((any-number at-least-one)
		 (append
		  (map (lambda (l)
			`((string=? (first arguments)
				    ,(string-append "-" (first l)))
			  (set! arguments (rest arguments))
			  (set! ,(second l) #t)
			  ,@(map-reduce
			     append
			     '()
			     (lambda (l)
			      `((when (null? arguments) (usage))
				(set! ,(first l)
				      (cons (,(third l) (first arguments) usage)
					    ,(first l)))
				(set! arguments (rest arguments))))
			     (rest (rest l)))
			  (loop)))
		       (rest (first l)))
		  (loop (rest l))))
		((at-most-one exactly-one)
		 (append
		  (map (lambda (l1)
			`((string=? (first arguments)
				    ,(string-append "-" (first l1)))
			  (set! arguments (rest arguments))
			  (when (or ,@(map second (rest (first l)))) (usage))
			  (set! ,(second l1) #t)
			  ,@(map-reduce
			     append
			     '()
			     (lambda (l)
			      `((when (null? arguments) (usage))
				(set! ,(first l)
				      (,(third l) (first arguments) usage))
				(set! arguments (rest arguments))))
			     (rest (rest l1)))
			  (loop)))
		       (rest (first l)))
		  (loop (rest l))))
		((required optional rest) (loop (rest l)))
		(else (fuck-up))))))))
    (let loop ((l l))
     (if (null? l)
	 '()
	 (case (first (first l))
	  ((at-least-one exactly-one)
	   (cons `(unless (or ,@(map second (rest (first l)))) (usage))
		 (loop (rest l))))
	  ((at-most-one any-number required optional rest) (loop (rest l)))
	  (else (fuck-up)))))))
  (define (command-positional-argument-parser l)
   (let loop ((l l))
    (if (null? l)
	'((unless (null? arguments) (usage)))
	(case (first (first l))
	 ((any-number at-least-one at-most-one exactly-one) (loop (rest l)))
	 ((required)
	  (append
	   `((when (null? arguments) (usage))
	     (set! ,(first (second (first l)))
		   (,(third (second (first l))) (first arguments) usage))
	     (set! arguments (rest arguments)))
	   (loop (rest l))))
	 ((optional)
	  (cons `(unless (null? arguments)
		  (set! ,(first (second (first l)))
			(,(third (second (first l))) (first arguments) usage))
		  (set! arguments (rest arguments)))
		(loop (rest l))))
	 ((rest)
	  `((let loop ()
	     (unless (null? arguments)
	      (set! ,(first (second (first l)))
		    (cons (,(third (second (first l))) (first arguments) usage)
			  ,(first (second (first l)))))
	      (set! arguments (rest arguments))
	      (loop)))))
	 (else (fuck-up))))))
  (expander
   `(define (,(first (second form)) arguments)
     (define (string-argument string usage) string)
     (define (integer-argument string usage)
      (let ((integer (string->number string)))
       (unless (integer? integer) (usage))
       integer))
     (define (real-argument string usage)
      (let ((real (string->number string)))
       (unless (real? real) (usage))
       real))
     (let ((program (first arguments)))
      (define (usage)
       (format
	stderr-port
	,(string-append "usage: ~a" (command-usage (rest (second form))) "~%")
	program)
       (exit -1))
      (set! arguments (rest arguments))
      (let ,(command-bindings (rest (second form)))
       ,@(command-keyword-argument-parser (rest (second form)))
       ,@(command-positional-argument-parser (rest (second form)))
       ,@(rest (rest form)))))
   expander)))

;;; Tam V'Nishlam Shevah L'El Borei Olam
