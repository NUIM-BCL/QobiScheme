;;; LaHaShem HaAretz U'Mloah
;;; Copyright 1993, 1994, and 1995 University of Toronto. All rights reserved.
;;; Copyright 1996 Technion. All rights reserved.
;;; Copyright 1996 and 1997 University of Vermont. All rights reserved.
;;; Copyright 1997, 1998, 1999, 2000, and 2001 NEC Research Institute, Inc. All
;;; rights reserved.
;;; Copyright 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, and 2011
;;; Purdue University. All rights reserved.

(module body (main body))

(define (read-line)
 (define (read-line)
  (let ((c (read-char)))
   (if (eq? c #\newline) '() (cons c (read-line)))))
 (list->string (read-line)))

(define (discard-header)
 (unless (string=? (read-line) "% begin word count") (discard-header)))

(define (discard-trailer)
 (let ((l (read-line)))
  (unless (string=? l "% end word count")
   (display l)
   (newline)
   (discard-trailer))))

(define (body) (discard-header) (discard-trailer))

;;; Tam V'Nishlam Shevah L'El Borei Olam
