;;; LaHaShem HaAretz U'Mloah
;;; Copyright 2000 and 2001 NEC Research Institute, Inc. All rights reserved.
;;; Copyright 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, and 2011
;;; Purdue University. All rights reserved.

(include "QobiScheme.sch")
(include "minimal-projectlib-component.sch")

;;; Macros

;;; Structures

;;; Variables

;;; Parameters

(define *interesting-numbers* '(23 42 77))

;;; Procedures

(define (fact n) (if (zero? n) 1 (* n (fact (- n 1)))))

;;; Commands

;;; Tam V'Nishlam Shevah L'El Borei Olam
