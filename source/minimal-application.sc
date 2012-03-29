;;; LaHaShem HaAretz U'Mloah
;;; Copyright 2000 and 2001 NEC Research Institute, Inc. All rights reserved.
;;; Copyright 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, and 2011
;;; Purdue University. All rights reserved.

(include "QobiScheme.sch")
(include "minimal-application.sch")

(set! *program* "minimal-application")
(set! *panic?* #t)

;;; Macros

;;; Structures

;;; Variables

;;; Parameters

;;; Procedures

;;; Commands

;;; Top Level

(define-command (main (required (n "#" integer-argument)))
 (when (member n *interesting-numbers*)
  (display "Wow!")
  (newline))
 (write (fact n))
 (newline))

;;; Tam V'Nishlam Shevah L'El Borei Olam
