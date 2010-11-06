;;; LaHaShem HaAretz U'Mloah

(module qsci (with QobiScheme xlib) (main qsci))

(include "QobiScheme-AD.sch")
(eval-when (load) (include "QobiScheme.load"))

(define-c-external (nobuff) void "nobuff")

(define-c-external (c-cd pointer) int "cd")

(define (cd pathname)
 (unless (string? pathname) (panic "Argument to CD must be a string"))
 (let ((result (c-cd pathname)))
  (unless (zero? result) (panic "Cannot CD to directory"))
  #t))

(set! *program* "qsci")
(set! *panic?* #f)
(nobuff)

(define (qsci arguments) (apply qobischeme-read-eval-print arguments))

;;; Tam V'Nishlam Shevah L'El Borei Olam
