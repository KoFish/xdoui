(define-module (helpers)
  #:export (destroy-win
             take 
             quote-chars 
             divide-array
             box*))

(use-modules (ncurses curses)
             (ice-9 match)
             (srfi srfi-11)) 

(define (destroy-win win)
  (let ((s (normal #\sp)))
    (border win s s s s s s s s)
    (refresh win)
    (delwin win)))

(define (box* w) 
  (border w (acs-vline) (acs-vline)
            (acs-hline) (acs-hline)
            (acs-ltee) (acs-rtee)
            (acs-llcorner) (acs-lrcorner))) 

(define (take lst n)
  "Reimplementation of take since the default guile implementation does not allow
  taking more elements than is in the list."
  (match lst
    ('() '())
    ((?h . ?r) (if (> n 0) (cons ?h (take ?r (1- n))) '()))))

(define (quote-chars ch)
  "Quote characters to be printed in the UI."
  (match ch
    (#\newline "\\n")
    (#\tab "\\t")
    (#\esc "\\esc")
    ((? string? ?str) (if (> (string-length ?str) 10)
                         (string-concatenate (list (string-take ?str 8) ".."))
                         ?str))
    (else ch)))

(define (divide-array n d)
  "Divide a by b and returns an array of b elements such that the sum of all
  elements equal to a with as less difference between the elements as possible."
  (let-values (((r q) (floor/ n d)))
    (append (make-list q (1+ r)) (make-list (- d q) r))))

