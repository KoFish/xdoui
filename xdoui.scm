#!/usr/bin/guile \
-e main -s
coding: utf-8
!#

(use-modules (ncurses curses)
             (oop goops)) 
(use-modules (ice-9 i18n)
             (ice-9 match)
             (ice-9 format)
             (ice-9 rdelim)
             (ice-9 popen)
             (ice-9 vlist)
             (ice-9 optargs))

(use-modules (srfi srfi-1)
             (srfi srfi-9)
             (srfi srfi-9 gnu)
             (srfi srfi-11)
             (srfi srfi-88))

(use-modules (xdo libxdo))

(setlocale LC_ALL "en_US.UTF-8")

(define key-bindings
  `((#\" change-register store) 
    (#\' change-register load) 
    (#\h show-help)
    (#\newline set-prefix text)
    (#\r prefix-to-reg)
    (#\t (#\p ,(λ (xdoui regs state . rest)
                  (debug-print xdoui "Debug!!\n")
                  (add-prompt-flash xdoui 3 "Three!!" )
                  (add-prompt-flash xdoui 5 "Five!!" )
                  (add-prompt-flash xdoui 4 "Four!!" )
                  (print xdoui 'registers "Reg!!\n")
                  (print xdoui 'test "Test!!\n")
                  (print xdoui #f "Main!!\n")) test-print)
         (#\a ,(λ (xdoui regs state . rest)
                  (add-to-sidebar! xdoui 'test)
                  (print xdoui 'test "Test window!!\n")) test-add)
         (#\d ,(λ (xdoui regs state . rest)
                  (remove-from-sidebar! xdoui 'test)) test-delete))
    (#\p (#\e set-prefix position)
         (timeout 300 print-register))
    (#\m (#\g (#\p get-mouse-position))
         (#\p put-mouse-position)
         (#\w get-mouse-window)
         (#\c click-mouse))
    (#\w (#\p put-window-position)
         (#\g (#\a get-active-window)
              (#\p get-window-position))
         (timeout 300 get-active-window))
    (#\q quit "Exited by user"))) 

(define-record-type command-type
                    (make-command command arguments register)
                    command?
                    (command   get-command)
                    (arguments get-arguments set-arguments!)
                    (register  get-register  set-register!))

(define-class <xdoui> ()
  (xdo             #:getter   get-xdo         #:init-keyword #:xdo) 
  (scr             #:getter   get-screen      #:init-keyword #:scr) 
  (right-width     #:accessor right-width     #:init-value   0)  
  (sidebar-windows #:accessor sidebar-windows #:init-value   '()) 
  (main-window     #:accessor main-window     #:init-value   #f)  
  (prompt-window   #:accessor prompt-window   #:init-value   #f)
  (prompt-text     #:getter get-prompt #:setter set-prompt! #:init-value   "")
  (flash-elements  #:accessor flashes         #:init-value   '()))

(define (make-xdoui xdo scr)
  (let ((xdoui (make <xdoui> #:xdo xdo #:scr scr)))
    (resize-xdoui! xdoui)
    xdoui))

(define-method (draw-prompt (xdoui <xdoui>))
  (let ((win (prompt-window xdoui))
        (mx  (getmaxx (prompt-window xdoui))))
    (hline win (normal #\Space) mx #:x 0 #:y 0)
    (addstr win (get-prompt xdoui) #:x 0 #:y 0)
    (if (not (null? (flashes xdoui)))
        (let ((s (string-join (map car (sort-list (flashes xdoui) (λ (a b) (< (cdr a) (cdr b))))) ", ")))
          (let ((len (string-length s)))
            (addstr win s #:y 0 #:x (if (> (floor/ mx 2) len) (- mx len) (floor/ mx 2))))))
    (refresh win)))

(define-method (set-prompt (xdoui <xdoui>) (str <string>) . args)
  "Set the text of the prompt-window at the bottom of the screen."
  (set-prompt! xdoui (apply format #f str args))
  (draw-prompt xdoui))

;; flashes is a alist of name and a timeout, each second the timeout
;; is ticked down until it reaches zero, when it does it's removed 
;; from the list. It's used to show information that isn't stricly
;; necessary but that can be of interest, especially during debugging.
;;
;; A flashes alist can look like this:
;; (("test" . 5) ("debug" . 3) ("printing" . 6))

(define-method (add-prompt-flash (xdoui <xdoui>) timeout (str <string>) . args)
  (set! (flashes xdoui)
    (assv-set! (flashes xdoui) (apply format #f str args) timeout))
  (draw-prompt xdoui))

(define-method (tick-flashes (xdoui <xdoui>))
  (set! (flashes xdoui) (filter (λ (e) 
                                   (positive? (cdr e))) 
                                (map (λ (e) 
                                        (cons (car e) (1- (cdr e)))) 
                                     (flashes xdoui)))))

(define-method (print (xdoui <xdoui>) window (str <string>))
  "Add a string to the window specified by the window argument. The window
  argument is either #f for the main window or a symbol specifing a sub
  window on the right hand side."
  (if window
      (cond ((assv-ref (sidebar-windows xdoui) window)
             => (λ (w) (let ((w (cdr w)))
                            (addstr w str)
                            (refresh w)))))
      (let ((win (main-window xdoui)))
        (addstr win str)
        (refresh win))))

(define-method (print (xdoui <xdoui>) window (str <string>) a . args)
  (print xdoui window (apply format #f str a args)))

(define* (debug-print xdoui str . arguments)
  (cond ((get-sidebar-window! xdoui 'debug)
         => (λ (win)
               (apply print xdoui 'debug str arguments))) 
        (else (begin
                (add-to-sidebar! xdoui 'debug)
                (apply debug-print xdoui str arguments)))))

(define-method (get-next-char (xdoui <xdoui>) timeout)
  "Read the next character from input or return false if timeout is reached."
  (if timeout (timeout! (get-screen xdoui) timeout))
  (let ((ch (getch (get-screen xdoui))))
    (if timeout (begin (timeout! (get-screen xdoui) -1) (raw!)))
    (if (not (eqv? ch KEY_RESIZE))
        ch
        (let ((scr (get-screen xdoui))) 
          (timeout! (get-screen xdoui) 500)
          (let loop ((ch (getch (get-screen xdoui))))
            (if (eqv? ch KEY_RESIZE) 
                (loop (getch (get-screen xdoui)))
                (begin
                  (resize-xdoui! xdoui)
                  (timeout! (get-screen xdoui) -1)
                  ch)))))))

(define (destroy-win win)
  (let ((s (normal #\sp)))
    (border win s s s s s s s s)
    (refresh win)
    (delwin win)))

(define (divide-array n d)
  "Divide a by b and returns an array of b elements such that the sum of all
  elements equal to a with as less difference between the elements as possible."
  (let-values (((r q) (floor/ n d)))
    (append (make-list q (1+ r)) (make-list (- d q) r))))

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

(define (abort history . reason)
  (throw 'abort history (if (> (length reason) 0) (car reason) "Aborted")))

(define-method (resize-xdoui! (xdoui <xdoui>))
  "Resize the whole screen to the current terminal size, it also clears the
  content of all the sidebar windows."
  (let*  ((scr (get-screen xdoui))
          (my (getmaxy scr)) 
          (mx (getmaxx scr)) 
          (width (min (floor (/ mx 2)) 50)) )
    (if (main-window xdoui) (delwin (main-window xdoui)))
    (if (prompt-window xdoui) (delwin (prompt-window xdoui)))
    (set! (main-window xdoui) (subwin scr (- my 3) (- mx width) 0 0))
    (set! (prompt-window xdoui) (subwin scr 1 mx (- my 2) 0))
    (set! (right-width xdoui) width)
    (scrollok! (main-window xdoui) #t)
    (resize-sidebar! xdoui)
    (draw-prompt xdoui)
    (hline scr (acs-hline) (- mx width) #:y (- my 3) #:x 0)  
    (refresh (main-window xdoui))
    (refresh (prompt-window xdoui))))

(define-method (resize-sidebar! (xdoui <xdoui>))
  "Recreates all the sidebar windows in proper new size accord to the size of the
  terminal."
  (let ((win-count (length (sidebar-windows xdoui)))) 
    (if (positive? win-count) 
        (let* ((my (getmaxy (get-screen xdoui))) 
               (mx (getmaxx (get-screen xdoui)))
               (whs (divide-array (- my 3) win-count))
               (whsa (reverse (fold (λ (e a) (cons (+ e (car a)) a)) (list 0) whs)))
               (w (right-width xdoui)))
          (let-values (((wh q) (floor/ (- my 3) win-count))) 
            (set! (sidebar-windows xdoui) 
              ;; Destroy the old windows and create new ones to replace 
              ;; them.
              (map (λ (e wh x)
                      (cond ((cdr e) 
                             => (λ (win)
                                   (destroy-win (cdr win))
                                   (destroy-win (car win)))))
                      (let* ((wino (subwin (get-screen xdoui) (1+ wh) w x (- mx w))) 
                             (wini (derwin wino (- wh 2) (- w 2) 1 1))) 
                        (scrollok! wini #t) 
                        (refresh wini) 
                        (cons (car e) (cons wino wini)))) 
                   (sidebar-windows xdoui)
                   whs
                   whsa)) 
            ;; Draw frames around each windows and write the title over the
            ;; top border.
            (map (λ (e p) 
                    (let ((n (car e))
                          (wo (cadr e))
                          (wi (cddr e)))
                      (if (not p)
                          (box wo (acs-vline) (acs-hline))
                          (border wo 
                                  (acs-vline) (acs-vline)
                                  (acs-hline) (acs-hline)
                                  (acs-ltee) (acs-rtee)
                                  (acs-llcorner) (acs-lrcorner))) 
                      (addstr wo (string-capitalize (symbol->string n)) #:x 2 #:y 0)
                      (refresh wo)))
                 (sidebar-windows xdoui)
                 (cons #f (cdr (make-list win-count #t)))))))))

(define-method (add-to-sidebar! (xdoui <xdoui>) name)
  (cond ((assv name (sidebar-windows xdoui)) #f)
        (else (set! (sidebar-windows xdoui) (assv-set! (sidebar-windows xdoui) name #f)))) 
  (resize-sidebar! xdoui))

(define-method (remove-from-sidebar! (xdoui <xdoui>) name)
  (cond ((assv name (sidebar-windows xdoui))
         => (λ (win)
               (let ((wini (cddr win))
                     (wino (cadr win)))
                 (set! (sidebar-windows xdoui) (assv-remove! (sidebar-windows xdoui) name))
                 (destroy-win wini) ; It's important to delete the windows in the right
                 (destroy-win wino) ; order, inner first and then outer.
                 (resize-sidebar! xdoui))))
        (else #f)))

(define-method (get-sidebar-window (xdoui <xdoui>) name)
  (cond ((assv name (sidebar-windows xdoui))
         => (λ (v) (cdr v)))
        (else #f)))

(define-method (get-sidebar-window! (xdoui <xdoui>) name)
  (cond ((get-sidebar-window xdoui name) => (λ (w) (cdr w)))
        (else (add-to-sidebar! xdoui name)
              (get-sidebar-window! xdoui name))))

(define (update-register registers key value) 
  (vhash-consv key value (vhash-delv key registers)))

(define (register->string register) 
  (match register
    (((? number? x) . (? number? y)) (format #f "[X ~a | Y ~a]" x y))
    ((? window? window) (format #f "[Win: ~a]" window))
    ((? string? str) (format #f "[String: ~s]" str))
    (e (format #f "[Unknown: ~a]" e))))

(define-class <xdoui-dialog-prompt> ()
  (xdoui #:getter get-xdoui #:init-keyword #:xdoui)
  (prompt #:getter get-prompt #:init-keyword #:prompt)
  (win #:accessor window)
  (swin #:accessor subwindow)
  (input #:accessor input-row))

(define (make-dialog-prompt xdoui prompt)
  (let ((p (make <xdoui-dialog-prompt> #:xdoui xdoui #:prompt prompt))
        (myx (getmaxyx (get-screen xdoui))))
    (let* ((w (floor (/ (cadr myx) 3)))
           (x (floor (/ (- (cadr myx) w) 2)))
           (y (- (floor (/ (car myx) 2)) 2))
           (win (newwin 4 w y x))) 
      (let ((swin (derwin win 2 (- w 2) 1 1))) 
        (let ((input (derwin win 1 (- (- w 4) (string-length prompt)) 1 (+ 2 (string-length prompt)))))  
          (set! (window p) win) 
          (set! (subwindow p) swin) 
          (set! (input-row p) input) 
          (box (window p) (acs-vline) (acs-hline))
          (refresh (window p)) 
          (attr-set! (input-row p) A_REVERSE)
          (update-dialog-prompt p '()))))))

(define-method (update-dialog-prompt (p <xdoui-dialog-prompt>) acc)
  (let ((w (getmaxx (input-row p))))
    (addstr (subwindow p) (get-prompt p) #:x 0 #:y 0)
    (hline (input-row p) (bold #\space)  w #:x 0 #:y 0) 
    (touchwin (window p))
    (refresh (window p)) 
    (addstr (input-row p) (list->string (reverse (take acc w))) #:x 0 #:y 0)
    (refresh (input-row p))
    p))

(define-method (destroy-prompt (p <xdoui-dialog-prompt>))
  (delwin (input-row p))
  (delwin (subwindow p))
  (destroy-win (window p))
  (touchwin (get-screen (get-xdoui p)))
  (refresh (get-screen (get-xdoui p))))

(define-method (add-prompt-flash (p <xdoui-dialog-prompt>) (str <string>) . args)
  (addstr (subwindow p) (apply format #f str args) #:x 1 #:y 1)
  (refresh (subwindow p)))

(define (read-while-dialog xdoui p? prompt)
  (let* ((prompt (make-dialog-prompt xdoui prompt))
         (p (λ (acc)
               (update-dialog-prompt prompt acc)))
         (p-e (λ (str . args)
                 (apply add-prompt-flash 5 prompt str args))))
    (with-throw-handler 'abort
      (λ ()
         (let ((acc (read-while-generic xdoui p? p p-e)))
           (destroy-prompt prompt)
           (if (null? acc) '() (cdr acc))))
      (λ (key . args)
         (destroy-prompt prompt)))))

(define (read-while-prompt xdoui p? prompt)
  (let* ((p (λ (acc)
               (set-prompt xdoui "~a: ~{~a~}" prompt (map quote-chars (reverse acc)))))
         (p-e (λ (str . args)
                 (apply add-prompt-flash xdoui 5 str args))))
    (let ((acc (read-while-generic xdoui p? p p-e)))
      (if (not (null? acc)) 
          (begin (ungetch (car acc)) (cdr acc))
          '()))))

(define (read-while-generic xdoui p? p p-e)
  (let ((backspace? (λ (c) (or (eqv? c KEY_BACKSPACE) (eqv? c #\backspace))))
        (escape?    (λ (c) (or (eqv? c #\esc) (eqv? c #\x03))))
        (finish?    (λ (c) (or (eqv? c #\x04))))
        (no-key?    (λ (c) (and (not (char? c)) (> c 255)))))
    (let read-stuff ((acc '())
                     (ch (get-next-char xdoui #f)))
      (match ch
        (#f (read-stuff acc (get-next-char xdoui #f)))
        ((? backspace?)
         (let ((acc (if (null? acc) '() (cdr acc))))
           (p acc)
           (read-stuff acc (get-next-char xdoui #f))))
        ((? escape?) (abort (cons ch acc) "Input aborted"))
        ((? finish?) (cons ch acc))
        ((? no-key? ?key)
         (p-e "Invalid character (~a)" ?key)
         (read-stuff acc (get-next-char xdoui #f)))
        ((? p?)
         (let ((acc (cons ch acc)))
           (p acc)
           (read-stuff acc (get-next-char xdoui #f))))
        (else (cons ch acc))))))

#|
 |(define-syntax define-action
 |  (syntax-rules (store load key)
 |    ((_ name (xdoui registers state rest ...)
 |        ((load load-reg) (store store-reg) (key ch))
 |        exp exp* ...)
 |     (define (name xdoui registers state rest ...)
 |       (let ((load-reg (if (identifier? load-reg) (cond ((vhash-assv 'load state) => (λ (e) (cdr e))))))
 |             (store-reg (if (identifier? store-reg) (cond ((vhash-assv 'store state) => (λ (e) (cdr e))))))
 |             (ch (if (identifier? ch) (get-next-char xdoui #f))))
 |         exp exp* ...)))))
 |#

#|
 |(define-action get-current-window (xdoui registers state history args)
 |               ((load #f) (store streg) (key #f))
 |               (debug-print xdoui "Stuff! ~s\n" streg))
 |#

(define (change-register xdoui registers state history type)
  "Change the current registers used as operators."
  (let ((ch (get-next-char xdoui #f)))
    (if (and (char? ch) (char-alphabetic? ch))
        (begin
          (values `(set-register ,type ,ch)
                  (cons ch history))) 
        (abort (cons ch history) "Not a valid register, use only alphabetic characters"))))

(define (show-help xdoui registers state history . rest)
  "Show the description of the action called with a particular
  keybinding."
  (values `(add-to-state show-help #t)
          history))

(define (prefix-to-reg xdoui registers state history . args)
  "Set the content of a register, specified by the store register, to the 
  current prefix value."
  (cond ((vhash-assv 'store state)
         => (λ (reg)
               (cond ((vhash-assv 'prefix state) 
                      => (λ (prx)
                            (values `(add-to-register ,(cdr reg) ,(cdr prx))
                                    history))))))))

(define (set-prefix xdoui registers state history type . args)
  "Set the prefix."
  (match type
    ('text
     (let ((result (read-while-dialog xdoui (λ (c) (not (eqv? c #\newline))) "Text")))
       (if (null? result)
           (abort (append result history) "Empty string")
           (let ((str (list->string (reverse result))))
             (values `(add-to-state prefix ,str)
                     (cons str history))))))
    ('number
     (let ((result (read-while-prompt xdoui char-numeric? "Number prefix")))
       (cond ((locale-string->integer (list->string (reverse result)))
              => (λ (r) (values `(add-to-state prefix ,r)
                                (cons r history))))
             (else (abort (append result history) "Not a valid integer")))))
    ('position
     (let ((result (read-while-dialog xdoui (λ (c) (or (eqv? c #\x) (char-numeric? c))) "Position prefix")))
       (if (> (length result) 0)
           (let ((es (string-split (list->string (reverse result)) #\x)))
             (cond ((or (eq? (length es) 1) (eq? (length es) 2))
                    (let ((es (map (λ (e) (or (locale-string->integer e) 0)) es)))
                      (let ((es (if (> (length es) 1) 
                                    (cons (car es) (cadr es))
                                    (reverse (cons 0 (car es))))))
                        (values `(add-to-state prefix ,es)
                                (append result history)))))
                   ((> (length es) 2) 
                    (abort (append result history) "Too many x's in the string"))))
           (abort (append result history) "Not enough characters entered"))))
    (else
      (abort history "This type of prefix isn't implemented yet."))))

(define (get-mouse-position xdoui registers state history . rest)
  "Get the current mouse position, either print to main-window or
  store in specified register."
  (cond ((xdo-get-mouse-location (get-xdo xdoui))
         => (λ (result)
               (cond ((vhash-assv 'store state)
                      => (λ (value)
                            (values `(add-to-register ,(cdr value) ,(cons (car result) (cadr result)))
                                    history)))
                     (else (values `(value ,result)
                                   history)))))
        (else 
          (abort history "Could not get mouse position"))))

(define (print-register xdoui registers state history . rest)
  "Print the content of the register specified in the load or store
  register."
  (cond ((or (vhash-assv 'load state) (vhash-assv 'store state))
         => (λ (reg)
               (cond ((vhash-assv (cdr reg) registers)
                      => (λ (cnt)
                            (print xdoui #f "Register ~c: ~a\n" (car cnt) (register->string (cdr cnt)))))
                     (else
                       (abort history (format #f "No content in register \"~c\"" (cdr reg)))))))
        (else
          (abort history "No register specified"))))

(define (print-all-registers xdoui registers)
  (cond ((get-sidebar-window! xdoui 'registers)
         => (λ (scr)
               (let ((x (getcurx scr))
                     (mx (getmaxx scr)))
                 (vhash-fold (λ (key value count)
                                (addstr scr (format #f "Reg '~a': ~a" key (register->string value)) 
                                        #:x 0 #:y count #:n mx)
                                (1+ count)) 
                             0 registers)
                 (refresh scr)))) 
        (else (let ((myx (getmaxyx (get-screen xdoui))))
                (add-to-sidebar! xdoui 'registers)
                (print-all-registers xdoui registers)))))

(define* (do-command xdoui registers parse-tree)
  (let cmd-loop ((state vlist-null) 
                 (history '())
                 (branch parse-tree))
    (let ((call-proc 
            (λ (ƒ . args)
               (with-throw-handler 'abort
                 (λ ()
                    (let-values (((result . new-history) (apply ƒ xdoui registers state history args)))
                      (cond ((vhash-assv 'prefix state) 
                             => (λ (p)
                                   (debug-print xdoui "Prefix: ~a~%" (cdr p)))))
                      (if (not (boolean? result))
                          (debug-print xdoui "Result: ~S~%" result))
                      (let ((history (if (not (null? new-history)) 
                                         (car new-history) 
                                         history))) 
                        (match result
                          (('set-register ?type ?reg . _)
                           (set-prompt xdoui "> ~{~a~^-~}" (map quote-chars (reverse history))) 
                           (cmd-loop (vhash-consq ?type ?reg state) history parse-tree))
                          (('add-to-state ?key ?value)
                           (let ((state (vhash-consv ?key ?value state)))
                             (cmd-loop state history parse-tree)))
                          (('value ?value) (print xdoui #f "Result: ~a~%" ?value))
                          (_ result)))))
                 (λ (key . args) #f)))))
      (match branch 
        ((((? char?) . _) . _)
         (print-all-registers xdoui registers)
         (let ((timeout (assv-ref branch 'timeout)))
           (let ((ch (get-next-char xdoui (if timeout (car timeout) #f))))
             (match ch
               (#f (if timeout (cmd-loop state history (cdr timeout))))
               (#\x03 (abort history "Keyboard interrupt"))
               (else (let ((history (cons ch history)))
                       (set-prompt xdoui "> ~{~a~^-~}" (map quote-chars (reverse history)))
                       (if (char? ch)
                           (if (char-numeric? ch)
                               (begin
                                 (ungetch ch)
                                 (let* ((res (read-while-prompt xdoui char-numeric? ">"))  
                                        (v (locale-string->integer (list->string (reverse res)))))
                                   (cmd-loop (vhash-consv 'prefix v state)
                                             (cons v (cdr history))
                                             branch)))
                               (let ((new-branch (assq ch branch)))
                                 (if new-branch
                                     (cmd-loop state history (cdr new-branch))
                                     (abort history (format #f "~a is not a recognized character" (quote-chars ch))))))
                           (abort history "Aborted"))))))))
        (((? procedure? ƒ) (? symbol? ?symbol) . ?args)
         (let ((help? (vhash-assv 'show-help state)))
           (if (and help? (cdr help?))
               (cond ((procedure-documentation ƒ)
                      => (λ (help-text)
                            (print xdoui #f "~%Description: ~%  ~a~%" 
                                   (string-join 
                                     (map string-trim-both 
                                          (string-split help-text #\newline))
                                     " ")))))
               (begin
                 (add-prompt-flash xdoui 3 "~S" ?symbol)
                 (apply call-proc ƒ ?args)))))
        (('quit ?reason) `(quit ,?reason))
        (((? symbol? ?symbol) . args)
         (cond ((false-if-exception (module-ref (current-module) ?symbol))
                => (λ (ƒ)
                      (cmd-loop state history (cons ƒ branch))))
               (else (abort history (format #f "No such function: (~S)" ?symbol)))))
        (_ (debug-print xdoui "Unknown: ~S\n" branch))))))

(define (main-loop xdoui)
  (resize-xdoui! xdoui)
  (refresh (get-screen xdoui))
  (call-with-new-thread (λ () (let loop ((counter 0))
                                   (sleep 1)
                                   (tick-flashes xdoui)
                                   (draw-prompt xdoui)
                                   (refresh (prompt-window xdoui))
                                   (loop (1+ counter)))))
  (let loop ((registers vlist-null))
    (catch 'abort
      (λ ()
         (let ((result (do-command xdoui registers key-bindings)))
           (if (not (match result
                      (('quit ?reason)
                       (print xdoui #f "Quit: ~a\n" ?reason))
                      (_ #f)))
               (begin
                 (for-each (λ (e) 
                              (if (cdr e) (refresh (cddr e))))
                           (sidebar-windows xdoui))
                 (match result
                   (('add-to-register ?key ?value)
                    (let ((new-registers (vhash-consv ?key ?value (vhash-delv ?key registers))))
                      (loop new-registers)))
                   (('update-registers (? vlist? ?new-registers))
                    (loop ?new-registers))
                   (_
                     (loop registers)))))))
      (λ (key history reason)
         (set-prompt xdoui "-- aborted --")
         (print xdoui #f "Abort: ~a [~{~a~^-~}]\n" reason (map quote-chars (reverse history)))
         (loop registers)))))

(define (main args)
  (let ((stdscr (initscr))
        (xdo (new-xdo)))
    (raw!)
    (keypad! stdscr #t)
    (noecho!)
    (if (has-colors?)
      (begin
        (start-color!)
        ;(init-pair! 1 COLOR_GREEN COLOR_BLACK)
        (with-throw-handler #t
                            (λ ()
                              (let ((xdoui (make-xdoui xdo stdscr)))
                                (main-loop xdoui))
                              (timeout! stdscr 700)
                              (getch stdscr)
                              (endwin))
                            (λ (key . parameters)
                              (endwin)
                              (throw key))))
      (begin
        (endwin)
        (display "No colors for this terminal!\n")))))
