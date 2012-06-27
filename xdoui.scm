#!/usr/bin/guile \
-e main -s
coding: utf-8
!#

(use-modules (ncurses curses)
             (oop goops)) 
(use-modules (ice-9 match)
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

(define-record-type command-type
                    (make-command command arguments register)
                    command?
                    (command   get-command)
                    (arguments get-arguments set-arguments!)
                    (register  get-register  set-register!))

(define-record-type register-value
                    (make-register-value window position)
                    register-value?
                    (window   get-window   set-window!)
                    (position get-position set-position!))

(set-record-type-printer!
  register-value
  (λ (reg port)
     (let ((p (get-position reg))
           (w (get-window reg)))
       (format port "[ ~@[Win:~S~]~:[~; ~]~@[Pos:~S~] ]" w (and w p) p))))

(define-class <xdoui> ()
  (xdo           #:getter   get-xdo       #:init-keyword #:xdo) 
  (scr           #:getter   get-screen    #:init-keyword #:scr) 
  (right-width   #:accessor right-width   #:init-value   0)  
  (extra-windows #:accessor extra-windows #:init-value   '()) 
  (main-window   #:accessor main-window   #:init-value   #f)  
  (prompt-window #:accessor prompt-window #:init-value   #f))

(define (make-xdoui xdo scr)
  (let ((xdoui (make <xdoui> #:xdo xdo #:scr scr)))
    (resize-xdoui! xdoui)
    xdoui))

(define-method (set-prompt (xdoui <xdoui>) (str <string>) . args)
  (let ((win (prompt-window xdoui)))
    (erase win)
    (addstr win (apply format #f str args))
    (refresh win)))

(define-method (prompt-flash (xdoui <xdoui>) (str <string>) . args)
  (let* ((win (prompt-window xdoui))
         (mx (getmaxx win))
         (str (apply format #f str args))
         (len (string-length str)))
    (addstr win str #:y 0 #:x (- mx len))
    (refresh win)))

(define-method (print (xdoui <xdoui>) window (str <string>))
  (let ((win (cond ((assv window (extra-windows xdoui))
                    => (λ (w) (cddr w)))
                   (else (main-window xdoui)))))
    (addstr win str)
    (refresh win)))

(define-method (print (xdoui <xdoui>) window (str <string>) a . args)
  (print xdoui window (apply format #f str a args)))

(define-method (add-subwindow! (xdoui <xdoui>) name)
  (cond ((assv name (extra-windows xdoui)) #f)
        (else 
          (set! (extra-windows xdoui) (assv-set! (extra-windows xdoui) name #f)))) 
  (resize-subwindows! xdoui))

(define-method (resize-xdoui! (xdoui <xdoui>))
  (let*  ((scr (get-screen xdoui))
          (my (getmaxy scr)) 
          (mx (getmaxx scr)) 
          (width (min (floor (/ mx 2)) 50)) )
    (if (main-window xdoui) (delwin (main-window xdoui)))
    (if (prompt-window xdoui) (delwin (prompt-window xdoui)))
    (set! (main-window xdoui) (newwin (- my 3) (- mx width) 0 0))
    (set! (prompt-window xdoui) (newwin 1 (- mx width) (- my 2) 0))
    (set! (right-width xdoui) width)
    (scrollok! (main-window xdoui) #t)
    (resize-subwindows! xdoui)
    (hline scr (acs-hline) (- mx width) #:y (- my 3) #:x 0)  
    (refresh (main-window xdoui))
    (refresh (prompt-window xdoui))))

(define-method (get-next-char (xdoui <xdoui>))
  (let ((ch (getch (get-screen xdoui))))
    (cond ((eqv? ch KEY_RESIZE) (resize-xdoui! xdoui) ch)
          (else ch))))

(define (destroy-win win)
  (let ((s (normal #\sp)))
    (border win s s s s s s s s)
    (refresh win)
    (delwin win)))

(define-method (resize-subwindows! (xdoui <xdoui>))
  (let ((win-count (length (extra-windows xdoui)))) 
    (if (positive? win-count) 
        (let* ((my (getmaxy (get-screen xdoui))) 
               (mx (getmaxx (get-screen xdoui))) )
          (let-values (((wh q) (floor/ (1- my) win-count)))
                      (set! (extra-windows xdoui) 
                        (map (λ (e c)
                                (cond ((cdr e) 
                                       => (λ (w)
                                             (destroy-win (cdr w))
                                             (destroy-win (car w)))))
                                (let* ((h wh)  
                                       (w (right-width xdoui))
                                       (x (* h c)) (y (- mx w))) 
                                  (let* ((wino (newwin (1+ h) w x y)) 
                                         (wini (derwin wino (- h 2) (- w 2) 1 1))) 
                                    (scrollok! wini #t) 
                                    (refresh wini) 
                                    (cons (car e) (cons wino wini))))) 
                             (extra-windows xdoui)
                             (iota win-count))) 
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
                           (extra-windows xdoui)
                           (cons #f (cdr (make-list win-count #t)))))))))

(define-method (remove-subwindow! (xdoui <xdoui>) name)
  (cond ((assv name (extra-windows xdoui))
         => (λ (win)
               (let ((wini (cddr win))
                     (wino (cadr win)))
                 (set! (extra-windows xdoui) (assv-remove! (extra-windows xdoui) name))
                 (destroy-win wini)
                 (destroy-win wino)
                 (resize-subwindows! xdoui))))
        (else #f)))

(define-method (get-subwindows (xdoui <xdoui>) name)
  (cond ((assv name (extra-windows xdoui))
         => (λ (v) (cdr v)))
        (else #f)))

(define-method (get-subwindow (xdoui <xdoui>) name)
  (cond ((get-subwindows xdoui name) => (λ (w) (cdr w)))
        (else (add-subwindow! xdoui name)
              (get-subwindow xdoui name))))

(define* (update-register registers key #:key window position)
  (vhash-consv key (cond ((vhash-assv key registers)
                          => (λ (v)
                                (let ((reg (cdr v)))
                                  (if window (set-window! reg window))
                                  (if position (set-position! reg position))
                                  reg))) 
                         (else (make-register-value window position)))
               (vhash-delv key registers)))

#|
 |(define-syntax define-action
 |  (syntax-rules (store load key)
 |    ((_ name (xdoui registers state rest ...)
 |        ((load load-reg) (store store-reg) (key ch))
 |        exp exp* ...)
 |     (define (name xdoui registers state rest ...)
 |       (let ((load-reg (if (identifier? load-reg) (cond ((vhash-assv 'load state) => (λ (e) (cdr e))))))
 |             (store-reg (if (identifier? store-reg) (cond ((vhash-assv 'store state) => (λ (e) (cdr e))))))
 |             (ch (if (identifier? ch) (get-next-char xdoui))))
 |         exp exp* ...)))))
 |#

#|
 |(define-action get-current-window (xdoui registers state history args)
 |               ((load #f) (store streg) (key #f))
 |               (debug-print xdoui "Stuff! ~s\n" streg))
 |#

(define (change-register xdoui registers state history type)
  (let ((ch (get-next-char xdoui)))
    (if (and (char? ch) (char-alphabetic? ch))
        (begin
          (values `(set-register ,type ,ch)
                  (cons ch history))) 
        (throw 'abort (cons ch history) "Not a valid register, use only alphabetic characters"))))

(define (show-help xdoui registers state history . rest)
  "Show the description of the action called with a particular
  keybinding."
  (values `(add-to-state ,(acons 'show-help #t '()))
          history))

(define (get-mouse-position xdoui registers state history . rest)
  "Get the current mouse position, either print to main-window or
  store in specified register."
  (cond ((xdo-get-mouse-location (get-xdo xdoui))
         => (λ (result)
               (cond ((vhash-assv 'store state)
                      => (λ (value)
                            (values `(new-registers ,(update-register registers (cdr value) #:position result))
                                    history)))
                     (else (values `(value ,result)
                                   history)))))
        (else 
          (throw 'abort history "Could not get mouse position"))))

(define (print-register xdoui registers state history . rest)
  (cond ((or (vhash-assv 'load state) (vhash-assv 'store state))
         => (λ (reg)
               (cond ((vhash-assv (cdr reg) registers)
                      => (λ (cnt)
                            (print xdoui #f "Register ~c: ~s\n" (car cnt) (cdr cnt))))
                     (else
                       (throw 'abort history (format #f "No content in register \"~c\"" (cdr reg)))))))
        (else
          (throw 'abort history "No register specified"))))

(define key-bindings
  `((#\" change-register store) 
    (#\' change-register load) 
    (#\h show-help)
    (#\t (#\p ,(λ (xdoui regs state . rest)
                  (debug-print xdoui "Debug!!\n")
                  (prompt-flash xdoui "Prompt!!" )
                  (print xdoui 'registers "Reg!!\n")
                  (print xdoui 'test "Test!!\n")
                  (print xdoui #f "Main!!\n")))
         (#\a ,(λ (xdoui regs state . rest)
                  (add-subwindow! xdoui 'test)
                  (print xdoui 'test "Test window!!\n")))
         (#\d ,(λ (xdoui regs state . rest)
                  (remove-subwindow! xdoui 'test))))
    (#\p print-register)
    (#\m (#\g (#\p get-mouse-position))
         (#\p put-mouse-position)
         (#\w get-mouse-window)
         (#\c click-mouse))
    (#\w (#\p get-window-position)
         (#\m move-window)
         (#\g (#\a get-active-window)))
    (#\q quit "Exited by user"))) 

(define (deref-symbol sym)
  (false-if-exception (module-ref (current-module) sym)))

(define (print-all-registers xdoui registers)
  (cond ((get-subwindow xdoui 'registers)
         => (λ (scr)
               (let ((x (getcurx scr))
                     (mx (getmaxx scr)))
                 (vhash-fold (λ (key value count)
                                (addstr scr (format #f "Reg ~s: ~s" key value) 
                                        #:x 0 #:y count #:n mx)
                                (+ 1 count)) 
                             0 registers)
                 (refresh scr)))) 
        (else (let ((myx (getmaxyx (get-screen xdoui))))
                (add-subwindow! xdoui 'registers)
                (print-all-registers xdoui registers)))))

(define* (debug-print xdoui str . arguments)
  (cond ((get-subwindow xdoui 'debug)
         => (λ (win)
               (apply print xdoui 'debug str arguments))) 
        (else (begin
                (add-subwindow! xdoui 'debug)
                (apply debug-print xdoui str arguments)))))

(define* (do-command xdoui registers parse-tree)
  (let cmd-loop ((state vlist-null) 
                 (history '())
                 (branch parse-tree))
    (let ((call-proc 
            (λ (ƒ . args)
               (catch 'abort
                 (λ ()
                    (let-values (((result . new-history) (apply ƒ xdoui registers state history args)))
                                (if (not (boolean? result))
                                    (debug-print xdoui "Result: ~S~%" result))
                                (let ((history (if (not (null? new-history)) 
                                                   (car new-history) 
                                                   history))) 
                                  (match result
                                    (('set-register type reg . _)
                                     (set-prompt xdoui "> ~{~a~^-~}" (reverse history)) 
                                     (cmd-loop (vhash-consq type reg state) history parse-tree))
                                    (('new-registers regs) 
                                     `(update-registers ,regs))
                                    (('add-to-state states)
                                     (let ((result (fold (λ (e r) (vhash-consv (car e) (cdr e) r)) state states)))
                                       (cmd-loop result history parse-tree)))
                                    (('value value) (print xdoui #f "Result: ~a~%" value))
                                    (_ result)))))
                 (λ (abort history reason)
                    (throw 'abort history reason))))))
      (match branch 
        ((((? char? char) . _) . _)
         (print-all-registers xdoui registers)
         (let parse-loop ((ch (get-next-char xdoui)))
           (let ((history (cons ch history)))
             (set-prompt xdoui "> ~{~a~^-~}" (reverse history))
             (if (char? ch)
                 (let ((new-branch (assq ch branch)))
                   (if new-branch
                       (cmd-loop state history (cdr new-branch))
                       (throw 'abort history (format #f "~c is not a recognized character" ch))))
                 (throw 'abort history "Aborted")))))
        (((? procedure? ƒ) . args)
         (apply call-proc ƒ args))
        (('quit reason) `(quit ,reason))
        (((? symbol? symbol) . args)
         (cond ((deref-symbol symbol) 
                => (λ (ƒ)
                      (let ((help? (vhash-assv 'show-help state)))
                        (if (and help? (cdr help?))
                            (cond ((procedure-documentation ƒ)
                                   => (λ (help-text)
                                   (print xdoui #f "~%Description of ~a:~%  ~a~%" symbol (string-join 
                                                                                           (map
                                                                                             string-trim-both 
                                                                                             (string-split help-text #\newline))
                                                                                           " ")))))
                            (begin 
                              (prompt-flash xdoui "[~S]" symbol)
                              (apply call-proc ƒ args))))))
               (else (throw 'abort history (format #f "No such function: (~S)" symbol)))))
        (_ (debug-print xdoui "Unknown: ~S\n" branch))))))

(define (main-loop xdoui)
  (resize-xdoui! xdoui)
  (refresh (get-screen xdoui))
  (let loop ((registers vlist-null))
    (catch 'abort
      (λ ()
         (let ((result (do-command xdoui registers key-bindings)))
           (if (not (match result
                      (('quit reason)
                       (print xdoui #f "Quit: ~a\n" reason))
                      (_ #f)))
               (begin
                 (for-each (λ (e) 
                              (if (cdr e) (refresh (cddr e))))
                           (extra-windows xdoui))
                 (match result
                   (('update-registers (? vlist? new-registers))
                    (loop new-registers))
                   (_
                     (loop registers)))))))
      (λ (key history reason)
         (print xdoui #f "Abort: ~a [~{~a~^-~}]\n" reason (reverse history))
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
                              (getch stdscr)
                              (endwin))
                            (λ (key . parameters)
                              (endwin)
                              (throw key))))
      (begin
        (endwin)
        (display "No colors for this terminal!\n")))))
