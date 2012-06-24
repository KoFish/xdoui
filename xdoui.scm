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
             (srfi srfi-88))

(use-modules (xdo libxdo))

(setlocale LC_ALL "en_US.UTF-8")

(define-syntax aif
  (syntax-rules ()
    ((_ name test 
        true-branch 
        false-branch ...)
     (let ((name test))
       (if name
           true-branch
           false-branch ...)))
    ((_ name pred test
        true-branch
        false-branch ...)
     (let ((name test))
       (if (pred name)
           true-branch
           false-branch ...)))))

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
  (xdo #:getter get-xdo #:init-keyword #:xdo) 
  (scr #:getter get-screen #:init-keyword #:scr) 
  (main-window #:accessor main-window #:init-keyword #:main-window) 
  (right-width #:accessor right-width #:init-keyword #:right) 
  (extra-windows #:accessor extra-windows #:init-value '()) 
  (bottom-window #:accessor bottom-window #:init-keyword #:bottom))  

(define (make-xdoui xdo scr)
  (let* ((my (getmaxy scr))
         (mx (getmaxx scr))
         (right-width (min (floor (/ mx 2)) 50))
         (main-window (newwin (- my 3) (- mx right-width) 0 0))
         (bottom-window (newwin 3 (- mx right-width) (- my 3) 0)))
    (scrollok! main-window #t)
    (make <xdoui> #:xdo xdo #:scr scr #:main-window main-window 
          #:right right-width #:bottom bottom-window)))

(define-generic print)

(define-method (print (xdoui <xdoui>) window (str <string>))
  (let ((win (or (aif w (assv window (extra-windows xdoui)) (cddr w) #f)
                 (main-window xdoui))))
    (addstr win str)
    (refresh win)))

(define-method (print (xdoui <xdoui>) window (str <string>) a . args)
  (print xdoui window (apply format (cons* #f str (cons a args)))))

(define-method (add-subwindow! (xdoui <xdoui>) name)
  (cond ((assv name (extra-windows xdoui)) #f)
        (else 
          (set! (extra-windows xdoui) (assv-set! (extra-windows xdoui) name #f)))) 
  (resize-subwindows! xdoui))

(define-method (resize-xdoui! (xdoui <xdoui>))
  (let*  ((scr (get-screen xdoui))
          (my (getmaxy scr)) 
          (mx (getmaxx scr)) 
          (width (min (floor (/ mx 2)) 50)) 
          (new-window (newwin (- my 3) (- mx width) 0 0)))
    (scrollok! new-window #t)
    (delwin (main-window xdoui))
    (set! (main-window xdoui) new-window)
    (set! (right-width xdoui) width)
    (resize-subwindows! xdoui)))

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
    (cond
      ((> win-count 0) 
       (let* ((my (getmaxy (get-screen xdoui))) 
              (mx (getmaxx (get-screen xdoui))) 
              (window-height (floor (/ (1- my) win-count)))
              (s (normal #\sp)))
         (print xdoui #f "Window-height: ~s/~s = ~s~%" (1- my) win-count window-height)
         (set! (extra-windows xdoui) 
           (map (λ (e c)
                   (let ((k (car e))) 
                     (if (cdr e) 
                         (let ((wo (cadr e)) 
                               (wi (cddr e))) 
                           (destroy-win wi) 
                           (destroy-win wo))) 
                     (let* ((h (if (eq? c win-count) 5 window-height))
                            (w (right-width xdoui))
                            (x (* h c)) (y (- mx w))
                            (wino (newwin (1+ h) w x y))
                            (wini (derwin wino (- h 1) (- w 2) 1 1)))
                       (scrollok! wini #t)
                       (refresh wini)
                       (cons k (cons wino wini))) )) 
                (extra-windows xdoui)
                (iota win-count)))
         (map (λ (e p) (let ((n (car e))
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
              (cons #f (cdr (iota win-count)))))))))

(define-method (remove-subwindow! (xdoui <xdoui>) name)
  (aif win (assv name (extra-windows xdoui))
       (let ((wini (cddr win))
             (wino (cadr win)))
         (set! (extra-windows xdoui) (assv-remove! (extra-windows xdoui) name))
         (destroy-win wini)
         (destroy-win wino)
         (resize-subwindows! xdoui))
       #f))

(define-method (get-subwindows (xdoui <xdoui>) name)
  (aif v (assv name (extra-windows xdoui))
       (cdr v) #f))

(define-method (get-subwindow (xdoui <xdoui>) name)
  (cond ((get-subwindows xdoui name) => (λ (w) (cdr w)))
        (else (add-subwindow! xdoui name)
              (get-subwindow xdoui name))))

(define* (update-register registers key #:key window position)
  (vhash-consv key (aif v (vhash-assv key registers)
                        (let ((reg (cdr v)))
                          (if window (set-window! reg window))
                          (if position (set-position! reg position))
                          reg)
                        (make-register-value window position))
               (vhash-delv key registers)))

(define (change-register xdoui registers state type)
  (let ((ch (get-next-char xdoui)))
    (if (and (char? ch) (char-alphabetic? ch))
      `(set-register ,type ,ch)
      (throw 'abort (list ch) "Not a valid register, use only alphabetic characters"))))

(define (get-mouse-position xdoui registers state . rest)
  (aif result (xdo-get-mouse-location (get-xdo xdoui))
       (aif value (vhash-assv 'store state)
            `(new-registers ,(update-register registers (cdr value) #:position result))
            `(value ,result))))

(define (print-register xdoui registers state . rest)
  (aif reg (or (vhash-assv 'load state) (vhash-assv 'store state))
       (aif cnt (vhash-assv (cdr reg) registers)
            (print xdoui #f "Register ~c: ~s\n" (car cnt) (cdr cnt))
            (throw 'abort '() (format #f "No content in register \"~c\"" (cdr reg))))
       (throw 'abort '() "No register specified")))

(define key-bindings
  `((#\" change-register store) 
    (#\' change-register load) 
    (#\t (#\p ,(λ (xdoui regs state . rest)
                  (debug-print xdoui "Debug!!\n")
                  (print xdoui 'reg "Reg!!\n")
                  (print xdoui #f "Main!!\n")))
         (#\a ,(λ (xdoui regs state . rest)
                  (add-subwindow! xdoui 'test)
                  (print xdoui 'test "Test window!!\n")))
         (#\d ,(λ (xdoui regs state . rest)
                  (remove-subwindow! xdoui 'test))))
    (#\m (#\p get-mouse-position))
    (#\w (#\p get-window-position)
         (#\g (#\a get-active-window)))
    (#\p print-register)
    (#\q quit "Exited by user"))) 

(define (deref-symbol sym)
  (false-if-exception (module-ref (current-module) sym)))

(define (print-all-registers xdoui registers)
  (aif scr (get-subwindow xdoui 'reg)
    (let ((x (getcurx scr))
          (mx (getmaxx scr)))
      (vhash-fold (λ (key value count)
                     (addstr scr (format #f "Reg ~s: ~s" key value) 
                             #:x 0 #:y count #:n mx)
                     (+ 1 count)) 
                  0 registers)
      (refresh scr))
    (let ((myx (getmaxyx (get-screen xdoui))))
      (add-subwindow! xdoui 'reg)
      (print-all-registers xdoui registers))))

(define* (debug-print xdoui str . arguments)
  (aif win (get-subwindow xdoui 'debug)
       (apply print (cons* xdoui 'debug str arguments))
       (begin
         (add-subwindow! xdoui 'debug)
         (apply debug-print (cons* xdoui str arguments)))))

(define* (do-command xdoui registers parse-tree)
  (let cmd-loop ((state vlist-null) 
                 (input-history '())
                 (branch parse-tree))
    (let ((call-proc 
            (λ (ƒ . args)
               (catch 'abort
                 (λ ()
                    (let ((result (apply ƒ (cons* xdoui registers state args))))
                      (if (not (boolean? result))
                          (debug-print xdoui "Result: ~S~%" result))
                      (match result
                        (('set-register type reg . _)
                         (cmd-loop (vhash-consq type reg state) input-history parse-tree))
                        (('new-registers regs) 
                         `(update-registers ,regs))
                        (('value value) (print xdoui #f "Result: ~a~%" value))
                        (_ result))
                      )
                    )
                 (λ (abort history reason)
                    (throw 'abort (append history input-history) reason))))))
      (match branch 
        ((((? char? char) . _) . _)
         (print-all-registers xdoui registers)
         (let parse-loop ((ch (get-next-char xdoui)))
           (let ((history (if input-history (append input-history (list ch)) (list ch))))
             (if (char? ch)
                 (let ((new-branch (assq ch branch)))
                   (if new-branch
                       (cmd-loop state history (cdr new-branch))
                       (throw 'abort history (format #f "~c is not a recognized character" ch))))
                 (throw 'abort history "Aborted")))))
        (((? procedure? ƒ) . args)
         (apply call-proc (cons* ƒ args)))
        (('quit reason) `(quit ,reason))
        (((? symbol? symbol) . args)
         (let ((ƒ (deref-symbol symbol)))
           (if ƒ
               (apply call-proc (cons* ƒ args))
               (throw 'abort input-history (format #f "No such function (~S)" symbol)))))
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
                 (for-each (λ (e) (if (cdr e) (refresh (cddr e)))) (extra-windows xdoui))
                 (match result
                   (('update-registers (? vlist? new-registers))
                    (loop new-registers))
                   (_
                     (loop registers)))))))
      (λ (key history reason)
         (print xdoui #f "Abort: ~a [~{~a~}]\n" reason history)
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
