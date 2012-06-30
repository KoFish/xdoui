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

(define key-bindings
  `((#\" change-register store) 
    (#\' change-register load) 
    (#\h show-help)
    (#\t (#\p ,(λ (xdoui regs state . rest)
                  (debug-print xdoui "Debug!!\n")
                  (prompt-flash xdoui "Prompt!!" )
                  (print xdoui 'registers "Reg!!\n")
                  (print xdoui 'test "Test!!\n")
                  (print xdoui #f "Main!!\n")) test-print)
         (#\a ,(λ (xdoui regs state . rest)
                  (add-subwindow! xdoui 'test)
                  (print xdoui 'test "Test window!!\n")) test-add)
         (#\d ,(λ (xdoui regs state . rest)
                  (remove-subwindow! xdoui 'test)) test-delete))
    (#\p print-register)
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
  "Set the text of the prompt-window at the bottom of the screen."
  (let ((win (prompt-window xdoui)))
    (erase win)
    (addstr win (apply format #f str args))
    (refresh win)))

(define-method (prompt-flash (xdoui <xdoui>) (str <string>) . args)
  "Show a string to the right side of the prompt. This will most likely
  be erased by the next call to `set-prompt`."
  (let* ((win (prompt-window xdoui))
         (mx (getmaxx win))
         (str (apply format #f str args))
         (len (string-length str)))
    (addstr win str #:y 0 #:x (- mx len))
    (refresh win)))

(define-method (print (xdoui <xdoui>) window (str <string>))
  "Add a string to the window specified by the window argument. The window
  argument is either #f for the main window or a symbol specifing a sub
  window on the right hand side."
  (if window
      (cond ((assv-ref (extra-windows xdoui) window)
             => (λ (w) (let ((w (cdr w)))
                            (addstr w str)
                            (refresh w)))))
      (let ((win (main-window xdoui)))
        (addstr win str)
        (refresh win))))

(define-method (print (xdoui <xdoui>) window (str <string>) a . args)
  (print xdoui window (apply format #f str a args)))

(define* (debug-print xdoui str . arguments)
  (cond ((get-subwindow! xdoui 'debug)
         => (λ (win)
               (apply print xdoui 'debug str arguments))) 
        (else (begin
                (add-subwindow! xdoui 'debug)
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

(define (deref-symbol sym)
  (false-if-exception (module-ref (current-module) sym)))

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

(define-method (resize-xdoui! (xdoui <xdoui>))
  "Resize the whole screen to the current terminal size, it also clears the
  content of all the subwindows."
  (let*  ((scr (get-screen xdoui))
          (my (getmaxy scr)) 
          (mx (getmaxx scr)) 
          (width (min (floor (/ mx 2)) 50)) )
    (if (main-window xdoui) (delwin (main-window xdoui)))
    (if (prompt-window xdoui) (delwin (prompt-window xdoui)))
    (set! (main-window xdoui) (newwin (- my 3) (- mx width) 0 0))
    (set! (prompt-window xdoui) (newwin 1 mx (- my 2) 0))
    (set! (right-width xdoui) width)
    (scrollok! (main-window xdoui) #t)
    (resize-subwindows! xdoui)
    (hline scr (acs-hline) (- mx width) #:y (- my 3) #:x 0)  
    (refresh (main-window xdoui))
    (refresh (prompt-window xdoui))))

(define-method (resize-subwindows! (xdoui <xdoui>))
  "Recreates all the subwindows in proper new size accord to the size of the
  terminal."
  (let ((win-count (length (extra-windows xdoui)))) 
    (if (positive? win-count) 
        (let* ((my (getmaxy (get-screen xdoui))) 
               (mx (getmaxx (get-screen xdoui)))
               (whs (divide-array (- my 3) win-count))
               (whsa (reverse (fold (λ (e a) (cons (+ e (car a)) a)) (list 0) whs)))
               (w (right-width xdoui)))
          (let-values (((wh q) (floor/ (- my 3) win-count))) 
            (set! (extra-windows xdoui) 
              ;; Destroy the old subwindows and create new ones to replace 
              ;; them.
              (map (λ (e wh x)
                      (cond ((cdr e) 
                             => (λ (win)
                                   (destroy-win (cdr win))
                                   (destroy-win (car win)))))
                      (let* ((wino (newwin (1+ wh) w x (- mx w))) 
                             (wini (derwin wino (- wh 2) (- w 2) 1 1))) 
                        (scrollok! wini #t) 
                        (refresh wini) 
                        (cons (car e) (cons wino wini)))) 
                   (extra-windows xdoui)
                   whs
                   whsa)) 
            ;; Draw frames around each subwindow and write the title over the
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
                 (extra-windows xdoui)
                 (cons #f (cdr (make-list win-count #t)))))))))

(define-method (add-subwindow! (xdoui <xdoui>) name)
  (cond ((assv name (extra-windows xdoui)) #f)
        (else (set! (extra-windows xdoui) (assv-set! (extra-windows xdoui) name #f)))) 
  (resize-subwindows! xdoui))

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

(define-method (get-subwindow (xdoui <xdoui>) name)
  (cond ((assv name (extra-windows xdoui))
         => (λ (v) (cdr v)))
        (else #f)))

(define-method (get-subwindow! (xdoui <xdoui>) name)
  (cond ((get-subwindow xdoui name) => (λ (w) (cdr w)))
        (else (add-subwindow! xdoui name)
              (get-subwindow! xdoui name))))

(define* (update-register registers key value) 
  (vhash-consv key value (vhash-delv key registers)))

(define (register->string register) 
  (match register
    (((? number? x) . (? number? y)) (format #f "[X ~a | Y ~a]" x y))
    ((? window? window) (format #f "[Win: ~a]" window))
    (e (format #f "[Unknown: ~a]" e))))

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
                            (values `(new-registers ,(update-register registers (cdr value) 
                                                                      (cons (car result) (cadr result))))
                                    history)))
                     (else (values `(value ,result)
                                   history)))))
        (else 
          (throw 'abort history "Could not get mouse position"))))

(define (print-register xdoui registers state history . rest)
  "Print the content of the register specified in the load or store
  register."
  (cond ((or (vhash-assv 'load state) (vhash-assv 'store state))
         => (λ (reg)
               (cond ((vhash-assv (cdr reg) registers)
                      => (λ (cnt)
                            (print xdoui #f "Register ~c: ~a\n" (car cnt) (register->string (cdr cnt)))))
                     (else
                       (throw 'abort history (format #f "No content in register \"~c\"" (cdr reg)))))))
        (else
          (throw 'abort history "No register specified"))))

(define (print-all-registers xdoui registers)
  (cond ((get-subwindow! xdoui 'registers)
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
                (add-subwindow! xdoui 'registers)
                (print-all-registers xdoui registers)))))

(define* (do-command xdoui registers parse-tree)
  (let cmd-loop ((state vlist-null) 
                 (history '())
                 (branch parse-tree))
    (let ((call-proc 
            (λ (ƒ . args)
               (catch 'abort
                 (λ ()
                    (let-values 
                      (((result . new-history) (apply ƒ xdoui registers state history args)))
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
         (let ((timeout (assv-ref branch 'timeout)))
           (if timeout (debug-print xdoui "~s" (cdr timeout)))
           (let ((ch (get-next-char xdoui (if timeout (car timeout) #f))))
             (if ch
                 (let ((history (cons ch history)))
                   (set-prompt xdoui "> ~{~a~^-~}" (reverse history))
                   (if (char? ch)
                       (let ((new-branch (assq ch branch)))
                         (if new-branch
                             (cmd-loop state history (cdr new-branch))
                             (throw 'abort history (format #f "~c is not a recognized character" ch))))
                       (throw 'abort history "Aborted")))
                 (if timeout 
                     (begin
                       (cmd-loop state history (cdr timeout))))))))
        (((? procedure? ƒ) (? symbol? symbol) . args)
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
                 (prompt-flash xdoui "[~S]" symbol))))
         (apply call-proc ƒ args))
        (('quit reason) `(quit ,reason))
        (((? symbol? symbol) . args)
         (cond ((deref-symbol symbol) 
                => (λ (ƒ)
                      (cmd-loop state history (cons ƒ branch))))
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
