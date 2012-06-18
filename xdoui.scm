#!/usr/bin/guile \
-e main -s
coding: utf-8
!#

(use-modules (ncurses curses))
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

(define-record-type command-type
                    (make-command command arguments register)
                    command?
                    (command   get-command)
                    (arguments get-arguments set-arguments!)
                    (register  get-register  set-register!))

(define-record-type register-value
                    (make-register-value)
                    register-value?
                    (window   get-window   set-window!)
                    (position get-position set-position!))

(setlocale LC_ALL "en_US.UTF-8")

(define* (update-register registers key #:key window position)
  (let ((register (let ((v (vhash-assv key registers)))
                    (if v (cdr v) (make-register-value)))))
    (if window
      (set-window! register window))
    (if position 
      (set-position! register position))
    (vhash-consv key register (vhash-delv key registers))))

(define (change-register scr xdo registers store-register load-register arguments)
  (let ((ch (getch scr)))
    (if (and (char? ch) (char-alphabetic? ch))
      (if (eq? (car arguments) 'store) 
        `(set-store-register ,ch ,(car arguments))
        `(set-load-register ,ch ,(car arguments)))
      `(abort ,(list ch) "Not a valid register, use only alphabetic characters"))))

(define key-bindings
  `((#\" change-register store) 
    (#\' change-register load) 
    (#\m (#\p get-mouse-position))
    (#\w (#\p get-window-position)
         (#\g (#\a get-active-window)))
    (#\p print-register)
    (#\q quit "Exited by user"))) 

(define (deref-symbol sym)
  (catch #t 
    (λ () (module-ref (current-module) sym))
    (λ (key . args) #f)))

(define* (do-command scr xdo registers parse-tree #:optional input-history) 
  (let cmd-loop ((load-register #f)
                 (store-register #f))
    (match parse-tree
      ((((? char? char) . _) . _)
       (let parse-loop ((ch (getch scr)))
         (let ((history (if input-history (append input-history (list ch)) (list ch))))
           (if (char? ch)
             (let ((branch (assq ch parse-tree)))
               (if branch
                 (do-command scr xdo registers (cdr branch) history)
                 `(abort ,history "Not a recognized character")))
             `(abort ,history "Aborted")))))
      (('quit reason) `(quit ,reason))
      (((? symbol? symbol) . rest)
       (catch #t
         (λ ()
           (let ((ƒ (deref-symbol symbol)))
             (if ƒ
               (let ((result (ƒ scr xdo registers store-register load-register rest)))
                 (addstr scr (format #f "Result (~S): ~S" symbol result))    
                 (match result
                   (('set-load-register reg args) (cmd-loop reg store-register))
                   (('set-store-register reg args) (cmd-loop load-register reg))
                   (_ #f))))))
         (λ (key . args)
           (addchstr scr (bold (format #f "Failed: ~S: ~S" key args))))))
      (_ (addstr scr (format #f "Unknown: ~S" parse-tree))))))

(define (main-loop2 scr xdo)
  (let loop ((registers '()))
    (let ((result (do-command scr xdo registers key-bindings)))
      (if (not (match result
                 (('quit reason)
                  (addstr scr (format #f "Quit: ~a" reason)))
                 (_ #f)))
        (begin
          (match result
            (('abort history) history)
            (('abort history reason) (addchstr scr (bold (format #f "Abort: ~a" reason))))
            (_ #f)) 
          (loop registers))))))

(define (main-loop scr xdo initial-state)
  (let loop ((state initial-state)
             (registers vlist-null))
    (refresh scr)
    (let ((result
            (let parse-loop ((ch (getch scr))
                             (store-register #f)
                             (load-register #f))
              (match ch
                (#\" (let ((pch (cons ch '()))
                           (ch (getch scr)))
                       (if (and (char? ch) (char-alphabetic? ch))
                         (parse-loop (getch scr) store-register ch)
                         `(abort ,(cons ch pch)))))
                (#\' (let ((pch (cons ch '()))
                           (ch (getch scr)))
                       (if (and (char? ch) (char-alphabetic? ch))
                         (parse-loop (getch scr) ch load-register)
                         `(abort ,(cons ch pch)))))
                (#\m (let ((pch (cons ch '()))
                           (ch (getch scr)))
                       (match ch
                         (#\p (let ((result (xdo-get-mouse-location xdo)))
                                (if store-register
                                  (set! registers (update-register registers store-register #:position result)))
                                result))
                         (_ `(abort ,(cons ch pch))))))
                (#\w (let ((pch (cons ch '()))
                           (ch (getch scr)))
                       (match ch 
                         (#\p (if (and load-register (vhash-assv load-register registers))
                                (let ((window (get-window (cdr (vhash-assv load-register registers)))))
                                  (if window
                                    (let ((result (xdo-get-window-location xdo #:window window)))
                                      (if store-register
                                        (set! registers (update-register registers store-register #:window result))) 
                                      result)
                                    `(abort ,(cons ch pch) "No window in register")))
                                (xdo-get-window-location xdo)))
                         (#\g (let ((pch (cons ch pch))
                                    (ch (getch scr)))
                                (match ch
                                  (#\a (let ((result (xdo-get-active-window xdo)))
                                         (if store-register
                                           (set! registers (update-register registers store-register #:window result)))
                                         result))
                                  (_ `(abort ,(cons ch pch))))))
                         (_ `(abort ,(cons ch pch))))))
                (#\p (begin
                       (let ((register (vhash-assv load-register registers)))
                         (if (pair? register)
                           (addstr scr (format #f "[~a: ~a]" (car register) (cdr register)))
                           (addstr scr "You have to specify register")))
                       `(printed)))
                (#\q '(quit "Let's end this!"))
                (_ `(abort ,(cons ch '())))))))
      (match result
        (('abort (character . rest) string) 
         (let ((reason (list-ref result 2)))
           (addchstr scr (bold reason))
           (loop state registers)))
        (('abort (character . rest))
         (begin
           (addstr scr (format #f "~a" result))
           (loop state registers)))
        (('quit string)
         (let ((reason (list-ref result 1)))
           (addchstr scr (bold (format #f "Quit: ~S" reason)))))
        (('printed . rest)
         (loop state registers))
        (_ (addstr scr (format #f "{ ~S }" result))
           (loop state registers))))))

(define (main args)
  (let ((stdscr (initscr))
        (xdo (new-xdo)))
    (raw!)
    (keypad! stdscr #t)
    (noecho!)
    (if (has-colors?)
      (begin
        (start-color!)
        (init-pair! 1 COLOR_GREEN COLOR_BLACK)
        (with-throw-handler #t
                            (λ ()
                              (main-loop2 stdscr xdo)
                              ;(main-loop stdscr xdo '())
                              (getch stdscr)
                              (endwin))
                            (λ (key . parameters)
                              (endwin)
                              (throw key))))
      (begin
        (endwin)
        (display "No colors for this terminal!\n")))))
