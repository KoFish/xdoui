#!/usr/bin/guile \
-e main -s
coding: utf-8
!#

;; xdoui.scm 

;; Copyright 2012 Krister Svanlund <krister dot svanlund at gmail dot com> 

;; This file is part of XDO UI

;; xdoui is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3
;; of the License, or (at your option) any later version.

;; xdoui is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A ncurses based UI for libxdo, provided by the xdotool
;; project.
;;
;;; Code:

(add-to-load-path (dirname (current-filename)))

(use-modules (ncurses curses)
             (oop goops)) 

(use-modules (ice-9 i18n)
             (ice-9 match)
             (ice-9 format)
             (ice-9 rdelim)
             (ice-9 popen)
             (ice-9 vlist)
             (ice-9 optargs))

(use-modules ((srfi srfi-1)
              #:select ((take . srfi-take) fold map))
             (srfi srfi-9)
             (srfi srfi-9 gnu)
             (srfi srfi-11)
             (srfi srfi-88))

(use-modules (xdo libxdo))
(use-modules (helpers))

(setlocale LC_ALL "en_US.UTF-8")

;;
;; Key bindings for the interface
;;

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
    (#\m (#\g (#\p get-mouse-position)
              (#\w get-window at-mouse))
         (#\p put-mouse-position)
         (#\c click-mouse))
    (#\w (#\p put-window-position)
         (#\g (#\a get-window active)
              (#\p get-window-position)
              (#\n get-window-name))
         (timeout 700 get-window focused))
    (#\q quit "Exited by user"))) 

;;
;; Utility functions
;;

(define (register->string register) 
  (match register
    (((? number? x) . (? number? y)) (format #f "[X ~a | Y ~a]" x y))
    (((? xdo-window? window) . title) (format #f "[Win: ~a]" (or title window)))
    ((? string? str) (format #f "[String: ~s]" str))
    (e (format #f "[Unknown: ~a]" e)))) 

(define (abort history . reason)
  (throw 'abort history (if (> (length reason) 0) (car reason) "Aborted")))

;;
;; Class definitions of <xdoui>
;;

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
    (redraw-xdoui! xdoui)
    xdoui))

;;
;; Methods on <xdoui> that deals with the prompt window
;;

; flashes is a alist of name and a timeout, each second the timeout
; is ticked down until it reaches zero, when it does it's removed 
; from the list. It's used to show information that isn't stricly
; necessary but that can be of interest, especially during debugging.
;
; A flashes alist can look like this:
; (("test" . 5) ("debug" . 3) ("printing" . 6))

(define-method (set-prompt (xdoui <xdoui>) (str <string>) . args)
  "Set the text of the prompt-window at the bottom of the screen."
  (set-prompt! xdoui (apply format #f str args))
  (draw-prompt xdoui))

(define-method (add-prompt-flash (xdoui <xdoui>) timeout (str <string>) . args)
  (set! (flashes xdoui)
    (assv-set! (flashes xdoui) (apply format #f str args) timeout))
  (draw-prompt xdoui))

(define-method (tick-flashes (xdoui <xdoui>))
  (set! (flashes xdoui) (filter (λ (e) (positive? (cdr e))) 
                                (map (λ (e) (cons (car e) (1- (cdr e)))) 
                                     (flashes xdoui)))))

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

;;
;; Methods on <xdoui> that deals with printing and ouputting
;; things to windows.
;;

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

;;
;; Functions for reading characters.
;;

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
                  (redraw-xdoui! xdoui)
                  (timeout! (get-screen xdoui) -1)
                  ch)))))))

;;
;; Redraws the screen and all the sidebar windows according the
;; the current size of the terminal.
;;

(define-method (redraw-xdoui! (xdoui <xdoui>))
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
    (redraw-sidebar! xdoui)
    (draw-prompt xdoui)
    (hline scr (acs-hline) (- mx width) #:y (- my 3) #:x 0)  
    (erase (main-window xdoui))
    (refresh (main-window xdoui))
    (refresh (prompt-window xdoui))))

(define-method (redraw-sidebar! (xdoui <xdoui>))
  "Recreates all the sidebar windows in proper new size accord to the size of the
  terminal."
  (if (not (null? (sidebar-windows xdoui))) 
      (let* ((win-count (length (sidebar-windows xdoui)))
             (my (getmaxy (get-screen xdoui))) 
             (mx (getmaxx (get-screen xdoui)))
             (whs (divide-array (- my 3) win-count))
             (whsa (reverse (fold (λ (e a) (cons (+ e (car a)) a)) (list 0) whs)))
             (w (right-width xdoui)))
        (let-values (((wh q) (floor/ (- my 3) win-count))) 
          (if (not (null? (sidebar-windows xdoui)))
              (begin
                (set! (sidebar-windows xdoui) 
                  ;; Destroy the old windows and create new ones to replace 
                  ;; them.
                  (map (λ (e wh y)
                          (cond ((cdr e) 
                                 => (λ (win) (destroy-win (cdr win))
                                             (destroy-win (car win)))))
                          (let* ((wino (subwin (get-screen xdoui) (1+ wh) w y (- mx w))) 
                                 (wini (derwin wino (- wh 2) (- w 2) 1 1))) 
                            (scrollok! wini #t) 
                            (erase wino)
                            (refresh wini) 
                            (cons (car e) (cons wino wini)))) 
                       (sidebar-windows xdoui) whs whsa)) 
                ;; Draw frames around each windows and write the title over the
                ;; top border.
                (let* ((e (car (sidebar-windows xdoui))) 
                       (wo (cadr e)))
                  (box wo (acs-vline) (acs-hline))
                  (addstr wo (string-capitalize (symbol->string (car e))) #:x 2 #:y 0) 
                  (refresh wo))  
                (map (λ (e) 
                        (let ((wo (cadr e)))
                          (box* wo)
                          (addstr wo (string-capitalize (symbol->string (car e))) #:x 2 #:y 0)
                          (refresh wo)))
                     (cdr (sidebar-windows xdoui)))))))
      (let* ((scr (get-screen xdoui))
             (my (getmaxy scr))
             (mx (getmaxx scr))
             (w (right-width xdoui)))
        (let ((win (subwin scr (- my 2) w 0 (- mx w))))
          (box win (acs-vline) (acs-hline))
          (refresh win)
          (delwin win)))))

;;
;; Functions for manipulating the sidebar.
;;

(define-method (add-to-sidebar! (xdoui <xdoui>) name)
  (cond ((assv name (sidebar-windows xdoui)) #f)
        (else (set! (sidebar-windows xdoui) (assv-set! (sidebar-windows xdoui) name #f)))) 
  (redraw-sidebar! xdoui))

(define-method (remove-from-sidebar! (xdoui <xdoui>) name)
  (cond ((assv name (sidebar-windows xdoui))
         => (λ (win)
               (let ((wini (cddr win))
                     (wino (cadr win)))
                 (set! (sidebar-windows xdoui) (assv-remove! (sidebar-windows xdoui) name))
                 (destroy-win wini) ; It's important to delete the windows in the right
                 (destroy-win wino) ; order, inner first and then outer.
                 (redraw-sidebar! xdoui))))
        (else #f)))

(define-method (get-sidebar-window (xdoui <xdoui>) name)
  "Get a sidebar window by it's name."
  (cond ((assv name (sidebar-windows xdoui))
         => (λ (v) (cdr v)))
        (else #f)))

(define-method (get-sidebar-window! (xdoui <xdoui>) name)
  "Same as get-sidebar-window but adds a window if there isn't 
  anyone with that name specfied."
  (cond ((get-sidebar-window xdoui name) => (λ (w) (cdr w)))
        (else (add-to-sidebar! xdoui name)
              (get-sidebar-window! xdoui name))))

;;
;; Class definitions and methods for dealing with
;; read-while-prompts.
;;
;; A read-while-prompt is a prompt that is created with a
;; predicate that decides what characters to be read by it.
;;

(define-class <xdoui-dialog-prompt> ()
  (xdoui #:getter get-xdoui #:init-keyword #:xdoui)
  (prompt #:getter get-prompt #:init-keyword #:prompt)
  (win #:accessor window)
  (swin #:accessor subwindow)
  (input #:accessor input-row))

(define (make-dialog-prompt xdoui prompt)
  (let ((p (make <xdoui-dialog-prompt> #:xdoui xdoui #:prompt prompt))
        (myx (getmaxyx (get-screen xdoui))))
    (let* ((w (floor (/ (cadr myx) 2)))
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

(define-method (add-dialog-prompt-flash (p <xdoui-dialog-prompt>) timeout (str <string>) . args)
  (addstr (subwindow p) (apply format #f str args) #:x 1 #:y 1)
  (refresh (subwindow p)))

(define-method (update-dialog-prompt (p <xdoui-dialog-prompt>) acc)
  (let ((w (getmaxx (input-row p))))
    (addstr (subwindow p) (get-prompt p) #:x 0 #:y 0)
    (hline (input-row p) (bold #\space)  w #:x 0 #:y 0) 
    (touchwin (window p))
    (refresh (window p)) 
    (addstr (input-row p) (list->string (reverse (take acc w))) #:x 0 #:y 0)
    (refresh (input-row p))
    p))

(define (read-while-dialog xdoui p? prompt-string)
  (let reader ((prompt (make-dialog-prompt xdoui prompt-string))
               (acc '()))
    (with-throw-handler 'abort
      (λ ()
         (let ((p (λ (acc) (update-dialog-prompt prompt acc)))
               (p-e (λ (str . args)
                       (apply add-dialog-prompt-flash prompt 5 str args)))) 
           (catch 'screen-resize
             (λ ()
                (let ((acc (apply read-while-generic xdoui p? p p-e acc)))
                  (destroy-dialog-prompt prompt)
                  (if (not (null? acc))
                      (begin
                        (if (not (eqv? (car acc) #\newline)) 
                            (ungetch (car acc)))
                        (cdr acc)) 
                      '())))
             (λ (key acc)
                (destroy-dialog-prompt prompt)
                (reader (make-dialog-prompt xdoui prompt-string) acc)))))
      (λ (key . args)
         (destroy-dialog-prompt prompt)))))

(define (read-while-prompt xdoui p? prompt)
  (let ((p (λ (acc)
              (set-prompt xdoui "~a: ~{~a~}" prompt (map quote-chars (reverse acc)))))
        (p-e (λ (str . args)
                (apply add-prompt-flash xdoui 5 str args)))) 
    (let reader ((acc '()))
      (catch 'screen-resize
        (λ ()
           (let ((acc (apply read-while-generic xdoui p? p p-e acc)))
             (if (not (null? acc)) 
                 (begin (ungetch (car acc)) (cdr acc))
                 '())))
        (λ (key acc)
           (reader acc))))))

(define (read-while-generic xdoui p? p p-e . start-acc)
  (let ((backspace? (λ (c) (or (eqv? c KEY_BACKSPACE) (eqv? c #\backspace))))
        (escape?    (λ (c) (or (eqv? c #\esc) (eqv? c #\x03))))
        (finish?    (λ (c) (or (eqv? c #\x04))))
        (no-key?    (λ (c) (and (not (char? c)) (> c 255)))))
    (p start-acc)
    (let read-stuff ((acc start-acc) 
                     (ch (get-next-char xdoui #f)))
      (match ch
        (#f (throw 'screen-resize acc))
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

(define-method (destroy-dialog-prompt (p <xdoui-dialog-prompt>))
  (delwin (input-row p))
  (delwin (subwindow p))
  (destroy-win (window p))
  (touchwin (get-screen (get-xdoui p)))
  (refresh (get-screen (get-xdoui p))))

;;
;; Utility functions for the command functions
;;

(define (add-to-register reg val history)
  (values `(add-to-register ,reg ,val)
          history))

(define (value val history)
  (values `(value ,val) history))

(define (get-value state name)
  (cond ((vhash-assv name state)
         => (λ (s) (cdr s)))
        (else #f)))

;;
;; Defines all the command functions
;;

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

(define (get-window-position xdoui registers state history . rest)
  (let ((sreg (get-value state 'store))
        (lreg (get-value state 'load)))
    (let ((w (cond ((get-value registers lreg)
                    => (λ (v) 
                          (match v
                            (((? xdo-window? w) . _) w)
                            (((? number?) . (? number?))
                             (debug-print xdoui "Position ~a in load reg\n" v)
                             (cond ((xdo-get-mouse-location (get-xdo xdoui))
                                    => (λ (old-pos) 
                                          (xdo-move-mouse (get-xdo xdoui) v)
                                          (let ((w (xdo-get-window-at-mouse (get-xdo xdoui))))
                                            (xdo-move-mouse (get-xdo xdoui) old-pos)
                                            w))) 
                                   (else 
                                     (debug-print xdoui "Could not get mouse position\n")
                                     #f)))
                            (else 
                              (debug-print xdoui "Not a useful value in register\n")
                              #f))))
                   (else (if lreg (abort history "The register does not contain a window.") #f)))))
      (cond ((xdo-get-window-location (get-xdo xdoui) #:window w)
             => (λ (pos) 
                   (if sreg
                       (add-to-register sreg pos history)
                       (value pos history))))))))

(define (get-mouse-position xdoui registers state history . rest)
  "Get the current mouse position, either print to main-window or
  store in specified register."
  (let ((sreg (get-value state 'store)))
    (cond ((xdo-get-mouse-location (get-xdo xdoui))
           => (λ (pos)
                 (if sreg
                     (add-to-register sreg pos history)
                     (value pos history))))
          (else (abort history "Could not get mouse position")))))

(define (get-window xdoui registers state history selector . args)
  (let ((sreg (get-value state 'store))
        (ƒ (match selector
                  ('active xdo-get-active-window)
                  ('focused xdo-get-focused-window)
                  ('at-mouse xdo-get-window-at-mouse)
                  (else #f))))
    (if ƒ (cond 
             ((ƒ (get-xdo xdoui))
              => (λ (w)
                    (if (xdo-window? w)
                        (let* ((w-title (xdo-get-window-name (get-xdo xdoui) #:window w))
                               (title (if w-title (car w-title) #f))) 
                          (if sreg
                              (add-to-register sreg (cons w title) history)
                              (value (cons w title) history)))
                        (abort history "Did not get a window"))))
             (else 
               (abort history "Failed to get any window")))
        (abort history "No such function"))))

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

;;;                                                                        ;;;
;;                                                                          ;;
;; What follows here is the hardcore loops and such that runs the whole app ;;
;;                                                                          ;;
;;;                                                                        ;;;

(define (print-all-registers xdoui registers)
  (cond ((get-sidebar-window! xdoui 'registers)
         => (λ (scr)
               (let ((x (getcurx scr))
                     (mx (getmaxx scr)))
                 (vhash-fold (λ (key value count)
                                (hline scr (normal #\Space) mx #:x 0 #:y count)
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
                             => (λ (p) (debug-print xdoui "Prefix: ~a~%" (cdr p)))))
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
                          (('value ?value) (print xdoui #f "Result: ~a~%" (register->string ?value)))
                          (_ result)))))
                 (λ (key . args) #f)))))
      (match branch 
        ((((? char?) . _) . _)
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
                                   (string-join (map string-trim-both (string-split help-text #\newline)) " ")))))
               (begin
                 (add-prompt-flash xdoui 3 "~S" ?symbol)
                 (apply call-proc ƒ ?args)))))
        (('quit ?reason) `(quit ,?reason))
        (((? symbol? ?symbol) . args)
         (cond ((false-if-exception (module-ref (current-module) ?symbol))
                => (λ (ƒ) (cmd-loop state history (cons ƒ branch))))
               (else (abort history (format #f "No such function: (~S)" ?symbol)))))
        (_ (debug-print xdoui "Unknown: ~S\n" branch))))))

(define (main-loop xdoui)
  (redraw-xdoui! xdoui)
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
                 (for-each (λ (e) (if (cdr e) (refresh (cddr e))))
                           (sidebar-windows xdoui))
                 (match result
                   (('add-to-register ?key ?value)
                    (let ((new-registers (vhash-consv ?key ?value (vhash-delv ?key registers))))
                      (print-all-registers xdoui new-registers)
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
