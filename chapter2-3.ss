; Chapter 2 : abstract data
; Instance 3 : define of SET

; load the global variables 
(load "f://cp//scheme//global-variables.ss")

; define memq, find an item in a list. if a is in list, renturn the rest item after first appear index of the list, else return false
(define memq?! (lambda (item lst) 
	       (cond ((null? lst) false) 
		     ((equal? item (car lst)) lst) 
		     (else (memq item (cdr lst))))))