; the homework of the little scheme
;========
; preface 
;========
; to wok with scheme, you have to define atom?, add1, sub1
(define atom? (lambda (x) 
  (and (not (pair? x)) (not (null? x)))))


; chapter 1  ()
(define lat? (lambda (x)  
    (cond ((null? x) #t) 
	  ((atom? (car x)) (lat? (cdr x)))
	  (else #f))))

;chapter 2 (do (it again) (and again))
;analyze it angin and angin ...

;*********************************************************************************************
;chapter 3 
;rember stand for remove a member
(define rember (lambda (x list) 
		 (cond ((null? list) '())
		       ((eq? x (car list)) (cdr list))
		       (else (cons (car list) (rember x (cdr list)))))))
;firsts, get the first element of the each arguments cons to a list
(define firsts (lambda (l) 
		 (cond ((null? l) '())
		       (else (cons (cond ((atom? (car l)) (car l)) 
					 (else (car (car l)))) 
				   (firsts (cdr l)))))))
;insert the old element's right side of the list
(define insertR (lambda (new old list) 
  (cond ((null? list) '())
	((eq? (car list) old) (cons old (cons new (cdr list))))
	(else (cons (car list) (insertR new old (cdr list)))))))
;remove all members in a list
(define remberAll (lambda (x list) 
  (cond ((null? list) '())
	((eq? x (car list)) (remberAll x (cdr list)))
	(else (cons (car list) (remberAll x (cdr list)))))))
; multiuInsertR insert a new  element after the all targets of the list
(define multiInsertR (lambda (new old list) 
		       (cond ((null? list) '())
			     ((eq? old (car list)) (cons old (cons new (multiInsertR new old (cdr list))))) ;also to call the procedure multiInsertR to solve the rset of the list
			     (else (cons (car list) (multiInsertR new old (cdr list)))))))

;***********************************************************************************************
;========================
; chapter 4 number games
;========================

; all numbers are atoms
;tup+ stand for look each of the two list at the same time and add them up put the result into a list
(define tup+ (lambda (la lb) 
  (cond ((and (null? la) (null? lb)) '())
	; if the la and the lb don't have the same length, return the longer list's rest part
	((null? la) lb)
	((null? lb) la)
	(else  (cons (+ (car la) (car lb)) (tup+ (cdr la) (cdr lb)))))))


