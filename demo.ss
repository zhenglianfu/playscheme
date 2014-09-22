;;the 'define' refer
;; refer x to 10
(define x 10)
;;true , false
(define false #f)
(define true #t)
; nil null
(define nil '())
;; refer anonymous to an anonymous function  
(define anonymous_plus (lambda (x y) (+ x y)))
;; refer _list_ to a list
(define _list_ '())
;refer % to remainder
(define % remainder)
;structure is simple, the point is complex the atom structure
(+ (- 1 (* 2 (/ 2 1))))
;;user defined process
(define (identify x) x)
(define (self x) x)
(define (inc x) (+ x 1))
(define (add-one x) (+ x 1))
(define (plus x y) (+ x y))
(define (square x) (* x x))
(define (cube x) (* x x x))
(define (average x y) (/ (+ x y) 2))
(define (foo . x) (void))
;;sometimes I always think about humems, that humens is stuck

;;use local var in a process
(define (add_local_one x) (define con 1) (+ x con))
;; process and the declare is the same way in scheme

;; =========== the beauty of lisp ===============

;; seven operation symbols in lisp
;; first : quote / '表示不对后面的表达式求值
; '只是语法糖而已
'a
;a
(quote  a)
;a
'1
;1
(quote 1)
;1
'plus
;plus
(quote plus)
;plus
'(plus 1 2)
;(plus 1 2)
(quote (plus 1 2))
;(plus 1 2)

(car (list 1 2))
(cdr (list 1 2))

(cons 1 2) ;(1 . 2) pair
(car (cons 1 2))
(cdr (cons 1 2))
(cons 1 '())

;; local variable  use let, let*, letrec
(let ((x 4) (y 3)) (define (f z) (+ z x y)) (f 5))
(let* ((x 4) (y (+ 3 x))) y)
;;letrec通常用于双重递归，可以互相引用

;find the max 公约数
(define (gcd a b) 
  (if (= b 0) 
      (if (= 0 a) 1 a) 
      (gcd b (remainder a b))))

;判断素数
(define (prime? n) 
  (= n (smallest-divisor n)))
; hlep fn
(define (divides? a b) (= 0 (remainder a b)))
(define (smallest-divisor n)
  (define (next b) 
    (if (= b 2) 
	(+ b 1) 
	(+ b 2)))
  (define (divides a b) 
    (cond ((> (square b) a) a)
	  ((= 0 (remainder a b)) b)
	  (else (divides a (next b)))))(divides n 2))
;fermat-guess prime number
(define (fermat-prime? n times) 
  (cond ((= 0 times) #t) 
	((fermat-test n) (fermat-prime? n (- times 1)))
	(else #f)))
;fermat little guess once
(define (fermat-test n) 
  (define (try-it a) 
    (= (expmod a n a) 0))
  (try-it (+ 1 (random (- n 1)))))
;expt mod of n, control the exp times less than m
(define (expmod base exp m) 
  (cond ((= exp 0) 1)
	((even? exp) (remainder (square (expmod base (/ exp  2) m)) m))
	(else (remainder (* base (expmod base (- exp 1) m)) m))))
;use the other fn
;when the base is to large, it could be crash dwon
(define (expmod-fn base exp m) (% (fast-expt-linear base exp) m))

;define high-level process deal with process
;plus from a to b, with a compute rule 
(define (sum-com a b term next) 
  (if (> a b) 
      0
      (+ (term a) (sum-com (next a) b term next))))
;example 
; sum-integer
(sum-com 1 10 (lambda (x) x) (lambda (x) (+ x 1)))
(sum-com 1 10 self add-one)
; sum-cube
(sum-com 1 10 (lambda (x) (cube x)) (lambda (x) (+ x 1)))
(sum-com 1 10 cube add-one)
;change sum-com to iter
(define (sum-com-linear a b term next) 
  (define (iter a result) 
    (if (> a b) 
	result 
	(iter (next a) (+ result (term a)))))
  (iter a 0))
; compute the multipart of the numbers
(define (multi-product a b term next) 
  (if (> a b)
      1
      (* (term a) (multi-product (next a) b term next))))
(define (multi-product-linear a b term next)
  (define (iter a result) 
    (if (> a b) 
	result 
	(iter (next a) (* result (term a)))))
  (iter a 1))
; accumulate
(define (accumulate a b null-value combiner term next) 
  (if (> a b) 
      null-value
      (combiner (term a) (accumulate (next a) b null-value combiner term next))))

(define (accumulate-iter a b null-value combiner term next) 
  (define (iter a result) 
    (if (> a b)
	result
	(iter (next a) (combiner result (term a)))))
  (iter a null-value))
; add filter return true/false
(define (accumulate-filter a b null-value term next combiner filter) 
  (cond ((> a b) null-value)
	((filter a) (combiner (term a) (accumulate-filter (next a) b null-value) term next combiner filter))
	(else (accumulate-filter (next a) b null-value term next combiner filter))))

; let make local variable
(let ((x 1) (y 2)) (+ x y))
; is the same as lambda express
((lambda (x y) (+ x y)) 1 2)
;return procedure
(define (average-domp f) 
  (lambda (x) (average x (f x))))
;; f(x)' = (f(x + i) - f(x)) / i (i->0)
(define EPSILON 0.00001)
 
;exer 1.40 square steps
(define (double-call f) 
  (lambda (x) (f (f x))))
;call the procedure
(((double-call (double-call (double-call double-call))) inc) 5); 2^(2^3) times call

(define (compose f g) 
  (lambda (x) (f (g x))))

(define (repeated f x) 
  (lambda (n) 
    (define (help n r) 
      (if (> n 0) 
	  (help (- n 1) (f r)) 
	  r)) 
    (help n x)))

(define (rep-com f x) 
  (define (help n g) 
    ;at lessest use once				       
    (if (> n 1) 
	(help (- n 1) (compose f g))
	g)) 
  (lambda (n) ((help n f) x)))

(define (smooth f) 
  (lambda (x) (/ (+ (f x) (f (+ x EPSILON)) (f (- x EPSILON))) 3)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  chapter2                           ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;jduge the type of the data which you declared
;I don't know how to change a char to a string
(define (to_string x) (cond 
			 ((number? x) (number->string x))
			 ((boolean? x) (if x "ture" "false"))
			 ((list? x) (list->string x))
			 ((symbol? x) (symbol->string x))
			 ((procedure? x) "<#procedure>")
			 (else x))) 

(define (typeof x) (cond 
		   ((boolean? x) "boolean") 
		   ((number? x) "number")
		   ((char? x) "char")
		   ((list? x) "list")
		   ((pair? x) "pair")
		   ((vector? x) "vector")
		   ((symbol? x) "symbol")
		   ((procedure? x) "procedure")
		   ((string? x) "string")
		   (else "undefined")))
; let's begin the chapter 2 formally 
; data structure in computer
; complex data structure in scheme

; =======  cons car cdr  =======
;; define cons car cdr use lambda
(define (i-cons x y) 
  (lambda (m) (m x y)))

(define (i-car z) 
  (z (lambda (p q) p)))

(define (i-cdr z) 
  (z (lambda (p q) q)))

;constructor function
(define (make-rat x y)
  (define g (abs (gcd x y)))
  (cond ((= 0 y) (error "mathmatic error" "can't divi 0"))
	((< y 0) (cons (/ (- x) g) (/ (- y) g)))
	(else (cons (/ x g) (/ y g)))))
;select function
(define (number x) (car x))
(define (denom x) (cdr x))
(define (add-rat x y) 
  (make-rat (+ (* (number x) (denom y)) (* (number y) (denom x)))
	    (* (denom x) (denom y))))
(define (sub-rat x y) 
  (make-rat (- (* (number x) (denom y)) 
	       (* (number y) (denom x)))
	    (* (denom x) (denom y))))

(define (multi-rat x y) 
  (make-rat (* (number x) (number y))
	    (* (denom x) (denom y))))

(define (divi-rat x y) 
  (make-rat (* (number x) (denom y))
	    (* (denom x) (number y))))

(define (equal-rat? x y)
  (= (* (number x) (denom y)) 
     (* (number y) (denom x))))

(define (print-rat x) 
  (display (number x))
  (display #\/)
  (display (denom x)))

(define (make-segment s-point e-point) 
  (cons s-point e-point))

(define (start-segment segment) 
  (car segment))

(define (end-segment segment) 
  (cdr segment))

(define (k-segment segment)
  (let ((p1 (start-segment segment)) (p2 (end-segment segment))) 
    (/ (- (point-y p2) (point-y p1)) 
       (- (point-x p2) (point-x p1)))))

(define (midpoint-segment segment) 
  (let ((p1 (start-segment segment)) (p2 (end-segment segment))) 
    (make-point 
     (average (point-x p1) (point-x p2)) 
     (average (point-y p1) (point-y p2)))))

(define (make-point x y) 
  (cons x y))

(define (point-x p) 
  (car p))

(define (point-y p)
  (cdr p))

(define (print-point p) 
  (display #\()
  (display (point-x p))
  (display #\,)
  (display (point-y p))
  (display #\)))

; *******************************
; how to declare a rectangle? ---- exe 2.3
; *******************************

(define (remian-times n d) 
  (define (help n t) 
    (if (= 0 (% n d))  
	(help (/ n d) (+ t 1)) 
	t))
  (if (= 0 d) #f (help n 0)))
; change pair to 2^a*3^b
(define (u-cons x y) (* (expt 2 x) (expt 3 y)))
(define (u-car z) (remian-times z 2))
(define (u-cdr z) (remian-times z 3))


; Church compute 
; you don't be expected to understand this
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n) 
  (lambda (f) (lambda (x) (f ((n f) x)))))

; Alyssa P.Hacker
; min , max are the basic procedure
(define (make-interval a b) (cons a b))
(define (upper-bound c) (max (car c) (cdr c)))
(define (lower-bound c) (min (car c) (cdr c)))

(define (sub-interval x y) 
  (make-interval (- (upper-bound x) (lower-bound y))
		 (- (lower-bound x) (upper-bound y))))

(define (multi-interval x y) 
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4) 
		   (max p1 p2 p3 p4))))

; something wrong in this process
(define (div-interval x y)
  (if (or (> (upper-bound y) 0) (< (lower-bound y) 0)) 
      (error "invalidate params" "not cross 0")
      (multi-interval x 
		      (make-interval (/ 1.0 (upper-bound y))
				     (/ 1.0 (lower-bound y))))))

; list by cons
(cons 0 (cons 1 (cons 2 (cons 3 (cons 4 #f)))))
(cadr (list 1 2 3))

(define (list-ref items n)
  (cond ((atom? items) (error "car" "argument is not a pair"))
	((> 0 n) #f)
	((= 0 n) (car items))
	(else (list-ref (cdr items) (- n 1)))))

(define (u-length items) 
  (if (null? items) 
      0 
      (+ 1 (length (cdr items)))))

(define (u-append x y)
  (define (help ret x y)
    (cond ((and (null? x) (null? y)) ret)
	  ((null? x) (help (cons (car y) ret) x (cdr y)))
	  (else (help (cons (car x) ret) (cdr x) y))))
  (help '() x y))

; last-pair
(define (last-pair items) 
  (cond ((null? items) '())
	((null? (cdr items)) items)
	(else (last-pair (cdr items)))))

; reverse
(define (u-reverse items) 
  (define (help items temp) 
    (if (null? items) 
	temp
	(help (cdr items) (append (list (car items)) temp))))
  (help items '()))

(define (no-more? list) 
  (null? list))


; dynamic arguments in lambda express, don't need () for arguments
(define dynamic-g (lambda a 
		    (if (null? a) 
			"" 
			(display a))))
; dynamic arguments use .
(define (dynamic-fn . a) 
  (if (null? a) 
      ""
      (display a)))

(define (u-map fn items) 
  (if (null? items)
      '()
      (cons (fn (car items)) (u-map fn (cdr items)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; todo to finish it
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get-firsts items) 
  (cond ((null? items) '()) 
	(else (cons (car (car items)) 
		    (get-firsts (cdr items))))))

(define (filter-firsts items) 
  (define (iter items result) 
    (if (null? items) 
	result
	(iter (cdr items) 
	      (if (null? result) 
		  (cdr (car items))  
		  (cons result (list (cdr (car items))))))))
  (if (or (null? items) (null? (car items))) '() (iter items '())))
  
(define (u-map-dynamic fn . items) 
  (define (iter fn items result)
    (if (or (null? items) (null? (get-firsts items)))
	result 
	(iter fn (filter-firsts items) (append result (list (apply fn (get-firsts items)))))))
  (cond ((null? items) (list (fn)))
	((= 1 (length items)) (u-map fn (car items)))
	((not (apply equal-length? items)) (error "params error" "the length of the pair list is not equal"))
	(else (iter fn items '()))))

(define (square-list items) 
  (if (null? items) 
      '()
      (cons (square (car items)) 
	    (square-list (cdr items)))))

(define (equal-list-len? x y . z) 
  (define (help p items) 
    (cond ((null? items) #t)
	  ((not (= (length p) (length (car items)))) #f)
	  (else (help p (cdr items)))))
  (cond ((null? z) (= (length x) (length y)))
	((not (= (length x) (length y))) #f)
	(else (help x z))))

(define (equal-length? . z) 
  (cond ((> 2 (length z)) (error "invalidate params number to call" "at least need one param"))
	(else (apply equal-list-len? z))))

(define (call fn . z) 
  (apply fn z))

(define (u-for-each fn items)
  (define (iter fn items) 
    (fn (car items))
    (u-for-each fn (cdr items)))
  (cond ((null? items) (void))
	(else (iter fn items))))

(define (count-leaves? tree) 
  (cond ((null? tree) 0)
	((not (pair? tree)) 1)
	(else (+ (count-leaves? (car tree))
		 (count-leaves? (cdr tree))))))

(define (firnge tree) 
  (define (help ret list) 
    (cond ((null? list) ret)
	  ((not (pair? list)) (cons list ret))
	  (else (append (help ret (car list))
		      (help ret (cdr list))))))
  (help '() tree))

(define (count-levels? tree) 
  tree)

;; exec 2.29  p74
; banary tree like

; constructor
; merge left branch and right branch
(define (make-moile left right) 
  (list left right))

(define (make-branch length structure) 
  (list length structure))

; choose function 
(define (left-branch tree) 
  (car tree))

(define (right-branch tree) 
  (car (cdr tree)))

(define (branch-length branch) 
  (car branch))
(define (branch-structure branch) 
  (car (cdr branch)))

; compute the total weight 
(define (total-weight moile) 
  (define (count-branch branch) 
    (cond ((null? branch) 0)
	  ((pair? branch) (if (pair? (branch-structure branch))
			      (total-weight (branch-structure branch)) 
			      (branch-structure branch)))
	  (else 0)))
  (cond ((null? moile) 0)
	((pair? moile) (+ (count-branch (left-branch moile)) (count-branch (right-branch moile))))))


;compute the tree is blance or not


; tree with map
(define (scale-tree tree factor) 
  (cond ((null? tree) '())
	((not (pair? tree)) (* tree factor))
	(else (cons (scale-tree (car tree) factor) (scale-tree (cdr tree) factor)))))

;use map, map will car the list one by one automaticly, so the argument is (car tree) -> subtree
(define (scale-tree-map tree factor) 
  (map (lambda (sub-tree) 
	 (if (pair? sub-tree) 
	     (scale-tree-map sub-tree factor)
	     (* sub-tree factor))) 
       tree))

;use the basicly operation to bulid the procedure square-tree
(define (square-tree tree) 
  (cond ((null? tree) '())
	((not (pair? tree)) (square tree))
	(else (cons (square-tree (car tree)) 
		    (square-tree (cdr tree))))))

;use map and recursion to re-bulid the square-tree
(define (square-tree-map tree) 
  (map (lambda (sub-tree) 
	 (if (pair? sub-tree) 
	     (square-tree-map sub-tree) 
	     (square sub-tree))) 
       tree))

; more abstract way to use map in the tree, as the map used in the list, move up and you can see it.
(define (tree-map fn tree) 
  (map (lambda (sub-tree) 
	 (if (pair? sub-tree) 
	     (tree-map fn sub-tree)
	     (fn sub-tree))) 
       tree))

;define the square-tree use the tree-map above
(define (square-map-tree tree) 
  (tree-map square tree))

;subset divi the sub form a list
;(1 2) -> ((), (1) (2) (1 2))
(define (subsets s) 
  (if (null? s) 
      (list s)
      (let ((rest (subsets (cdr s))))
	; try to explain this, use the (1 2) to call the procedure
	(append rest (map (lambda (x) (cons (car s) x))  rest)))))

;to make structure more clearly, we segment process to pices
;first filter procedure, return a list
(define (filter predicate sequence) 
  (cond ((null? sequence) '())
	((predicate (car sequence)) 
	 (cons (car sequence) 
	       (filter predicate (cdr sequence))))
	(else (filter predicate (cdr sequence)))))
;ex return the even numbers into a new list
(filter even? '(1 2 3 4 5))

; accumulate
(define (accumulate op initial sequence) 
  (if (null? sequence) 
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))


; sum the square of the odd numbers of a list
(define (sum-odd-square list) 
  (accumulate + 0 (map square (filter odd? list))))

; above that we make each operation of the procedure clearly

; we use accumulate to define the basic procedure
; (define (map p list) 
;   (accumulate (lambda (x y) (cons (p x) y)) '() list))

;(define (length list) 
;  (accumulate (lambda (x y) (+ 1 y)) 0 list))

;(define (append seq1 seq2) 
;  (accumulate cons seq2  seq1))
;(cons (car seq1) (cons (car (cdr seq1)) (cons (car (cdr (cdr seq1))) seq2)))

; compute ex^0 + ax + bx^2 + cx^3 
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff high-trem) 
		(+ this-coeff (* x high-trem))) 
	      0 
	      coefficient-sequence))

; tha last arg is sequence of sequences '((1 2) (2 4) (3 6)), supposed that each element's length of the sequence is the same
(define (accumulate-n op init seqs) 
  (define first-ele (lambda (list) 
		      (if (null? list) 
		      '() 
		      (cons (car (car list)) (first-ele (cdr list))))))
  (define filter-first (lambda (list) 
			 (if (null? list) 
			     '() 
			     (cons (cdr (car list))
				   (filter-first (cdr list))))))
  (if (null? (car seqs)) 
      '()
      (cons (accumulate op init (first-ele seqs)) 
	    (accumulate-n op init (filter-first seqs)))))

; matrix forget how to calculate matrix
;| 1 2 3 |
;| 4 5 6 |
;| 7 8 9 |

(define (fold-left op init sequence) 
  (define (iter result rest) 
    (if (null? rest)
	result 
	(iter (op result (car rest)) 
	      (cdr rest))))
  (iter init sequence))

; you have to write two versions of the procedure, one is recurivse, one is iter linear, so you may be got a level up

; high level will use map 
(define (enumerate-interval i j) 
  (if (> i j) 
      '()
      (cons i (enumerate-interval (+ i 1) j))))

(define (enumerate-interval i j)
  (define (iter i result) 
    (if (> i j) 
	result 
	(iter (+ i 1) (append result (list i)))))
  (iter i '()))

(define (enumerate-interval i j)
  (define (iter result n) 
    (if (< n i) 
	result
	(iter (cons n result) (- n 1))))
  (iter '() j))

;create pair of pair
(define (create-nums-pair n) 
  (accumulate append '() (map (lambda (i) 
				(map (lambda (j) (list i j)) 
				     (enumerate-interval 1 (- i 1)))) 
			      (enumerate-interval 1 n))))

; more abstract than the above procedure
(define (flatmap proc seq) 
  (accumulate append '() (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair) 
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (sum-prime-pair n) 
  (map make-pair-sum 
       (filter prime-sum? (flatmap (lambda (i) 
				     (map (lambda (j) (list i j)) 
					  (enumerate-interval 1 (- i 1)))) 
				   (enumerate-interval 1 n))) ))

; this don't filter the '()
(define (sum-prime-pair-nf n)
  (accumulate cons 
	      '()   
	      (map (lambda (p) 
		     (if (prime-sum? p) 
			 (make-pair-sum p) 
			 '()))
		   (flatmap (lambda (i) 
			      (map (lambda (j) (list i j)) 
				   (enumerate-interval 1 (- i 1)))) 
			    (enumerate-interval 1 n)))))


; create all different order sequences of the list '(1 2 3)
(define (permutations s) 
  (if (null? s) 
      '(())  ; return a list contain a empty set
      (flatmap (lambda (x) 
		 (map (lambda (y) (cons x y)) 
		      (permutations (remove x s)))) 
	       s)))

(define (unique-pairs n)
  (if (<= n 0) 
      '()
      (flatmap (lambda (x) 
	      (map (lambda (y) (cons x y))
		   (enumerate-interval (+ x 1) n)) ) 
	    (enumerate-interval 1 n))))

; 0 < i < j < k < n; i + j + k = s
(define (unique-pair-3 n s) 
  (cond  ((< n 3) '())
	 ((<= s 0) (error "invalidate argument" "the sum must be great than 0"))
	 (else (flatmap (lambda (i) 
			  (map (lambda (j) (map (lambda (k) 
						  (cons i (cons j (cons k '())))) 
						(enumerate-interval (+ j 1) n))) 
			       (enumerate-interval (+ i 1) n)))	    
			(enumerate-interval 1 n)))))
;solution of eight queens 
;(define (queens k) ())

(define (queens board-size) 
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter 
	 (lambda (positions) (safe? k positions))
	 (flatmap 
	  (lambda (rest-of-queens) 
	    (map (lambda (new-row) 
		   (adjoin-position new-row k rest-of-queens)) 
		 (enumerate-interval 1 board-size)))
	  (queen-cols (- k 1))))))
  (queen-cols board-size))

;position
(define (make-position row col) 
  (cons row col))

(define (row-position position) 
  (car position))

(define (col-position position) 
  (cdr position))

; adjoin-position
(define (adjoin-position new-row k rest-of-queens)
  (cons (make-position new-row k) rest-of-queens))

(define empty-board '())

(define (safe? k positions)
  (define (check new-queen check-list) 
    (cond ((null? check-list) #t)
	  ((= (row-position new-queen) (row-position (car check-list))) #f)
	  ((= (col-position new-queen) (col-position (car check-list))) #f)
	  ((= 1 (abs (/ (- (row-position new-queen) (row-position (car check-list))) (- (col-position new-queen) (col-position (car check-list)))))) #f)
	  (else (check new-queen (cdr check-list)))))
  (check (get-queen k positions) (filter (lambda (p)
					   (not (= k (col-position p)))) positions)))

(define (get-queen k positions) 
    (cond ((null? positions) positions)
	  ((= k (col-position (car positions))) (car positions))
	  (else (get-queen k (cdr positions)))))

;=============================; 

; call-with-current-continuation

;; this is not working in scheme code, return is not binding
(define search-zero 
  (lambda (lst) 
    (for-each (lambda (ele) 
		(if (= 0 ele) 
		    (return ele) 
		    (printf "~a~%" ele))) 
	      lst)))

; the correct code working in scheme code, correct search-zero procedure
; with the procedure call/cc (call-with-current-continuation) to no-local exit(means that do not finish the whole procedure, exit in the middle of the procedure)
(define search-zero (lambda (lst) 
		      (call/cc 
		       (lambda (return) 
			 (for-each (lambda (x)
				     (if (= 0 x) 
					 (return x))) 
				   lst) 
			 #f))))

; (search-zero '(1 2 0 3 5)) 
; find the first 0 of index '3', the procedure is returned, do not check the rest of list '(3 5), exit of procedure 'search-zero' when find the first 0 of the list
;====================================================================;                                                                          
; construct process 'equal?' by base procedure eq?
; equal? can comp two items, if items are list both, will eq? each item of their items
(define equal? (lambda (x y) 
		 (if (and (pair? x) (pair? y)) 
		     (equal-list? x y)
		     (eq? x y))))

(define (equal-list? x y)
  (cond ((and (null? x) (null? y)) true)
	((or (and (null? x) (not (null? y))) 
	     (and (not (null? x)) (null? y))) false) 
	((equal? (car x) (car y)) (equal? (cdr x) (cdr y))) 
	(else false)))

