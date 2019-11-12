;; macro-expand-all, quasi-quote に必要な物は早めに
(define integer? number?)
(define quotient /)
#;(define call-with-current-continuation call/cc)

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (caaaar x) (car (caaar x)))
(define (caaadr x) (car (caadr x)))
(define (caadar x) (car (cadar x)))
(define (caaddr x) (car (caddr x)))
(define (cadaar x) (car (cdaar x)))
(define (cadadr x) (car (cdadr x)))
(define (caddar x) (car (cddar x)))
(define (cadddr x) (car (cdddr x)))
(define (cdaaar x) (cdr (caaar x)))
(define (cdaadr x) (cdr (caadr x)))
(define (cdadar x) (cdr (cadar x)))
(define (cdaddr x) (cdr (caddr x)))
(define (cddaar x) (cdr (cdaar x)))
(define (cddadr x) (cdr (cdadr x)))
(define (cdddar x) (cdr (cddar x)))
(define (cddddr x) (cdr (cdddr x)))

(define (nth n lis)
  (if (eqv? 0 n) (car lis) (nth (- n 1) (cdr lis))))

(define (nthcdr n lis)
  (if (eqv? 0 n) lis (nth (- n 1) (cdr lis))))


#;(define (map f li)
  (let recur ((f f) (li li))
	(if (pair? li)
		(cons (f (car li)) (recur f (cdr li)))
		li)))

(define (for-each f l)
  (let recur ((f f) (l l))
	(if (not (pair? l)) #f
		(f (car l))
		(recur f (cdr l)))))

(define (newline)
  (display end-of-line))

;; (puts obj1 ...)
(define (puts . x)
  (if (not (pair? x))
	  (display end-of-line)
	(display (car x))
	(display " ")
	(apply puts (cdr x))))

;; #p macro
(define (*tee* x)
  (puts "tee:" x)
  x)

(define (error err other)
  (display err)
  (newline)
  (exit))#;(define (error . mes)
  (apply puts (list "error:" mes))
  (backtrace)
  (exit 1))

;; require
(define %require-loaded-modules '())

(define (%require mod)
  ;; check already loaded
  (define (loaded? lis)
	(if (null? lis) #f
		(if (eqv? mod (car lis)) #t
			(loaded? (cdr lis)))))
  (if (loaded? %require-loaded-modules) #f
	  ;; not yet load
	  (set! %require-loaded-modules (cons mod %require-loaded-modules))
	  (let loop ((paths runtime-load-path))
		(if (null? paths)
			(error "cannot require" mod)
			(let ((path (string-append (car paths) "/" (symbol->string mod) ".scm")))
			  (if (file-exists? path)
				  (load path)
				  (loop (cdr paths))))))))

#;(define-syntax require
  (syntax-rules ()
	((_ ?mod) (%require '?mod))))


#;(define-syntax when 
  (syntax-rules ()
	  ((_ ?cnd ?body ...) (if ?cnd (begin ?body ...)))))

;; number

(define (zero? x) (eqv? x 0))
(define (negative? x) (< x 0))
(define (positive? x) (>= x 0))
(define (even? x) (eqv? (modulo x 2) 0))
(define (odd? x) (eqv? (modulo x 2) 1))

#;(define-syntax inc!
  (syntax-rules ()
	((_ x) (set! x (+ x 1)))))

#;(define-syntax dec!
  (syntax-rules ()
	((_ x) (set! x (+ x 1)))))

;; list

(define (list-tail li n)
  (let loop ((li li) (n n))
	(if (zero? n) li (loop (cdr li) (- n 1)))))

(define (length li)
  (let loop ((li li) (n 0))
	(if (null? li) n (loop (cdr li) (+ n 1)))))

#;(define-syntax let1
  (syntax-rules ()
	((_ ?var ?body ...)
	 (let (?var) ?body ...))))

(define (trace)
  (runtime-value-set! 'trace 1))

;;************************************************************
;; for srfi-1.scm
;; SEE: http://srfi.schemers.org/srfi-1/srfi-1-reference.scm
;;************************************************************

(define (:optional v def . f)
  (if (null? f)
	  (if (null? v) def (car v))
	  (if ((car f) (car v)) (car v) def)))

(define (tree-copy x_)
  (let recur ((x x_))
	(if (not (pair? x)) x
		(cons (recur (car x)) (recur (cdr x))))))

(define (check-arg pred val caller)
  (if (pred val) val (check-arg (error "Bad argument" val pred caller))))

;;************************************************************
;; for srfi-13.scm
;; SEE: http://srfi.schemers.org/srfi-13/srfi-13-reference.scm
;;************************************************************

(define (1+ n) (+ n 1))
(define (1- n) (- n 1))

(define (char-set? v) #f)

(define (char-set . cs)
  (lambda (c)
	(let loop ((cs cs))
	  (if (null? cs) #f
		  (if (char=? c (car cs)) #t (loop (cdr cs)))))))

(define (char-set-contains? cset c)
  (cset c))

(define (exact? v) (number? v))

(define (char-set:alphabetic c)
  (or (char<=? #\A c #\Z) (char<=? #\a c #\z)))

(define (char-set:letter c)
  (or (char<=? #\A c #\Z) (char<=? #\a c #\z)))

(define (char-set:lower-case c)
  (char<=? #\a c #\z))

(define (char-set:upper-case c)
  (char<=? #\A c #\Z))

(define (char-set:whitespace c)
  (or (char=? c #\space) (char=? c #\tab)))

(define (char-set:digit c)
  (and (char<=? #\0 c #\9)))

(define (char-set:numeric c)
  (and (char<=? #\0 c #\9)))

(define (char-set:punctuation c)
  (not (char-set:letter c)))

(define (char-set:graphic c)
  (not (char-set:whitespace c)))

(define char-alphabetic? char-set:alphabetic)
(define char-letter? char-set:letter)
(define char-upper-case? char-set:upper-case)
(define char-whitespace? char-set:whitespace)
(define char-digit? char-set:digit)
(define char-numeric? char-set:numeric)
(define char-graphic? char-set:graphic)

(define string=? eqv?)

(define (min . lis)
  (let loop ((min 9999)
			 (lis lis))
	(if (null? lis)
		min
		(if (< (car lis) min)
			(loop (car lis) (cdr lis))
			(loop min (cdr lis))))))

(define (char-ci<? a b)
  (char<? (char-downcase a) (char-downcase b)))

(define (char-ci=? a b)
  (char=? (char-downcase a) (char-downcase b)))

(define (string-list? ss)
  (or (null? ss)
	  (and (pair? ss) (string? (car ss)) (string-list? (cdr ss)))))
