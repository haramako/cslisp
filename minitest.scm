;;; minitest
(define *minitest-failed* 0)
(define *minitest-count* 0)

(define-syntax inc!
  (syntax-rules ()
	((_ ?x) (set! ?x (+ 1 ?x)))))

(define (minitest-dot)
  (inc! *minitest-count*)
  (display ".")
  (if (zero? (modulo *minitest-count* 80)) (newline)))
  
(define-syntax assert
  (syntax-rules ()
	((_ ?expect ?test)
	 (assert ?expect ?test equal?))
	((_ ?expect ?test ?elt=)
	 (begin
	   (minitest-dot)
	   (let ((*test* ?test)
			  (*expect* ?expect))
		 (if (?elt= *test* *expect*) #t
       (begin
			   (newline)
			   (puts "FAILED:" '?test "EXPECT" *expect* "BUT" *test*)
			   (inc! *minitest-failed*))))))
	))

(define-syntax assert-exception
  (er-macro-transformer 
    (lambda (expr rename compare)
      '(display 1))))

(define (minitest-finish)
  (newline)
  (puts "finished" *minitest-count* "tests" *minitest-failed* "failed")
  (exit (if (zero? *minitest-failed*) 0 1)))
