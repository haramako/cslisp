;;; minitest
(define *minitest-failed* 0)
(define *minitest-count* 0)

(define (zero? n) (= n 0))
(define (inc! x) #f)

(define (minitest-dot)
  (set! *minitest-count* (+ 1 *minitest-count*))
  (display ".")
  (if (zero? (modulo *minitest-count* 80)) (newline)))

(define (assert expect test . rest)
  (minitest-dot)
  (if (equal? test expect) #t
    (newline)
    (puts "FAILED:" '?test "EXPECT" expect "BUT" test)
    (inc! *minitest-failed*)))

(define-syntax assert-exception
  (er-macro-transformer 
    (lambda (expr rename compare)
      '(display 1))))

(define (minitest-finish)
  (newline)
  (puts "finished" *minitest-count* "tests" *minitest-failed* "failed")
  (exit (if (zero? *minitest-failed*) 0 1)))
