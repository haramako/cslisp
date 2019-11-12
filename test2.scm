(%load "macrotest.scm")
#;(%load "lib/prelude.scm")

(define v.1 99)
(puts v.1)

(define-syntax test-syntax
  (syntax-rules ()
	  ((_ x) (display x))))

(test-syntax 0)
