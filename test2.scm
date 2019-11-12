(%load "macrotest.scm")
(%load "lib/prelude.scm")

(define-syntax test-syntax
  (syntax-rules ()
	  ((_ x) (puts x))
	  ((_ x y) (puts x y))))

(test-syntax 1)
(test-syntax 1 2)
