
(define-library (scheme write)
  (import (%embeded))
  (export display)
  (begin
    (import (%embeded) (scheme base))
    (%load "lib/scheme/write.scm")
    ))
