
(define-library (scheme file)
  (import (scheme base) (%embeded))
  (export
   call-with-input-file call-with-output-file
   delete-file file-exists?
   open-binary-input-file open-binary-output-file
   open-input-file open-output-file
   with-input-from-file with-output-to-file
   )
   (begin 
    (define (open-input-file path)
      (%open-file path #f #f))
    (define (open-output-file path)
      (%open-file path #t #f))
    (define (open-binary-input-file path)
      (%open-file path #f #t))
    (define (open-binary-output-file path)
      (%open-file path #f #t))
    (define (call-with-input-file path proc)
      (let ((f (open-input-file path)))
        (proc f)))
    (define (call-with-output-file path proc)
      (let ((f (open-output-file path)))
        (proc f)))
    (define (with-input-from-file path proc)
      (error "not supported"))
    (define (with-output-to-file path proc)
      (error "not supported"))
    
    ))
