; (import (gauche base) (file util))

(import 
    (scheme base) 
    (gauche base) 
    (srfi 13) 
    (scheme show base) 
    (scheme sort))

(dolist (m (all-modules))
    (if (string-prefix? "scheme" (symbol->string (module-name m)))
        (begin
            (print (module-name m))
            (let ((ex (module-exports m)))
                (let ((names (map (lambda (m) (symbol->string m)) ex)))
                    (dolist (e (list-sort string< names))
                        (print "\t" e)))))))

