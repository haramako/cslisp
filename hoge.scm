(define-syntax cond (er-macro-transformer 
    (lambda (expr rename compare) 
        (if (null? (cdr expr)) 
            (if #f #f) 
            ((lambda (cl) (if (compare (rename (quote else)) (car cl)) (if (pair? (cddr expr)) (error "non-final else in cond" expr) (cons (rename (quote begin)) (cdr cl))) 
            (if (if (null? (cdr cl)) #t (compare (rename (quote =>)) (cadr cl))) (list (list (rename (quote lambda)) (list (rename (quote tmp))) (list (rename (quote if)) 
            (rename (quote tmp)) (if (null? (cdr cl)) (rename (quote tmp)) (list (car (cddr cl)) (rename (quote tmp)))) (cons (rename (quote cond)) (cddr expr)))) (car cl)) 
            (list (rename (quote if)) (car cl) (cons (rename (quote begin)) (cdr cl)) (cons (rename (quote cond)) (cddr expr)))))) (cadr expr))))))=>(%define-syntax cond (make-transformer (er-macro-transformer (%lambda (expr rename compare) (%if (null? (cdr expr)) (%if #f #f #undef) ((%lambda (cl) (%if (compare (rename (%quote else)) (car cl)) (%if (pair? (cddr expr)) (error "non-final else in cond" expr) (cons (rename (%quote begin)) (cdr cl))) (%if (%if (null? (cdr cl)) #t (compare (rename (%quote =>)) (cadr cl))) (list (list (rename (%quote lambda)) (list (rename (%quote tmp))) (list (rename (%quote if)) (rename (%quote tmp)) (%if (null? (cdr cl)) (rename (%quote tmp)) (list (car (cddr cl)) (rename (%quote tmp)))) (cons (rename (%quote cond)) (cddr expr)))) (car cl)) (list (rename (%quote if)) (car cl) (cons (rename (%quote begin)) (cdr cl)) (cons (rename (%quote cond)) (cddr expr)))))) (cadr expr)))))))
begin 
(%if (null? (cdr expr)) 
    (%if #f #f #undef) 
    ((%lambda (cl) (%if (compare (rename (%quote else)) (car cl)) (%if (pair? (cddr expr)) (error "non-final else in cond" expr) (cons (rename (%quote begin)) (cdr cl))) (%if (%if (null? (cdr cl)) #t (compare (rename (%quote =>)) (cadr cl))) (list (list (rename (%quote lambda)) (list (rename (%quote tmp))) (list (rename (%quote if)) (rename (%quote tmp)) (%if (null? (cdr cl)) (rename (%quote tmp)) (list (car (cddr cl)) (rename (%quote tmp)))) (cons (rename (%quote cond)) (cddr expr)))) (car cl)) (list (rename (%quote if)) (car cl) (cons (rename (%quote begin)) (cdr cl)) (cons (rename (%quote cond)) (cddr expr)))))) (cadr expr)))
