(define-library (scheme base)
(export 
  integer?
  quotient
  caar
  cadr
  cdar
  cddr
  end-of-line
  exit
  newline
  error

  close-syntax
  make-transformer
  define-syntax
  er-macro-transformer
  identifier?

  cond
  or
  and
  quasiquote
  letrec
  let
  let*
  map
  any
  every
  vector?
  cons-source
  memq
  find
  syntax-quote
  strip-syntactic-closures
  assoc
  assq
  assv
  syntax-rules

  call-with-current-continuation
  call/cc

  nth
  nthcdr
  for-each
  unless
  when

  values
  call-with-values
  receive
  let-values

  max
  min
  zero?
  negative?
  positive?
  even?
  odd?
  truncate-quotient
  truncate-reminder
  quotient
  reminder
  modulo
  floor
  ceiling
  round
  truncate

  list-tail
  length
  trace
  :optional

  eval

  ; 
  include
  )

(begin

(import (%embeded))

(define integer? number?)
(define quotient truncate-quotient)
(define reminder truncate-reminder)
(define modulo floor-remainder)
;(define call-with-current-continuation call/cc)

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

;(if #f 2)
;(aaa)

(define end-of-line "\n")

(define exit %exit)

(define (newline)
  (display end-of-line))

(define (error err . other)
  (display "error: ")
  (display err)
  (newline)
  (%backtrace)
  (exit 1))

(define (close-syntax sym mac-env)
  sym)

(define make-transformer
  (lambda (transformer)
    (lambda (expr use-env mac-env)
	  (transformer expr))))

(%define-syntax define-syntax
				(lambda (expr use-env mac-env)
				  (list (close-syntax '%define-syntax mac-env)
						(cadr expr)
						(list (close-syntax 'make-transformer mac-env)
							  (car (cddr expr))))))

(define free-identifier=?
  (lambda (x y)
	(eq? x y)))

(define (current-renamer . rest)
  (lambda (x) x))

(define er-macro-transformer
  (lambda (f)
    (lambda (expr)
      (f expr (current-renamer) free-identifier=?))))

(define (vector? x) #f)

(define identifier? symbol?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; include

(define (include filename)
    (%include (string-append (%dir-name (%current-filename)) "\\" filename)))

(define-syntax cond
  (er-macro-transformer
   (lambda (expr rename compare)
     (if (null? (cdr expr))
         (if #f #f)
         ((lambda (cl)
            (if (compare (rename 'else) (car cl))
                (if (pair? (cddr expr))
                    (error "non-final else in cond" expr)
                    (cons (rename 'begin) (cdr cl)))
                (if (if (null? (cdr cl)) #t (compare (rename '=>) (cadr cl)))
                    (list (list (rename 'lambda) (list (rename 'tmp))
                                (list (rename 'if) (rename 'tmp)
                                      (if (null? (cdr cl))
                                          (rename 'tmp)
                                          (list (car (cddr cl)) (rename 'tmp)))
                                      (cons (rename 'cond) (cddr expr))))
                          (car cl))
                    (list (rename 'if)
                          (car cl)
                          (cons (rename 'begin) (cdr cl))
                          (cons (rename 'cond) (cddr expr))))))
          (cadr expr))))))

(define-syntax or
  (er-macro-transformer
   (lambda (expr rename compare)
     (cond ((null? (cdr expr)) #f)
           ((null? (cddr expr)) (cadr expr))
           (else
            (list (rename 'let) (list (list (rename 'tmp) (cadr expr)))
                  (list (rename 'if) (rename 'tmp)
                        (rename 'tmp)
                        (cons (rename 'or) (cddr expr)))))))))

(define-syntax and
  (er-macro-transformer
   (lambda (expr rename compare)
     (cond ((null? (cdr expr)))
           ((null? (cddr expr)) (cadr expr))
           (else (list (rename 'if) (cadr expr)
                       (cons (rename 'and) (cddr expr))
                       #f))))))

(define-syntax quasiquote
  (er-macro-transformer
   (lambda (expr rename compare)
     (define (qq x d)
       (cond
        ((pair? x)
         (cond
          ((compare (rename 'unquote) (car x))
           (if (<= d 0)
               (cadr x)
               (list (rename 'list) (list (rename 'quote) 'unquote)
                     (qq (cadr x) (- d 1)))))
          ((compare (rename 'unquote-splicing) (car x))
           (if (<= d 0)
               (list (rename 'cons) (qq (car x) d) (qq (cdr x) d))
               (list (rename 'list) (list (rename 'quote) 'unquote-splicing)
                     (qq (cadr x) (- d 1)))))
          ((compare (rename 'quasiquote) (car x))
           (list (rename 'list) (list (rename 'quote) 'quasiquote)
                 (qq (cadr x) (+ d 1))))
          ((and (<= d 0) (pair? (car x))
                (compare (rename 'unquote-splicing) (caar x)))
           (if (null? (cdr x))
               (cadr (car x))
               (list (rename 'append) (cadr (car x)) (qq (cdr x) d))))
          (else
           (list (rename 'cons-source) (qq (car x) d) (qq (cdr x) d)
                 (list (rename 'quote) x)))))
        ((vector? x) (list (rename 'list->vector) (qq (vector->list x) d)))
        ((if (identifier? x) #t (null? x)) (list (rename 'quote) x))
        (else x)))
     (qq (cadr expr) 0))))

(define-syntax letrec
  (er-macro-transformer
   (lambda (expr rename compare)
     ((lambda (defs)
        `((,(rename 'lambda) () ,@defs ,@(cddr expr))))
      (map (lambda (x) (cons (rename 'define) x)) (cadr expr))))))

(define-syntax let
  (er-macro-transformer
   (lambda (expr rename compare)
     (if (null? (cdr expr)) (error "empty let" expr))
     (if (null? (cddr expr)) (error "no let body" expr))
     ((lambda (bindings)
        (if (list? bindings) #f (error "bad let bindings"))
        (if (every (lambda (x)
                     (if (pair? x) (if (pair? (cdr x)) (null? (cddr x)) #f) #f))
                   bindings)
            ((lambda (vars vals)
               (if (identifier? (cadr expr))
                   `((,(rename 'lambda) ,vars
                      (,(rename 'letrec) ((,(cadr expr)
                                           (,(rename 'lambda) ,vars
                                            ,@(cdr (cddr expr)))))
                       (,(cadr expr) ,@vars)))
                     ,@vals)
                   `((,(rename 'lambda) ,vars ,@(cddr expr)) ,@vals)))
             (map car bindings)
             (map cadr bindings))
            (error "bad let syntax" expr)))
      (if (identifier? (cadr expr)) (car (cddr expr)) (cadr expr))))))

(define-syntax let*
  (er-macro-transformer
   (lambda (expr rename compare)
     (if (null? (cdr expr)) (error "empty let*" expr))
     (if (null? (cddr expr)) (error "no let* body" expr))
     (if (null? (cadr expr))
         `(,(rename 'let) () ,@(cddr expr))
         (if (if (list? (cadr expr))
                 (every
                  (lambda (x)
                    (if (pair? x) (if (pair? (cdr x)) (null? (cddr x)) #f) #f))
                  (cadr expr))
                 #f)
             `(,(rename 'let) (,(caar (cdr expr)))
               (,(rename 'let*) ,(cdar (cdr expr)) ,@(cddr expr)))
             (error "bad let* syntax"))))))

(define (map proc ls . lol)
  (define (map1 proc ls res)
    (if (pair? ls)
        (map1 proc (cdr ls) (cons (proc (car ls)) res))
        (reverse res)))
  (define (mapn proc lol res)
    (if (every pair? lol)
        (mapn proc
              (map1 cdr lol '())
              (cons (apply proc (map1 car lol '())) res))
        (reverse res)))
  (if (null? lol)
      (map1 proc ls '())
      (mapn proc (cons ls lol) '())))

(define (any pred ls . lol)
  (define (any1 pred ls)
    (if (pair? (cdr ls))
        ((lambda (x) (if x x (any1 pred (cdr ls)))) (pred (car ls)))
        (pred (car ls))))
  (define (anyn pred lol)
    (if (every pair? lol)
        ((lambda (x) (if x x (anyn pred (map cdr lol))))
         (apply pred (map car lol)))
        #f))
  (if (null? lol) (if (pair? ls) (any1 pred ls) #f) (anyn pred (cons ls lol))))

(define (every pred ls . lol)
  (define (every1 pred ls)
    (if (null? (cdr ls))
        (pred (car ls))
        (if (pred (car ls)) (every1 pred (cdr ls)) #f)))
  (if (null? lol)
      (if (pair? ls) (every1 pred ls) #t)
      (not (apply any (lambda xs (not (apply pred xs))) ls lol))))


(define (vector? ls) #f)
(define (cons-source kar kdr) (cons kar kdr))

(define (memq obj lis)
  (if (null? lis) 
    #f
    (if (eq? (car lis) obj) 
      #t 
      (memq obj (cdr lis)))))


(define (find-tail pred ls)
  (and (pair? ls) (if (pred (car ls)) ls (find-tail pred (cdr ls)))))

(define (find pred ls)
  (cond ((find-tail pred ls) => car) (else #f)))

(define-syntax syntax-quote 
  (er-macro-transformer 
    (lambda (expr rename compare)
      (list (rename 'quote) (cadr expr)))))

(define strip-syntactic-closures identity)

(define (assoc obj ls . o)
  (let ((eq (if (pair? o) (car o) equal?)))
    (let assoc ((ls ls))
      (cond ((null? ls) #f)
            ((eq obj (caar ls)) (car ls))
            (else (assoc (cdr ls)))))))

(define (assq obj ls) (assoc obj ls eq?))
(define (assv obj ls) (assoc obj ls eqv?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax-rules

(define (syntax-rules-transformer expr rename compare)
  (let ((ellipsis-specified? (identifier? (cadr expr)))
        (count 0)
        (_er-macro-transformer (rename 'er-macro-transformer))
        (_lambda (rename 'lambda))      (_let (rename 'let))
        (_begin (rename 'begin))        (_if (rename 'if))
        (_and (rename 'and))            (_or (rename 'or))
        (_eq? (rename 'eq?))            (_equal? (rename 'equal?))
        (_car (rename 'car))            (_cdr (rename 'cdr))
        (_cons (rename 'cons))          (_pair? (rename 'pair?))
        (_null? (rename 'null?))        (_expr (rename 'expr))
        (_rename (rename 'rename))      (_compare (rename 'compare))
        (_quote (rename 'syntax-quote)) (_apply (rename 'apply))
        (_append (rename 'append))      (_map (rename 'map))
        (_vector? (rename 'vector?))    (_list? (rename 'list?))
        (_len (rename 'len))            (_length (rename 'length*))
        (_- (rename '-))   (_>= (rename '>=))   (_error (rename 'error))
        (_ls (rename 'ls)) (_res (rename 'res)) (_i (rename 'i))
        (_reverse (rename 'reverse))
        (_vector->list (rename 'vector->list))
        (_list->vector (rename 'list->vector))
        (_cons3 (rename 'cons-source))
        (_underscore (rename '_)))
    (define ellipsis (if ellipsis-specified? (cadr expr) (rename '...)))
    (define lits (if ellipsis-specified? (car (cddr expr)) (cadr expr)))
    (define forms (if ellipsis-specified? (cdr (cddr expr)) (cddr expr)))
    (define (next-symbol s)
      (set! count (+ count 1))
      (rename (string->symbol (string-append s (%number->string count)))))
    (define (expand-pattern pat tmpl)
      (let lp ((p (cdr pat))
               (x (list _cdr _expr))
               (dim 0)
               (vars '())
               (k (lambda (vars)
                    (list _cons (expand-template tmpl vars) #f))))
        (let ((v (next-symbol "v.")))
          (list
           _let (list (list v x))
           (cond
            ((identifier? p)
             (cond
              ((ellipsis-mark? p)
               (error "bad ellipsis" p))
              ((memq p lits)
               (list _and
                     (list _compare v (list _rename (list _quote p)))
                     (k vars)))
              ((compare p _underscore)
               (k vars))
              (else
               (list _let (list (list p v)) (k (cons (cons p dim) vars))))))
            ((ellipsis? p)
             (cond
              ((not (null? (cdr (cdr p))))
               (cond
                ((any (lambda (x) (and (identifier? x) (ellipsis-mark? x)))
                      (cddr p))
                 (error "multiple ellipses" p))
                (else
                 (let ((len (length* (cdr (cdr p))))
                       (_lp (next-symbol "lp.")))
                   `(,_let ((,_len (,_length ,v)))
                      (,_and (,_>= ,_len ,len)
                             (,_let ,_lp ((,_ls ,v)
                                          (,_i (,_- ,_len ,len))
                                          (,_res (,_quote ())))
                                    (,_if (,_>= 0 ,_i)
                                        ,(lp `(,(cddr p)
                                               (,(car p) ,(car (cdr p))))
                                             `(,_cons ,_ls
                                                      (,_cons (,_reverse ,_res)
                                                              (,_quote ())))
                                             dim
                                             vars
                                             k)
                                        (,_lp (,_cdr ,_ls)
                                              (,_- ,_i 1)
                                              (,_cons3 (,_car ,_ls)
                                                       ,_res
                                                       ,_ls))))))))))
              ((identifier? (car p))
               (list _and (list _list? v)
                     (list _let (list (list (car p) v))
                           (k (cons (cons (car p) (+ 1 dim)) vars)))))
              (else
               (let* ((w (next-symbol "w."))
                      (_lp (next-symbol "lp."))
                      (new-vars (all-vars (car p) (+ dim 1)))
                      (ls-vars (map (lambda (x)
                                      (next-symbol
                                       (string-append
                                        (symbol->string
                                         (identifier->symbol (car x)))
                                        "-ls")))
                                    new-vars))
                      (once
                       (lp (car p) (list _car w) (+ dim 1) '()
                           (lambda (_)
                             (cons
                              _lp
                              (cons
                               (list _cdr w)
                               (map (lambda (x l)
                                      (list _cons (car x) l))
                                    new-vars
                                    ls-vars)))))))
                 (list
                  _let
                  _lp (cons (list w v)
                            (map (lambda (x) (list x (list _quote '()))) ls-vars))
                  (list _if (list _null? w)
                        (list _let (map (lambda (x l)
                                          (list (car x) (list _reverse l)))
                                        new-vars
                                        ls-vars)
                              (k (append new-vars vars)))
                        (list _and (list _pair? w) once)))))))
            ((pair? p)
             (list _and (list _pair? v)
                   (lp (car p)
                       (list _car v)
                       dim
                       vars
                       (lambda (vars)
                         (lp (cdr p) (list _cdr v) dim vars k)))))
            ((vector? p)
             (list _and
                   (list _vector? v)
                   (lp (vector->list p) (list _vector->list v) dim vars k)))
            ((null? p) (list _and (list _null? v) (k vars)))
            (else (list _and (list _equal? v p) (k vars))))))))
    (define ellipsis-mark?
      (if (if ellipsis-specified?
              (memq ellipsis lits)
              (any (lambda (x) (compare ellipsis x)) lits))
          (lambda (x) #f)
          (if ellipsis-specified?
              (lambda (x) (eq? ellipsis x))
              (lambda (x) (compare ellipsis x)))))
    (define (ellipsis-escape? x) (and (pair? x) (ellipsis-mark? (car x))))
    (define (ellipsis? x)
      (and (pair? x) (pair? (cdr x)) (ellipsis-mark? (cadr x))))
    (define (ellipsis-depth x)
      (if (ellipsis? x)
          (+ 1 (ellipsis-depth (cdr x)))
          0))
    (define (ellipsis-tail x)
      (if (ellipsis? x)
          (ellipsis-tail (cdr x))
          (cdr x)))
    (define (all-vars x dim)
      (let lp ((x x) (dim dim) (vars '()))
        (cond ((identifier? x)
               (if (or (memq x lits)
                       (compare x _underscore))
                   vars
                   (cons (cons x dim) vars)))
              ((ellipsis? x) (lp (car x) (+ dim 1) (lp (cddr x) dim vars)))
              ((pair? x) (lp (car x) dim (lp (cdr x) dim vars)))
              ((vector? x) (lp (vector->list x) dim vars))
              (else vars))))
    (define (free-vars x vars dim)
      (let lp ((x x) (free '()))
        (cond
         ((identifier? x)
          (if (and (not (memq x free))
                   (cond ((assq x vars) => (lambda (cell) (>= (cdr cell) dim)))
                         (else #f)))
              (cons x free)
              free))
         ((pair? x) (lp (car x) (lp (cdr x) free)))
         ((vector? x) (lp (vector->list x) free))
         (else free))))
    (define (expand-template tmpl vars)
      (let lp ((t tmpl) (dim 0) (ell-esc #f))
        (cond
         ((identifier? t)
          (cond
           ((find (lambda (v) (eq? t (car v))) vars)
            => (lambda (cell)
                 (if (<= (cdr cell) dim)
                     t
                     (error "too few ...'s"))))
           (else
            (list _rename (list _quote t)))))
         ((pair? t)
          (cond
           ((and (ellipsis-escape? t) (not ell-esc))
            (lp (if (and (pair? (cdr t)) (null? (cddr t))) (cadr t) (cdr t)) dim #t))
           ((and (ellipsis? t) (not ell-esc))
            (let* ((depth (ellipsis-depth t))
                   (ell-dim (+ dim depth))
                   (ell-vars (free-vars (car t) vars ell-dim)))
              (cond
               ((null? ell-vars)
                (error "too many ...'s"))
               ((and (null? (cdr (cdr t))) (identifier? (car t)))
                ;; shortcut for (var ...)
                (lp (car t) ell-dim ell-esc))
               (else
                (let* ((once (lp (car t) ell-dim ell-esc))
                       (nest (if (and (null? (cdr ell-vars))
                                      (identifier? once)
                                      (eq? once (car vars)))
                                 once ;; shortcut
                                 (cons _map
                                       (cons (list _lambda ell-vars once)
                                             ell-vars))))
                       (many (do ((d depth (- d 1))
                                  (many nest
                                        (list _apply _append many)))
                                 ((= d 1) many))))
                  (if (null? (ellipsis-tail t))
                      many ;; shortcut
                      (list _append many (lp (ellipsis-tail t) dim ell-esc))))))))
           (else (list _cons3 (lp (car t) dim ell-esc) (lp (cdr t) dim ell-esc) (list _quote t)))))
         ((vector? t) (list _list->vector (lp (vector->list t) dim ell-esc)))
         ((null? t) (list _quote '()))
         (else t))))
    (list
     _er-macro-transformer
     (list _lambda (list _expr _rename _compare)
           (list
            _car
            (cons
             _or
             (append
              (map
               (lambda (clause) (expand-pattern (car clause) (cadr clause)))
               forms)
              (list
               (list _cons
                     (list _error "no expansion for"
                           (list (rename 'strip-syntactic-closures) _expr))
                     #f)))))))))

(define-syntax syntax-rules
  (er-macro-transformer
   (lambda (expr rename compare)
     (syntax-rules-transformer expr rename compare))))




;; その他

(define (call-with-current-continuation f)
  (%call-with-current-continuation f))
(define call/cc call-with-current-continuation)

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
		(begin (f (car l))
		  (recur f (cdr l))))))

(define-syntax unless
  (syntax-rules ()
	((_ cnd body ...) (if cnd #f (begin body ...)))))

(define-syntax when 
  (syntax-rules ()
	  ((_ ?cnd ?body ...) (if ?cnd (begin ?body ...)))))

;; (puts obj1 ...)
(define (puts . x)
  (if (not (pair? x))
	  (display end-of-line)
	  (begin 
	    (display (car x))
	    (display " ")
	    (apply puts (cdr x)))))

;; #p macro
(define (*tee* x)
  (puts "tee:" x)
  x)

;; require
(define %require-loaded-modules '())

(define (%require mod)
  ;; check already loaded
  (define (loaded? lis)
	(if (null? lis) #f
		(if (eqv? mod (car lis)) #t
			(loaded? (cdr lis)))))
  (if (loaded? %require-loaded-modules) #f
      (begin
	    ;; not yet load
	    (set! %require-loaded-modules (cons mod %require-loaded-modules))
	    (let loop ((paths runtime-load-path))
		  (if (null? paths)
			  (error "cannot require" mod)
			  (let ((path (string-append (car paths) "/" (symbol->string mod) ".scm")))
			  	  (if (file-exists? path)
				  (load path)
				  (loop (cdr paths)))))))))

#;(define-syntax require
  (syntax-rules ()
	((_ ?mod) (%require '?mod))))


#;(define-syntax when 
  (syntax-rules ()
	  ((_ ?cnd ?body ...) (if ?cnd (begin ?body ...)))))

;; values

(define (values . x)
  (if (null? (cdr x)) (car x) (cons 'VALUES x)))

(define (call-with-values v f)
  (let ((v (apply v)))
	(if (and (pair? v) (eq? 'VALUES (car v)))
		(apply f (cdr v))
		(f v))))

(define-syntax receive
  (syntax-rules ()
	((_ ?formals ?expression ?body ...)
	 (call-with-values (lambda () ?expression)
	   (lambda ?formals ?body ...)))))

(define-syntax let-values
  (syntax-rules ()
	((_ (((?args ...) ?val)) ?body ...)
	 (call-with-values (lambda () ?val)
	   (lambda (?args ...) ?body ...)))
	((_ (((?args ...) ?val) ?rest ...) ?body ...)
	 (call-with-values (lambda () ?val)
	   (lambda (?args ...)
	  	 (let-values (?rest ...) ?body ...))))
	))

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

;; numbers

;; TODO: 非正確さの伝搬が実装されていない
(define (max . args)
  (let loop ((memo (car args)) (rest (cdr args)))
    (if (null? rest)
      memo
      (let ((cur (car rest)))
        (if (> memo cur)
          (loop memo (cdr rest))
          (loop cur (cdr rest)))))))

(define (min . args)
  (let loop ((memo (car args)) (rest (cdr args)))
    (if (null? rest)
      memo
      (let ((cur (car rest)))
        (if (> memo cur)
          (loop memo (cdr rest))
          (loop cur (cdr rest)))))))

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

(define (eval code env)
  ((%eval-compile code env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SRFI-0

(define *features* '())

(define-syntax cond-expand
  (er-macro-transformer
   (lambda (expr rename compare)
     (define (check x)
       (if (pair? x)
           (case (car x)
             ((and) (every check (cdr x)))
             ((or) (any check (cdr x)))
             ((not) (not (check (cadr x))))
             ((library) (eval `(find-module ',(cadr x)) (%meta-env)))
             (else (error "cond-expand: bad feature" x)))
           (memq (identifier->symbol x) *features*)))
     (let expand ((ls (cdr expr)))
       (cond
        ((null? ls))  ; (error "cond-expand: no expansions" expr)
        ((not (pair? (car ls))) (error "cond-expand: bad clause" (car ls)))
        ((eq? 'else (identifier->symbol (caar ls)))
         (if (pair? (cdr ls))
             (error "cond-expand: else in non-final position")
             `(,(rename 'begin) ,@(cdar ls))))
        ((check (caar ls)) `(,(rename 'begin) ,@(cdar ls)))
        (else (expand (cdr ls))))))))

(define (identifier->symbol x) x)

(cond-expand 
  (threads 2)
  (else 1))

))
