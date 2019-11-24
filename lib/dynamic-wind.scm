(import (scheme base))

;;************************************************************
;; dynamic-wind from tiny-scheme
;; URL: http://tinyscheme.sourceforge.net/
;;************************************************************

;;;;;Helper for the dynamic-wind definition.  By Tom Breton (Tehom)
(define (shared-tail x y)
   (let ((len-x (length x))
         (len-y (length y)))
      (define (shared-tail-helper x y)
         (if
            (eq? x y)
            x
            (shared-tail-helper (cdr x) (cdr y))))

      (cond
         ((> len-x len-y)
            (shared-tail-helper
               (list-tail x (- len-x len-y))
               y))
         ((< len-x len-y)
            (shared-tail-helper
               x
               (list-tail y (- len-y len-x))))
         (#t (shared-tail-helper x y)))))

;;;;;Dynamic-wind by Tom Breton (Tehom)

;;Guarded because we must only eval this once, because doing so
;;redefines call/cc in terms of old call/cc

;;These functions are defined in the context of a private list of
;;pairs of before/after procs.
(define *active-windings* '())

;;Poor-man's structure operations
(define before-func car)
(define after-func  cdr)
(define make-winding cons)

;;Manage active windings
(define (activate-winding! new)
	((before-func new))
	(set! *active-windings* (cons new *active-windings*)))

(define (deactivate-top-winding!)
	(let ((old-top (car *active-windings*)))
	;;Remove it from the list first so it's not active during its
	;;own exit.
	(set! *active-windings* (cdr *active-windings*))
	((after-func old-top))))

(define (set-active-windings! new-ws)
	(unless (eq? new-ws *active-windings*)
	(let ((shared (shared-tail new-ws *active-windings*)))

		;;Define the looping functions.
		;;Exit the old list.  Do deeper ones last.  Don't do
		;;any shared ones.
		(define (pop-many)
			(unless (eq? *active-windings* shared)
				(deactivate-top-winding!)
				(pop-many)))
		;;Enter the new list.  Do deeper ones first so that the
		;;deeper windings will already be active.  Don't do any
		;;shared ones.
		(define (push-many new-ws)
			(unless (eq? new-ws shared)
				(push-many (cdr new-ws))
				(activate-winding! (car new-ws))))

		;;Do it.
		(pop-many)
		(push-many new-ws))))

(define old-c/cc call/cc)

;;The definitions themselves.
(define call-with-current-continuation
	;;It internally uses the built-in call/cc, so capture it.
	(lambda (func)
		;;Use old call/cc to get the continuation.
		(old-c/cc
			(lambda (continuation)
				;;Call func with not the continuation itself
				;;but a procedure that adjusts the active
				;;windings to what they were when we made
				;;this, and only then calls the
				;;continuation.
				(func
					(let ((current-ws *active-windings*))
					(lambda x
						(set-active-windings! current-ws)
						(apply continuation x))))))))

;;We can't just say "define (dynamic-wind before thunk after)"
;;because the lambda it's defined to lives in this environment,
;;not in the global environment.
(define dynamic-wind
	(lambda (before thunk after)
			;;Make a new winding
			(activate-winding! (make-winding before after))
			(let ((result (thunk)))
			;;Get rid of the new winding.
			(deactivate-top-winding!)
			;;The return value is that of thunk.
			result)))

(define call/cc call-with-current-continuation)

(define-syntax try
  (syntax-rules ()
	((_ ?error ?body ...)
	 (call/cc (lambda (*try-exit*)
				(let ((*old-error* error)
					  (*new-error* (lambda x (*try-exit* (apply ?error x)))))
				  (dynamic-wind
					  (lambda () (set! error *new-error*))
					  (lambda () ?body ...)
					  (lambda () (set! error *old-error*)))))))
	))
