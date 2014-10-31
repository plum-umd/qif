;(define (load-relative filename)
;  (with-working-directory-pathname 
;   (directory-namestring (current-load-pathname))
;   (lambda () (load filename))))

(set! load/suppress-loading-message? #t) (newline)

;(load-relative "load-probscheme")

(load "load-probscheme")
(load "util")

(define (make-secret-belief)
  (let ((byear (if (flip 1/4) (uniform 1900 1949) (uniform 1950 1999)))
	(gender (if (flip 1/2) 'male 'female)))
    (list gender byear)))

(define (make-secret-belief-large)
  (let ((byear (if (flip 1/4) (uniform 1600 1949) (uniform 1950 2599)))
	(gender (if (flip 1/2) 'male 'female)))
    (list gender byear)))

(define (make-secret) (list 'female 1985))

(define (query1 secret)
  (let ((gender (first secret))
	(byear (second secret)))
    (if (<= byear 1980) 0
	(if (equal? gender 'male) 1 2))))

(define (query2 secret)
  (let ((gender (first secret))
	(byear (second secret)))
    (if (equal? byear 1943) 0 1)))

(define (refine prebelief secret query)
  (let* ((outbelief (query prebelief))
	 (output (query secret)))
    (observe! (equal? outbelief output))
    prebelief))

(define (refine-output prebelief output query)
  (let* ((outbelief (query prebelief)))
    (observe! (equal? outbelief output))
    prebelief))

(define predist (stochastic-thunk->distribution make-secret-belief))
(distribution/determine! predist)

(define predist-large (stochastic-thunk->distribution make-secret-belief-large))
(distribution/determine! predist-large)

(define postdist
  (stochastic-thunk->distribution 
   (lambda () (refine (distribution-select predist)
		      (make-secret)
		      query1
		      ))))
(distribution/determine! postdist)

(define (dist-possibilities d)
  (map car (distribution->current-density-alist d)))

(define (max-possibility d)
  (list-max (map cdr (distribution->current-density-alist d))))

(define (check-min-ent prebelief query)
  (list-max (map (lambda (secret)
		   (max-possibility
		    (let ((postbelief (stochastic-thunk->distribution
				       (lambda () (refine (distribution-select prebelief) secret query)))))
		      (det-and-norm postbelief))))
		 (dist-possibilities prebelief))))  

(define (check-min-ent-output prebelief query)
  (let* ((postbelief (stochastic-thunk->distribution (lambda () (query (distribution-select prebelief)))))
	 (outputs (begin (distribution/determine! postbelief)
			 (dist-possibilities postbelief))))
    (list-max (map (lambda (output)
		     (max-possibility
		      (let ((postbelief
			     (stochastic-thunk->distribution
			      (lambda () (refine-output (distribution-select prebelief) output query)))))
			(det-and-norm postbelief))))
		   outputs))))

