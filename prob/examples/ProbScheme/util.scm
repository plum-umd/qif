(define (list-max l)
  (fold-right max 0 l))

(define (det-and-norm d)
  (begin (distribution/determine! d)
	 (let ((ret (distribution/normalize d)))
	   (begin (distribution/determine! ret)
		  ret))))

(define (make-det-and-norm t)
  (det-and-norm (stochastic-thunk->distribution t)))

(define (range low high)
  (if (equal? low high) (cons low '()) (cons low (range (+ low 1) high))))

(define (uniform low high) (uniform-select (range low high)))

(define (flip p) (discrete-select (#t p) (#f (- 1 p))))

(define (gen-string o)
  (let* ((out (open-output-string)))
    (begin
      (display o out)
      (get-output-string out))))

(define (gen-compare l1 l2)
  (cond
   ((equal? l1 l2) #f)
   ((symbol? l1) (symbol<? l1 l2))
   ((string? l1) (string<? l1 l2))
   ((number? l1) (< l1 l2))
   ((pair? l1) (or (gen-compare (car l1) (car l2))
		   (and (equal? (car l1) (car l2))
			(gen-compare (cdr l1) (cdr l2)))))
   ((list? l1) (cond ((null? l1) #f)
		     (else (or (gen-compare (car l1) (car l2))
			       (and (equal? (car l1) (car l2))
				    (gen-compare (cdr l1) (cdr l2)))))))))

(define (get-sorted-elements adist)
  (begin ;(distribution/determine! adist)
	 (let* ((elements-temp (distribution->current-density-alist adist))
		(elements (sort elements-temp (lambda (a b) (gen-compare (car a) (car b))))))
	   elements)))

(define (print-dist adist)
  (let ((elements (get-sorted-elements adist)))
    (for-each (lambda (element)
		(let ((val (car element))
		      (prob (cdr element)))
		  (begin
		    (display val)
		    (display "\t")
		    (display prob)
		    (newline))))
	      elements)))

(define (graphics-draw-rect gd x1 y1 x2 y2)
  (begin
    (graphics-draw-line gd x1 y1 x2 y1)
    (graphics-draw-line gd x2 y1 x2 y2)
    (graphics-draw-line gd x2 y2 x1 y2)
    (graphics-draw-line gd x1 y2 x1 y1)))

(define font-name "-misc-fixed-medium-r-normal--6-60-75-75-c-40-iso8859-16")

(define (make-gd)
  (let* ((gd (make-graphics-device 'x #f (x-geometry-string 10 10 750 400)))
	 (font (graphics-operation gd 'font-structure font-name)))
    (begin
;      (graphics-operation gd 'set-font (x-font-structure/name font))
      (graphics-operation gd 'set-background-color "gray")
      (graphics-operation gd 'set-foreground-color "black")
      gd)))

(define (draw-dist adist)
  (let* ((gd (make-gd))
	 (elements (get-sorted-elements adist))
	 (num-elements (length elements))
	 (bar-width (/ 2 num-elements))
	 (max-prob (list-max (map cdr elements)))
	 (yscale (* 0.97 (/ 2 max-prob))))
    (begin
      (print-dist adist)
      (fold-left
       (lambda (pos e)
	 (begin 
		(let ((label (gen-string (car e)))
		      (prob (cdr e))
		      (xpos (- (* pos bar-width) 1)))
		  (begin
		    (graphics-draw-text gd xpos -1 label)
		    (graphics-draw-rect gd xpos -0.94 (+ xpos bar-width) (+ -0.94 (* prob yscale))) 
		    (+ pos 1)))))
	 0 elements)
       (graphics-flush gd))))
