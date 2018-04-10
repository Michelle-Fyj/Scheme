
(define (null-ld? obj)
	; is it a pair? 
	(if (pair? obj) 
		; if yes, then eq?
		(eq? (car obj)
			(cdr obj)
	    )
	; else, false
	#f)
	)

(define (ld? obj)
	; is it a pair?
	(if (pair? obj)
		; if yes, then 
	    (cond [(eq? (car obj) (cdr obj)) #t]
		  [(not (pair? (car obj))) #f]
		  [else (ld? (cdr-ld obj))])
	    #f)
	)

(define (cons-ld obj listdiff)
	; first element is obj
	(cons (cons obj (car listdiff)) (cdr listdiff))
	)

(define (car-ld listdiff)

					;  (cond [(and (ld? listdiff) (not (null-ld? listdiff)))
  (car (car listdiff))
					;])
  )


(define (cdr-ld listdiff)
	; everything except car-ld
					;(cond [(and (ld? listdiff) (not (null-ld? listdiff)))
  (cons (cdr (car listdiff))
	(cdr listdiff))
					;])
)

(define ld (lambda obj (cons obj '())))

;(define (ld obj . args) (cons (cons obj args) '()))

(define (length-ld listdiff)
	; if listdiff is empty
	(if (null-ld? listdiff)
	0
	(+ 1 (length-ld (cdr-ld listdiff)))))

(define append-ld (lambda listdiff
		    (cond [(null? listdiff) (cons listdiff '())]
			  [(eq? listdiff '()) listdiff]
			  [else (append-ld-helper (reverse listdiff))])))


(define (append-ld-helper manylistdiff)
	(if (pair? manylistdiff) (cons (append (cdr manylistdiff) (car (car manylistdiff))) (cdr (car manylistdiff)))

		(append-ld-helper (cons (cons (append (car (cdr manylistdiff)) (car (car manylistdiff)))
						(cdr (car manylistdiff))) (cdr (cdr manylistdiff))))))

(define (ld-tail listdiff k)
	; if k=0 no more element needs to be deleted
	(if (equal? k 0) listdiff
		(ld-tail (cdr-ld listdiff) (- k 1))
		))

(define (list->ld list)
	(cons list '()))

(define (ld->list listdiff)
  (if (null-ld? listdiff) '()
   (cons (car-ld listdiff) (ld->list (cdr-ld listdiff)))))




(define (map-ld proc . listdiff)
  (if (null-ld? (car listdiff))
      (cons '() '())
      (cons-ld (apply proc (map car-ld listdiff))
	       (apply map-ld (cons proc (map cdr-ld listdiff))))))

 



(define (expr2ld expr)
	(if (eq? expr '()) '()
		(if (not (list? expr)) (expr2ld-helper-match expr)
			(cons (expr2ld (car expr)) (expr2ld (cdr expr))))))

(define (expr2ld-helper-match element)
	(cond [(equal? element 'null?) 'null-ld?]
		  [(equal? element 'list?) 'ld?]
		  [(equal? element 'cons) 'cons-ld]
		  [(equal? element 'car) 'car-ld]
		  [(equal? element 'cdr) 'cdr-ld]
		  [(equal? element 'list) 'ld]
		  [(equal? element 'length) 'length-ld]
		  [(equal? element 'append) 'append-ld]
		  [(equal? element 'list-tail) 'ld-tail]
		  [(equal? element 'map) 'map-ld]
		  [else element])
	)




