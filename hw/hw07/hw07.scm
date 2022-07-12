(define (cddr s)
  (cdr (cdr s)))


(define (cadr s)
  (car (cdr s))
)

(define (caddr s)
  (car (cdr (cdr s)))
)


(define (sign num)
  (cond ((> num 0) 1)
    ((< num 0) -1)
    (else 0))
)


(define (square x) (* x x))

(define (pow x y)
  (if (= y 1)
    x
    (if (even? y)
      (square (pow x (/ y 2)))
      (* x (square (pow x (/ (- y 1) 2)) ))
    )
  )
)


(define (contains list x)
	(cond [(null? list) #f]
		[(equal? (car list) x) #t]
		[else (contains (cdr list) x)]))


(define (unique s)
; method 1
  (define (helper s lst)
    (if (null? s) lst
      (if (contains lst (car s))
        (helper (cdr s) lst)
        (helper (cdr s) (append lst (list (car s))))
      )
    )
  )
  (helper s ())
)

(define (unique s)
; method 2
  (if (null? s) nil 
    (cons
      (car s) 
      (unique (filter (lambda (x) (not (equal? x (car s)))) (cdr s)))
    )
  )
)


(define (replicate x n)
  (define (helper x n lst)
    (if (= n 0)
      lst
      (helper x (- n 1) (append lst (list x)))
    )
  )
  (helper x n ())
)


(define (accumulate combiner start n term)
  (if (= n 0)
    start
    (combiner (term n) (accumulate combiner start (- n 1) term))
  )
)


(define (accumulate-tail combiner start n term)
  (define (helper result combiner n term)
    (if (= n 0)
      result
      (helper (combiner (term n) result) combiner (- n 1) term)
    )  
  )
  (helper start combiner n term)
)


(define-macro (list-of map-expr for var in lst if filter-expr)
  `(map (lambda (,var) ,map-expr) (filter (lambda (,var) ,filter-expr) ,lst))
)