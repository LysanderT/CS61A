
(define-macro (def func args body)
    (list `define (cons func args)
      body)
)


(define (map-stream f s)
    (if (null? s)
    	nil
    	(cons-stream (f (car s)) (map-stream f (cdr-stream s)))))


(define all-three-multiples
  (map-stream (lambda (x) (+ x 3)) (cons-stream 0 all-three-multiples))
)


(define (compose-all funcs)
  (if (null? funcs)
    (lambda (x) x)
    (lambda (x) ((compose-all (cdr funcs)) ((car funcs) x)))
  )
)


(define (partial-sums stream)
    (define (helper result s)
        (if (null? s)
            ()
            (cons-stream (+ result (car s)) (helper (+ result (car s)) (cdr-stream s)))   
        )
    )
    (helper 0 stream)
)

