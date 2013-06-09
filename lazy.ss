;;---------------------------------
;;      lazy stream in sicp
;;---------------------------------
(define-syntax delay-my 
  (lambda (x)
    (syntax-case x ()
      [(delay-my a)
       (syntax (lambda () a))])))

(define-syntax force-my
  (lambda (x)
    (syntax-case x ()
      [(force-my a)
       (syntax (a))])))

(define-syntax cons-stream
  (lambda (x)
    (syntax-case x ()
      [(cons-stream a b)
       (syntax (cons a (delay-my b)))])))

(define stream-car
  (lambda (stream)
    (car stream)))

(define stream-cdr
  (lambda (stream)
    (force-my (cdr stream))))

(define stream-ref
  (lambda (s n)
    (if (= n 0)
	(stream-car s)
	(stream-ref (stream-cdr s)
		    (- n 1)))))

(define stream-null? null?)

(define empty-stream '())

(define stream-map
  (lambda (proc s)
    (if (stream-null? s)
	empty-stream
	(cons-stream (proc (stream-car s))
		     (stream-map proc (stream-cdr s))))))

(define stream-for-each
  (lambda (proc s)
    (if (stream-null? s)
	'done
	(begin
	  (proc (stream car))
	  (stream-for-each proc (stream-cdr s))))))

(define stream-enumerate-interval
  (lambda (low high)
    (if (> low high)
	empty-stream
	(cons-stream
	 low
	 (stream-enumerate-interval (+ low 1) high)))))


(define stream-filter
  (lambda (pred stream)
    (cond 
     [(stream-null? stream) empty-stream]
     [(pred (stream-car stream))
      (cons-stream (stream-car stream)
		   (stream-filter pred (stream-cdr stream)))]
     [else (stream-filter pred (stream-cdr stream))])))s