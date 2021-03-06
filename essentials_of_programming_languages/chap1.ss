;; 1.15
(define duple
  (lambda (n s)
    (if (zero? n)
	'()
	(cons s (duple (- n 1) s)))))

;; 1.16
(define invert
  (lambda (lst)
    (if (null? lst)
	'()
	(cons
	 (let ([l2 (car lst)])
	   (list (cadr l2) (car l2)))
	 (invert (cdr lst))))))
;; 1.17
(define down
  (lambda (lst)
    (if (null? lst)
	'()
	(cons
	 (list (car lst))
	 (down (cdr lst))))))

;; 1.18
(define swapper
  (lambda (s1 s2 lst)
    (if (null? lst)
	'()
	(cons
	 (swapper-aux s1 s2 (car lst))
	 (swapper s1 s2 (cdr lst))))))
(define swapper-aux
  (lambda (s1 s2 exp)
    (if (symbol? exp)
	(cond
	 [(eqv? exp s1) s2]
	 [(eqv? exp s2) s1]
	 [else exp])
	(swapper s1 s2 exp))))

;; 1.19
(define list-set-num
  (lambda (lst n x index)
    (if (null? lst)
	'()
	(cons
	 (if (= n index) x
	     (car lst))
	 (list-set-num (cdr lst) n x (+ index 1))))))

(define list-set
  (lambda (lst n x)
    (list-set-num lst n x 0)))

;; 1.20
(define count-occurrences
  (lambda (s slist)
    (if (null? slist)
	0
	(+ (count-occurrences-aux s (car slist))
	   (count-occurrences s (cdr slist))))))

(define count-occurrences-aux
  (lambda (s slist)
    (if (symbol? slist)
	(if (eqv? slist s) 1 0)
	(count-occurrences s slist))))
;; 1.21
(define product-aux
  (lambda (s slist)
    (if (null? slist)
	'()
	(cons
	 (list s (car slist))
	 (product-aux s (cdr slist))))))

(define product
  (lambda (sos1 sos2)
    (cond
     [(null? sos1) '()]
     [else (append
	    (product-aux (car sos1) sos2)
	    (product (cdr sos1) sos2))])))

;; 1.22
(define filter-in
  (lambda (pred lst)
    (if (null? lst)
	'()
	(if (pred (car lst))
	    (cons (car lst) (filter-in pred (cdr lst)))
	    (filter-in pred (cdr lst))))))

;; 1.23
(define list-index-aux
  (lambda (pred lst index)
    (if (null? lst)
	#f
	(if (pred (car lst)) index
	    (list-index-aux pred (cdr lst) (+ index 1))))))

(define list-index
  (lambda (pred lst)
    (list-index-aux pred lst 0)))

;; 1.24
(define every?
  (lambda (pred lst)
    (if (null? lst)
	#t
	(and (pred (car lst)) (every? pred (cdr lst))))))
;; 1.25
(define exists?
  (lambda (pred lst)
    (if (null? lst)
	#f
	(or (pred (car lst)) (exists? pred (cdr lst))))))

;; 1.26
(define up
  (lambda (lst)
    (if (null? lst)
	'()
	(if (list? (car lst))
	    (append (car lst) (up (cdr lst)))
	    (cons (car lst) (up (cdr lst)))))))
;; 1.27
(define flatten
  (lambda (slist)
    (if (null? slist)
	'()
	(append
	 (flatten-aux (car slist))
	 (flatten (cdr slist))))))
(define flatten-aux
  (lambda (exp)
    (if (symbol? exp)
	(list exp)
	(flatten exp))))

;; 1.28
(define merge
  (lambda (loi1 loi2)
    (cond
     [(null? loi1) loi2]
     [(null? loi2) loi1]
     [else
      (if (> (car loi1) (car loi2))
	  (cons
	   (car loi2)
	   (merge loi1 (cdr loi2)))
	  (cons
	   (car loi1)
	   (merge (cdr loi1) loi2)))])))
;; 1.29
(define sort
  (lambda (loi)
    (if (null? loi)
	'()
	(insert (car loi) (sort (cdr loi))))))
(define insert
  (lambda (i loi)
    (if (null? loi)
	(list i)
	(if (< i (car loi))
	    (cons i loi)
	    (cons (car loi) (insert i (cdr loi)))))))
;; 1.30
(define sort/predicate
  (lambda (pred loi)
    (if (null? loi)
	'()
	(insert/pred pred (car loi) (sort/predicate pred (cdr loi))))))
(define insert/pred
  (lambda (pred i loi)
    (if (null? loi)
	(list i)
	(if (pred i (car loi))
	    (cons i loi)
	    (cons (car loi) (insert/pred pred i (cdr loi)))))))

;; 1.31
(define leaf
  (lambda (x)
    (list 'leaf x)))

(define interior-node
  (lambda (symbol lnode rnode)
    (list 'node symbol lnode rnode)))

(define leaf?
  (lambda (node)
    (eq? (car node) 'leaf)))

(define lnode
  (lambda (node)
    (if (eq? 'node (car node))
	(caddr node))))

(define rnode
  (lambda (node)
    (if (eq? 'node (car node))
	(cadddr node))))

(define contents-of
  (lambda (node)
    (if (or (eq? 'node (car node))
	    (eq? 'leaf (car node)))
	(cadr node))))

;; 1.32
(define double-tree
  (lambda (tree)
    (if (leaf? tree)
	(leaf (* 2 (contents-of tree)))
	(interior-node (contents-of tree)
		       (double-tree (lnode tree))
		       (double-tree (rnode tree))))))

;; 1.33
(define mark-leaves-with-red-depth-aux
  (lambda (n tree)
    (if (leaf? tree)
	n
	(let ([n-t (if (eq? (contents-of tree) 'red)
		       (+ n 1)
		       n)])
	  (interior-node (contents-of tree)
			 (mark-leaves-with-red-depth-aux n-t (lnode tree))
			 (mark-leaves-with-red-depth-aux n-t (rnode tree)))))))
(define mark-leaves-with-red-depth
  (lambda (tree)
    (mark-leaves-with-red-depth-aux 0 tree)))

;; 1.34
(define path
  (lambda (n tree)
    (if (null? tree)
	'()
        (let ([content (car tree)])
	  (cond
	   [(< n content) (cons 'left (path n (cadr tree)))]
	   [(> n content) (cons 'right (path n (caddr tree)))]
	   [else '()])))))

;; 1.35
(define number-leaves-aux
  (lambda (node index)
    (if (leaf? node)
	(list index '())
	(let* ([left (number-leaves-aux (lnode node) index)]
	       [right (number-leaves-aux (rnode node) (+ 1 (car left)))])
	  (list (car right) 
		(interior-node (contents-of node)
			       (if (leaf? (lnode node))
				   (car left)
				   (cadr left))
			       (if (leaf? (rnode node))
				   (car right)
				   (cadr right))))))))
(define number-leaves
  (lambda (node)
    (cadr (number-leaves-aux node 0))))

; method2
(define (counter start)
  (lambda ()
    (set! start (+ 1 start))
    start))

(define number-leaves-aux
  (lambda (node counter)
    (if (leaf? node)
	(counter)
	(interior-node (contents-of node)
		       (number-leaves-aux (lnode node) counter)
		       (number-leaves-aux (rnode node) counter)))))
(define number-leaves
  (lambda (node)
    (number-leaves-aux node (counter -1))))
;; 1.36
(define g
  (lambda (l2 lst)
    (define g-t
      (lambda (l)
	(if (null? l)
	    '()
	    (cons (list (+ 1 (caar l)) (cadar l)) (g-t (cdr l))))))
    (cons l2 (g-t lst))))
