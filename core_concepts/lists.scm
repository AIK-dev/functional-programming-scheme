
(define (sumEven l)
  (cond ((null? l) 0)
        ((even? (car l)) (+ (car l) (sumEven (cdr l))))
        (else (sumEven (cdr l)))))

;Удвоено нечетно : (= (remainder n 4) 2)
(define (sum2odd l)
  (cond ((null? l) 0)
        ((= (remainder (car l) 4 ) 2)
         (+ (car l) (sum2odd (cdr l))))
        (else (sum2odd (cdr l)))))

(define (nth n l)
  (if (= n 0) (car l)
      (nth (- n 1) (cdr l))))

(define (leng l)
  (if (null? l) 0
      (+ 1 (leng (cdr l)))))
(define (last l)
  (nth (- (leng l) 1) l))

(define (atom? l) (and (not (pair? l)) (not (null? l))))
(atom? '())
(atom? +)

;This does a shallow count, wrong result
(define (countAtomsFail l)
  (cond ((null? l) 0)
        ((atom? (car l)) (+ 1 (countAtomsFail (cdr l))))
        (else (countAtomsFail (cdr l)))))

(define (countAtoms l)
  (cond ((null? l) 0)
        ((atom? l) 1)
        (+ (countAtoms (car l))
           (countAtoms (cdr l)))))

;This fails, because atom? doesnt allow '()
(define (dr l)
  (if (atom? l) l
      (append (dr (cdr l)) (dr (car l)))))

(define (deepRev l)
  (cond ((null? l) '())
        ((atom? l) l)
        (else (append (deepRev (cdr l)) (list (deepRev (car l)))))))

(deepRev '(1 (2 3) 4 (5 (6 7))))

;Traversal is not in reversed order
(define (oddList l)
  (define (iter i xs res)
    (cond ((null? xs) res)
          ((odd? i) (iter (+ i 1) (cdr xs) (cons (car xs) res)))
          (else (iter (+ i 1) (cdr xs) res))))
  (iter 0 l '()))

;Traversal in reversed order
(define (oddListReverse l)
  (define (iter i y)
    (cond ((< i 0) y)
          ((odd? i)
           (iter (- i 1) (cons (nth i l) y)))
          (else (iter (- i 1) y))))
  (iter (- (leng l) 1) '()))

(define (reverse1 l)
  (define (riv x y)
    (if (null? x) y
        (riv (cdr x) (cons (car x) y))))
  (riv l '()))

(define (my-reverse l)
  (if (null? l) '()
      (append (my-reverse (cdr l))
              (list (car l)))))

(define (my-rev-iter l)
  (define (iter acc xs)
    (if (null? xs) acc
        (iter (cons (car xs) acc) (cdr xs))))
  (iter '() l))

(define (sqList l)
  (define (sq x) (* x x))
  (define (iter xs res)
    (if (null? xs) (reverse res)
        (iter (cdr xs) (cons (sq (car xs)) res))))
  (iter l '()))
(sqList '(1 2 3 4 5))

(define (scan l)
  (define (iter xs count sum prod)
    (cond ((null? xs) (list count sum prod))
          ((= (car xs) 0)
           (iter (cdr xs) (+ 1 count) sum prod))
          ((< (car xs) 0)
           (iter (cdr xs) count (+ (car xs) sum) prod))
          ((> (car xs) 0)
           (iter (cdr xs) count sum (* prod (car xs))))
          (else (iter (cdr xs) count sum prod))))
  (iter l 0 0 1))

(define (horner l x)
  (if (null? l) x
      (+ (car l) (* x (horner (cdr l) x)))))

(define (put l s)
  (if (null? l) (list s)
      (cons (car l) (put (cdr l) s))))

(define (red l)
  (cond ((null? l) '())
        ((null? (cdr l)) '())
        (else (cons (car l) (red (cdr l))))))

(define (flatten l)
  (cond ((null? l) '())
        ((atom? l) (list l))
        (else (append (flatten (car l)) (flatten (cdr l))))))

(flatten '((1 2 3) (2 (3 (5 (7 8) 6) 4))))

;Minimum element of list
(define (minlist l)
  (define (iter xs res)
    (cond ((null? xs) res)
          ((< (car xs) res) (iter (cdr xs) (car xs)))
          (else (iter (cdr xs) res))))
  (cond ((null? l) '())
        ((null? (cdr l)) (car l))
        (else (iter (cdr l) (car l)))))

(define (minRec l)
  (cond ((null? l) '())
        ((null? (cdr l)) (car l))
        ((< (car l) (minRec (cdr l)))
         (car l))
        (else (minRec (cdr l)))))

(define (eql l1 l2)
  (cond ((and (null? l1) (null? l2)) #t)
        ((not (= (leng l1) (leng l2))) #f)
        ((not (= (car l1) (car l2))) #f)
        (else (eql (cdr l1) (cdr l2)))))

(define (my-member x l)
  (if (null? l) #f
      (or (= x (car l)) (my-member x (cdr l)))))
(define (union l1 l2)
  (cond ((null? l1) l2)
        ((member (car l1) l2)
         (union (cdr l1) l2))
        (else (cons (car l1) (union (cdr l1) l2)))))

(union '(1 2 3 4) '(2 4 5))

;;Associative lists

(define (make-alist f keys)
  (if (null? keys) '()
      (cons (cons (car keys) (f (car keys)))
            (make-alist f (cdr keys)))))

(define (make-alist-map f keys)
  (map (lambda (key)
         (cons key (f key))) keys))

(define (make-alist f keys)
  (if (null? keys) '()
      (cons (list (car keys)
                  (f (car keys)))
            (make-alist f (cdr keys)))))

(define (make-alist f keys)
  (map (lambda (key)
         (list key (f key))) keys))

(define (my-keys al) (map car al))
(define (my-values al) (map cdr al))

(define al (make-alist (lambda (x) (* x x)) '(1 16 4 1 3 5 3 5)))
(define (filter p? l)
  (cond ((null? l) l)
        ((p? (car l)) (cons (car l) (filter p? (cdr l))))
        (else (filter p? (cdr l)))))


(define (filt p l)
  (cond ((null? l) '())
        ((p (car l))
         (cons (car l) (filt p (cdr l))))
        (else (filt p (cdr l)))))

(define (del-assoc key al)
  (cond ((null? al) '())
        ((equal? (caar al) key)
         (del-assoc key (cdr al)))
        (else (cons (car al)
                    (del-assoc key (cdr al))))))
(define (del-assoc key al)
  (filter (lambda (kv)
            (not (equal? (car kv) key))) al))

(define (search p l)
  (and (not (null? l))
       (or (p (car l))
           (search p (cdr l)))))

(define (exists? p? l)
  (not (null? (filter p? l))))
(define (all? p? l)
  (not (search (lambda (x)
                 (not (p x)))) l))