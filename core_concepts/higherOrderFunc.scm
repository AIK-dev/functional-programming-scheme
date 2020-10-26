#lang racket

(define (acc op nv a b term next)
  (if (> a b) nv
      (op (term a) (acc op nv (next a ) b term next))))
(define (sum a b term next)
  (acc + 0 a b term next))
(define (product a b term next)
  (acc * 1 a b term next))

(define (filtAcc pred op nv a b term next)
  (cond ((> a b) nv)
        ((pred (term a)) (op (term a) (filtAcc pred op nv (next a) b term next)))
        (else (filtAcc pred op nv (next a) b term next))))
(define (filtAccIter pred op nv a b term next)
  (define (iter s i)
    (cond ((> i b) s)
          ((pred (term a)) (iter (op (term a) s) (next i)))
          (else (iter s (next i)))))
  (iter nv a))

(define (found a b term next rel x)
  (cond ((> a b) #f)
        ((rel (term a) x) #t)
        (found (next a) b term next rel x)))

(define (id x) x)
(define (1+ x) (+ x 1))
(define (variations n m)
  (define (termV x) (- n x))
  (product 0 (- m 1) id 1+))

(define (combinations n m)
  (define (termN x) (- n x))
  (define (termM x) (- m x))
  (quotient (product 0 (- m 1) termN 1+)
            (product 0 (- m 1) termM 1+)))

(define (prod1 n)
  (define (pred1 n)
    (= (remainder n 4) 2))
  (filtAcc pred1 * 1 1 n id 1+))

(define (prod5 n)
  (define (n5 n)
    (cond ((= n 0) #f)
    ((= (remainder n 10) 5) #t)
    (else (prod5 (quotient n 10)))))
  (filtAcc n5 * 1 1 n id 1+))

(define (prime n)
  (define (div p q)
    (= (remainder p q) 0))
  (not (found 2 (- n 1) id 1+ div n)))

(define (exist x n a b)
  (define (term i)
    (define (t1 k) (/ x k))
    (product 1 i term 1+))
  (define (rel x y)
    (and (>= x a) (<= x y)))
  (found a b term 1+ rel b))

(define (exp1 x n)
  (define (term i)
    (define (iter k res)
      (if (> k i) res
          (iter (+ k 1) (* res (/ x k)))))
    (iter 1 1))
  (sum 1 n term 1+))

(define (exp1ProdVer x n)
  (define (term i)
    (define (term k) (/ x k))
    (product 1 i term 1+))
  (sum 1 n term 1+))
;(=(exp1 17 122)
;(exp1ProdVer 17 122))

(define (sin x n)
  (define (term i)
    (define (t1 k) -1)
    (define (tx k) x)
    (/ (* (product 1 i t1 1+)
          (product 1 (+ (* 2 i) 1) tx 1+))
       (product 1 (+ (* 2 i) 1) id 1+)))
  (sum 1 n term 1+))



(define (cos x n)
  (define (term i)
    (define (t1 k) -1)
    (define (tx k) x)
    (/ (* (product 1 i t1 1+)
          (product 1 (* 2 i) tx 1+))
       (product 1 (* 2 i) id 1+)))
  (sum 0 n term 1+))


(define (f1 x n)
  (repeated (lambda (x) (+ (* x x) 1)) n))
(define (f2 x n)
  (repeated (lambda (x) (* x x x)) n))
(define (f3 x n)
  (repeated cos n))
(define (f4 x n)
  (repeated sin n))

;half-interval-method
(define (him f a b)
  (let ((a-val (f a))
        (b-val (f b)))
    (cond ((and (negative? a-val) (positive? b-val))
           (search f a b))
          ((and (negative? b-val) (positive? a-val))
           (search f b a))
          (else
           (error "Invalid val")))))
(define (search f neg pos)
  (let ((midpoint (avg neg pos)))
    (if (close-enough? neg pos)
        midpoint
        (let ((testVal (f midpoint)))
          (cond ((negative? testVal)
                 (search f midpoint pos))
                ((positive? testVal)
                 (search f neg testVal))
                (else midpoint))))))
(define (close-enough? x y)
  (< (abs (- x y)) 0.001))
(define (avg x y)
  (/ (+ x y) 2))

;Newton method
(define (improve f y)
  (- y (/ (f y)
          ((derive f 0.001) y))))
(define (good-enough? f y)
  (< (abs (f y)) 0.001))
(define (newton f y)
  (if (good-enough? f y) y
      (newton f (improve f y))))
;Fixed point with newton method
(define (fixed f y)
  (newton (lambda (x) (- (f x) x)) y))

;Iteration method for fixed point
(define (fp f guess)
  (if (close-enough? (f guess) guess)
      guess
      (fp f (f guess))))

;REPEATED 
(define (5+ n) (+ n 5))
(define (repeated1 f n)
  (lambda (x)
    (if (= n 1) (f x)
        (f ((repeated1 f (- n 1)) x)))))

(define (repeated f n)
  (lambda (x)
    (if (= n 0) x
        (f ((repeated f (- n 1)) x)))))
((repeated1 5+ 3) 2)

;DERIVE
(define (derive f dx)
  (lambda (x)
    (/ (- (f (+ x dx)) (f x))
       dx)))
;Base 1
(define (derive-n1 f n dx)
  (lambda (x)
    (if (= n 1) ((derive f dx) x)
        ((derive (derive-n1 f (- n 1) dx) dx) x))))
;Base 0
(define (deriven f n dx)
  (lambda (x)
    (if (= n 0) (f x)
        ((derive (deriven f (- n 1) dx) dx) x))))

;My version, letting only derive apply it to arguments
(define (derive-n f n dx)
  (if (= n 1) (derive f dx)
      (derive (derive-n f (- n 1) dx) dx)))
(define (deriven1 f n dx)
  (if (= n 0) f
      (derive (deriven f (- n 1) dx) dx)))

((deriven1 exp 10 0.001) 1)
((deriven1 exp 2 0.001) 1)

(define (smooth f dx)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))
(define (smoothN f n dx x)
  ((repeated (smooth f n) dx) x))

(define (pow x n)
  (acc * 1 1 n (lambda (i) x) 1+))

(define (fact n)
  (acc * 1 1 n id 1+))

(define (myExponent x n)
  (acc + 0. 0 n (lambda (i) (pow x i) (fact i)) 1+))

;Seek number in [a,b] with given property
(define (exists? a b pred)
  (acc (lambda (u v) (or u v)) (or) a b pred 1+))


(define (prime? n)
  (and (> n 1) (not (exists? 2 (sqrt n) (lambda (i) (= (remainder n i) 0))))))



