#lang racket


(define (monDecr n)
  (if (= (quotient n 10) 0) #t
      (and (<= (remainder n 10) (remainder (quotient n 10) 10))
           (monDecr (quotient n 10)))))

(define (f1 n)
  (define (f1-help i n)
    (if (= i n) (sin (expon n n))
        (sin (+ (expon i i)
                (f1-help (+ i 1) n)))))
  (f1-help 1 n))

(define (sum1 n)
  (if (= n 1) 1
      (+ (/ 1 (* n n)) (sum1 (- n 1)))))

(define (sum1-1 n)
  (define (iter res i)
    (if (> i n) res
        (iter (+ (/ 1 (* n n)) res) (+ i 1))))
  (iter 0 1))

(define (comb n m)
  (if (or (= m 0) (= n m)) 1
      (+ (comb (- n 1) (- m 1))
         (comb (- n 1) m))))

(define (ermit n x)
  (cond ((= n 0) 1)
        ((= n 1) (* 2 x))
        (else (- (* 2 x (ermit (- n 1) x))
                 (* 2 (- n 1) (ermit (- n 2) x))))))

(define (acc op nv a b term next)
  (if (> a b) nv
      (op (term a) (acc op nv (next a) b term next))))

(define (product a b term next)
  (acc * 1 a b term next))

(define (1+ n) (+ n 1))

(define (variations n m)
  (define (termV x) (- n x))
  (product 0 (- m 1) termV 1+))

(define (C n m)
  (define (termN) (- n x))
  (define (termM) (- m x))
  (quotient (product 0 (- m 1) termN 1+)
            (product 0 (- m 1) termM 1+)))

(define (filterAcc pred op nv a b term next)
  (cond ((> a b) nv)
        ((pred (term a)) (op (term a) (filterAcc pred op nv (next a) b term next)))
        (else (filterAcc pred op nv (next a) b term next))))

(define (filterAccIter pred op nv a b term next)
  (define (iter i s)
    (cond ((> i b) s)
          ((pred (term i)) (iter (next i) (op (term i) s)))
          (else (iter (next i) s))))
  (iter a nv))

(define (prod1 n)
  (define (pred x)
    (= (remainder x 4) 2))
  (define (id x) x)
  (filterAccIter pred * 1 1 n id 1+))

(define (prod5 n)
  (define (seekFive n)
    (or (= (remainder n 10) 5)
        (seekFive (quotient n 10))))
  (filterAccIter seekFive * 1 1 n id 1+))

(remainder 5 10)

(define (found a b term next rel x)
  (cond ((> a b) #f)
        ((rel x (term a)) #t)
        (else (found (next a) b term next rel x))))

(define (eqTerm n x)
  (define (term i)
    (+ (* i i i i)
       (* 3 i n n)
       (* n n n n)))
    (found 1 n term 1+ = x))

(define (divis n x)
  (define (term i)
    (+ (* i i i i)
       (* 3 i n n)
       (* n n n n)))
  (define (divRel p q)
    (= (remainder p q) 0))
  (found 1 n term 1+ = x))


(define (primeFound n)
  (define (id x) x)
  (define (div p q)
    (= (remainder p q) 0))
  (found 2 n id 1+ div n))






