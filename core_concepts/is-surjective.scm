#lang racket
(require rackunit rackunit/text-ui)

(define (toBinary n)
  (if (= n 0) 0
      (+ (remainder n 2)
         (* 10 (toBinary (quotient n 2))))))

(define (toBinary* n)
  (define (work acc i res)
    (if (= res 0) acc
        (work
         ;(+ res (* (remainder n 2) (expt 10 pos)))
         (+ acc (* (remainder n 2) (expt 10 i)))
         (+ i 1)
         (quotient res 2))))
  (work 0 0 n))


(toBinary 17)

(define (const c)
  (lambda (x) c))

(define (compose f g)
  (lambda (x)
    (f (g x))))


(define (id x) x)
(define (repeat n f)
  (if (= n 0) id
      (compose f (repeat (- n 1) f))))
(define (square x) (* x x))


(define dx 0.001)
(define (derive f)
  (lambda (x)
    (/ (- (f (+ x dx)) (f x)) dx)))

(define (twist k f g)
  (if (= k 0) id
      (lambda (x) (f (g ((twist (- k 2) f g) x))))
      ; алтернативно - (compose f (compose g (twist ...)))
))
(define (derive-n n f)
  (if (= n 0) f
      (derive (derive-n (- n 1) f))))

(define (derive-n* n f)
  ((repeat n derive) f))




(define (middle-digits n)

  (define (last-digit n)
    (remainder n 10))
  (define (without-last n)
    (quotient n 10))

  (define (append-digit n d)
    (+ d (* n 10)))

  (define (digit-count n)
    (if (= (quotient n 10) 0) 1
        (+ 1 (digit-count (quotient n 10)))))

  (define (ith-digit n i)
    (if (= i 0)
        (last-digit n)
        (ith-digit (quotient n 10) (- i 1))))

  (let ((count (digit-count n)))
    (if (odd? count)
        -1
        (append-digit
         (ith-digit n (/ count 2) )
         (ith-digit n (- (/ count 2) 1) )))) )
                    

(define middle-digits-tests
  (test-suite
   "Tests for middle-digits"

   (check = (middle-digits 0) -1)
   (check = (middle-digits 1) -1)
   (check = (middle-digits 452) -1)
   (check = (middle-digits 45241) -1)

   (check = (middle-digits 42) 42)
   (check = (middle-digits 4712) 71)
   (check = (middle-digits 471239) 12)))

(run-tests middle-digits-tests)

(define (last-digit n)
  (remainder n 10))
  (define (ith-digit n i)
    (if (= i 0)
        (last-digit n)
        (ith-digit (quotient n 10) (- i 1))))
  (ith-digit 123 2)



(define (is-sur? domain codomain f)

  (define (for-all? l p?)
    (cond ((null? l) #t)
          ((p? (car l))
           (for-all? (cdr l) p?))
          (else #f)))

  (define (exists-classic? l p?)
    (cond ((null? l) #f)
          ((p? (car l)) #t)
          (else (exists-classic? (cdr l) p?))))

  (define (exists-negation? l p?)
    (not (for-all? l (lambda (x)
                       (not (p? x))))))

  (define (exists? l p?)
    (foldl (lambda (x acc)
             (or (p? x) acc)) #f l))

  ;;Bit misleading don't you think?
  (define transforms?
    (for-all? domain
              (lambda (x)
                (member (f x) codomain))))
  (define covers?
    (for-all? codomain
              (lambda (y)
                (exists? domain (lambda (x)
                                  (= (f x) y))))))
  (and transforms? covers?)
 )


(define is-sur?-tests
  (test-suite
   "Tests for is-sur?"

   (check-true (is-sur? '() '() square))
   (check-true (is-sur? '(0 1 2 3) '(0 3 2 1) identity))
   (check-true (is-sur? '(0 1 -1 2) '(0 1 4) square))

   (check-false (is-sur? '() '(0 1 4) square))
   (check-false (is-sur? '(0 1 -1 2) '() square))
   (check-false (is-sur? '(0 1 2 3) '(0 1 2) identity))
   (check-false (is-sur? '(0 1 2) '(0 1 2 3) identity))
   (check-false (is-sur? '(0 1 -1 2 3) '(0 1 4) square))
   (check-false (is-sur? '(0 1 -1 2) '(0 1 4 9) square))))

(run-tests is-sur?-tests)