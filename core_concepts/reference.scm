

(define (expir x n)
  (define (iter i res)
    (cond ((> n 0) (if (> i n) res
                       (iter (+ i 1) (* x res))))
          ((< n 0) (if (< i (- 0 n)) res
                       (iter (+ i 1) (/ res x))))
          (else 1)))
  (iter 1 1))

(define (foldr op nv lst)
  (if (null? lst) nv
      (op (car lst)
          (foldr op nv (cdr lst)))))

(define (decToBin n)
  (if (= n 0) 0
      (+ (remainder n 2) (* 10 (decToBin (quotient n 2))))))
(define (toDecimal number)
	(if (< number 1)
		0
		(+ (remainder number 2) (* 10 (toDecimal (quotient number 2))))))

(define (binToDecIter n)
  (define (iter acc deg rest)
    (if (= rest 0) acc
        (iter (+ acc (* (expir 2 deg) (remainder rest 10))) (+ deg 1) (quotient rest 10))))
  (iter 0 0 n))
(binToDecIter 111)
(define (constantly c)
  (lambda (x) c))

(define (flip f)
  (lambda (x y)
    (f y x)))


(define (fiter n)
  (define (it i res)
    (if (> i n) res
        (it (+ i 1) (* i res))))
  (it 1 1))

(define (factorial-iter number)
	(define (helper number accum)
		(if (< number 2)
			accum
			(helper (- number 1) (* accum number))))
	(helper number 1))

(define (gcdd a b)
  (cond ((= a b) b)
        ((> a b) (gcdd (- a b) b))
        (else (gcdd a (- b a)))))

(define (prime? numb)
  (define (iter potential n)
    (if (>= potential n)
        #t
        (and
         (not (= (remainder n potential) 0))
         (iter (+ potential 1) numb))))
  (iter 2 numb))
(define (enumeratePrimes n)
  (define (iter i lst)
    (cond ((> i n) lst)
          ((prime? i) (iter (+ i 1) (cons i lst)))
          (else (iter (+ i 1) lst))))
  (iter 2 '()))

(define (rev n)
  (define (iter acc rest)
    (if (= rest 0) acc
        (iter (+ (* acc 10) (remainder rest 10)) (quotient rest 10))))
  (iter 0 n))

;Detect if number contains a palindrome
;This section merits revisiting...
(define (last n)
  (remainder n 10))
(define (next-to-last n)
  (last (quotient n 10)))
(define (second-to-last n)
  (next-to-last (quotient n 10)))

(last           123)
(next-to-last   123)
(second-to-last 123)
;First version, incorrect logic, if we have 3-dig number
;it doesnt check if the last two are the same
(define (ends-with-palindrome n)
  (if (> 99)
      (= (last n) (second-to-last n))
      (if (> 9)
          (= (last n) (next-to-last n))
          #f)))
;Second version , checks both cases
(define (ends-with-palindrome? number)
	(or 
		(and (> number 9)
			 (= (last number) (next-to-last number)))
		(and (> number 99)
			 (= (last number) (second-to-last number)))))
(define (contains-palindrome? number)
	(if (< number 10) 
		#f
		(or 
			(ends-with-palindrome? number)
			(contains-palindrome? (quotient number 10)))))

(define (contains-palindrome n)
  (if (< n 10) #f
      (or (ends-with-palindrome? n)
          (ends-with-palindrome? (quotient n 10)))))

;(ends-with-palindrome? 44)
;(contains-palindrome? 44123)

;Testing equivalence of both end-with-palindrome versions
(define (my-test-method f g n)
  (define (iter i lst)
    (cond ((> i n) lst)
          ((not (equal? (f i)
                (g i)))
           (iter (+ i 1) (cons i lst)))
          (else (iter (+ i 1) lst))))
  (iter 2 '()))
(ends-with-palindrome? 299)
(ends-with-palindrome 299)
;(my-test-method ends-with-palindrome? ends-with-palindrome 1888)

(contains-palindrome? 44123)
(ends-with-palindrome 1388)


(define (increasing-digits? n)
  (if (< n 10) #t
      (and (> (next-to-last n) (last n))
           (increasing-digits? (quotient n 10)))))

;(increasing-digits? 1234567);-> false
;(increasing-digits? 54321);-> True

(define (sum-divisors number)
	(define (helper divisor number accum)
		(cond 
			((>= divisor number) accum)
			((= (remainder number divisor) 0) (helper (+ 1 divisor) number (+ accum divisor)))
			(else (helper (+ 1 divisor) number accum))))
	(helper 1 number 0))

(define (sum-divisors1 n)
  (define (iter i acc)
    (cond ((> i (- n 1)) acc)
          ((= (remainder n i) 0)
           (iter (+ i 1) (+ i acc)))
          (else (iter (+ i 1) acc))))
  (iter 1 0))

(define (sum-divisors number)
	(define (helper divisor number accum)
		(cond 
			((>= divisor number) accum)
			((= (remainder number divisor) 0) (helper (+ 1 divisor) number (+ accum divisor)))
			(else (helper (+ 1 divisor) number accum))))
	(helper 1 number 0))

(my-test-method sum-divisors sum-divisors1 2000)
(sum-divisors 4)
(sum-divisors1 4)

(define (ending-with? a b)
  (if (< b 10)
      (= (remainder a 10) b)
      (and (= (last a) (last b))
           (ending-with? (quotient a 10) (quotient b 10)))))
;Give a base for the recursion next time.
(define (contains? a b)
  (if (< a 10) (= a b)
      (or (ending-with? a b)
          (contains? (quotient a 10) b))))


(define (apply-n n f arg)
  (if (= n 0)
      arg
      (f (apply-n (- n 1) f arg))))

(define (apply-iter-n n f arg)
  (if (= n 0)
      arg
      (apply-iter-n (- n 1) f (f arg))))

(define (apply-twice f arg)
  (f (f arg)))

(define (all? lst pred)
  (if (null? lst ) #t
      (and (pred (car lst))
           (all? (cdr lst) pred))))
(define (any? lst pred)
  (if (null? lst) #f
      (or (pred (car lst))
          (any? (cdr lst) pred))))

(define (derive f dx)
  (lambda (x)
    (/ (- (f (+ x dx)) (f x))
       dx)))

(define (mymem x lst)
  (if (null? lst) #f
      (or
       (equal? x (car lst))
       (mymem x (cdr lst)))))

(define (my-reverse lst)
  (if (null? lst)
      '()
      (append (my-reverse (cdr lst))
              (list (car lst)))))

(define (my-reverse-iter lst)
  (define (iter acc xs)
    (if (null? xs) acc
        (iter (cons (car xs) acc) (cdr xs))))
  (iter '() lst))

(define (take n lst)
  (cond ((= n 0) '())
        ((null? lst) '())
        (else (cons (car lst)
                    (take (- n 1) (cdr lst))))))

(define (compose func-list)
  (lambda (x)
    (foldr (lambda (func acc) (func acc))
           x func-list)))

(define (my-flatten lst)
  (cond ((null? lst) '())
        ((not (list? (car lst)))
         (cons (car lst) (my-flatten (cdr lst))))
        (else (append
               (my-flatten (car lst))
               (my-flatten (cdr lst))))))

(define (insert x sorted-list)
  (cond ((null? sorted-list) (list x))
        ((< x (car sorted-list))
         (cons x sorted-list))
        (else (cons (car sorted-list)
                    (insert x (cdr sorted-list))))))

(define (insertion-sort lst)
  (if (null? lst) '()
      (insert (car lst)
              (insertion-sort (cdr lst)))))

(define (my-map f lst)
  (foldr (lambda (elem acc)
           (cons (f elem) acc)) '() lst))

(define (deep-member x lst)
  (cond ((null? lst) #f)
        ((list? (car lst))
         (or (deep-member x (car lst))
             (deep-member x (cdr lst))))
        (else
         (or (equal? x (car lst))
             (deep-member x (cdr lst))))))

(define (sublists-n size lst)
  (if (or (> size (length lst))
          (= size 0))
      '()
      (cons (take size lst)
            (sublists-n size (cdr lst)))))

(define (sublists lst)
  (define (footwork i)
    (if (> i (length lst))
        '(())
        (cons (sublists-n i lst)
              (footwork (+ i 1)))))
  (footwork 1))
(sublists '(-2 1 3 4 4 62))

(define (gen-interval start end)
  (if (> start end) '()
      (cons start
            (gen-interval (+ start 1) end))))

(define (subl-with-interval lst)
  (foldr (lambda (x acc)
           (cons (sublists-n x lst);Using cons or append here obv s a difference
                   acc))
         '()
         (gen-interval 0 (length lst))))
;(subl-with-interval '(-2 1 3 4 4 62))

(newline)
(define (subsets list)
  (if (null? list)
      '(())
      (append
          (map (lambda (subset) (cons
                                    (car list) subset))
                                    (subsets (cdr list)))
          (subsets (cdr list)))))

(define (union l1 l2)
  (cond ((null? l1) l2)
        ((member (car l1) l2)
         (union (cdr l1) l2))
        (else (cons (car l1)
                    (union (cdr l1) l2)))))

(define (union2 l1 l2)
  (append
   l1
   (filter (lambda (x)
             (not (member x l1))) l2)))

(define (intersection l1 l2)
  (cond ((null? l1) '())
        ((member (car l1) l2)
         (cons (car l1)
               (intersection (cdr l1) l2)))
        (else
         (intersection (cdr l1) l2))))

(define (zip l1 l2)
  (if (or (null? l1) (null? l2))
      '()
      (cons (list (car l1) (car l2))
            (zip (cdr l1) (cdr l2)))))
