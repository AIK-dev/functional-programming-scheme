
;(require racket/string)

(define (atom? x) (and (not (pair? x)) (not (null? x))))

(define (deep-found item l)
  (cond ((null? l) #f)
        ((atom? l)
         (= (item (car l))))
        (else (or (deep-found item (car l))
                  (deep-found item (cdr l))))))

;Symbolic derivation
(define (derive expr var)
  (cond ((constant? expr) 0)
        ((variable? expr)
         (if (same-variables? expr var) 1 0))
        ((sum? expr)
         (make_sum (derive (ad1 expr) var)
                   (derive (ad2 expr) var)))
        ((product? expr)
         (make_sum
          (make_product (mult1 expr)
                        (derive (mult2 expr) var))
          (make_product (derive (mult1 expr) var)
                        (mult2 expr))))))

(define (constant? expr)
  (number? expr))

(define (variable? expr)
  (symbol? expr))

(symbol? 'ab)

(define (same-variables? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (sum? expr)
  (if (not (atom? expr))
      (eq? (car expr) '+)
      '()))

(define (product? expr)
  (if (not (atom? expr))
      (eq? (car expr) '*)
      '()))

(define (ad1 expr)
  (cadr expr))
(define (ad2 expr)
  (caddr expr))
(define (mult1 expr)
  (cadr expr))
(define (mult2 expr)
  (caddr expr))

(define (make_sum_Naive a1 a2)
  (list '+ a1 a2))
(define (make_product_Naive m1 m2)
  (list '* m1 m2))

(define (make_sum a1 a2)
  (cond ((and (number? a1)
              (number? a2))
         (+ a1 a2))
        ((number? a1)
         (if (= a1 0) a2
         (list '+ a1 a2)))
        ((number? a2)
         (if (= a2 0) a1
             (list '+ a1 a2)))
        (else (list '+ a1 a2))))

(define (make_product m1 m2)
  (cond ((and (number? m1)
              (number? m2))
         (* m1 m2))
        ((number? m1)
         (cond ((= m1 0) 0)
               ((= m1 1) m2)
               (else (list '* m1 m2))))
        ((number? m2)
         (cond ((= m2 0) 0)
               ((= m2 1) m1)
               (else (list '* m1 m2))))
        (else (list '* m1 m2))))

(define (del x l)
  (cond ((null? l) '())
        ((= x (car l)) (cdr l))
        (else (cons (car l) (del x (cdr l))))))
(define (del-all x l)
  (cond ((null? l) '())
        ((= x (car l)) (del-all x (cdr l)))
        (else (cons (car l) (del-all x (cdr l))))))

(define (rem-dub l)
  (if (null? l) '()
      (cons (car l) (rem-dub (del-all (car l) (cdr l))))))

(define (replace el1 el2 l)
  (cond ((null? l) '())
        ((eq? el1 (car l))
         (cons el2 (cdr l)))
        (else (cons (car l) (replace el1 el2 (cdr l))))))

(define (replace-all el1 el2 l)
  (cond ((null? l) '())
        ((eq? el1 (car l))
         (cons el2 (replace-all el1 el2 (cdr l))))
        (else (cons (car l)
                    (replace-all el1 el2 (cdr l))))))

(define (diff? l)
  (if (null? l) #t
        (and (not (memq (car l) (cdr l)))
              (diff? (cdr l)))))
;(diff? '(1 2 3 4 4 5))
;(not (memq 16 '(1 2 3 4 4 5)))

;Seeks a sublist of l that consists of distinct elements
(define (diff-sublist? l)
  (cond ((null? l) #f)
        ((diff? (car l)) #t)
        (else (diff-sublist? (cdr l)))))
(define (leftTree tree)
  (cadr tree))
(define (rightTree tree)
  (caddr tree))
(define (entry tree)
  (car tree))
(define (makeTree ent l r)
  (list ent l r))
(define (leaf? tree)
  (and (null? leftTree) (null? rightTree)))
(define (arexp tree)
  (cond ((and (number? (entry tree))
              (>= (entry tree) 0)
              (<= (entry tree) 9))
         (entry tree))
        ((eq? (entry tree) '+)
         (+ (arexp (leftTree tree))
            (arexp (rightTree tree))))
        ((eq? (entry tree) '-)
         (- (arexp (leftTree tree))
            (arexp (rightTree tree))))
        ((eq? (entry tree) '*)
         (* (arexp (leftTree tree))
            (arexp (rightTree tree))))
        ((eq? (entry tree) '/)
         (/ (arexp (leftTree tree))
            (arexp (rightTree tree))))))


(define (infix->prefix lst)
  (cond
    ((list? lst) 
     (let ((operand1 (car lst))
           (operator (cadr lst))
           (operand2 (caddr lst)))
       (list operator
             (infix->prefix operand1)
             (infix->prefix operand2))))
    (else lst)))
(infix->prefix '(10 + 20 * 30))


;This function doesn't seem very reasonable tbh
(define (tokenize l)
  (let loop ((t '())
             (l l))
    (if (pair? l)
        (let ((c (car l)))
          (if (char=? c #\space)
              (cons (reverse t) (loop '() (cdr l)))
              (loop (cons (car l) t) (cdr l))))
        (if (null? t)
            '()
            (list (reverse t))))))

(define (string-split s)
  (map list->string (tokenize (string->list s))))

(define (convert string)
  (map string->symbol          ; convert each substring into a symbol
       (string-split string))) ; split the string by its spaces

(convert "10+20")

(string->list "10   +20")
(map list->string (tokenize (string->list (string #\a #\space #\b #\space #\c))))

;Doesn't work...  FIGURE OUT WHAT THE PROBLEM IS 
;(map string->symbol (tokenize (string->list "   asd sfd gg hhhh")))

;(map list->string (tokenize (string->list (string #\a #\space #\b #\space #\c))))

;(string-split "this is a string")
;(string->symbol "this" )

;This seems to do something in the right direction
(map string->symbol
     (map list->string (tokenize (string->list (string #\a #\space #\space #\space #\space #\b #\space #\c)))))

(map string->symbol
     (map list->string (tokenize (string->list " sdasdsa    dsd fd fd"))))

;Attempt to filter whitespaces??
(map string->symbol
     (map list->string (tokenize (string->list " sdasdsa    dsd fd fd"))))
(char? #\space)


;A development:: what if the tokenize function introduces these weird fragments into
;the result? You don't even know how it works...

;Now what? Figure out how it works or try to work without it??

(define (string-head string end)
  (substring string 0 end))
(string-length "asdsa d")


;;Hmmmmm Am I allowed to use it?
;(string-replace "foo bar    baz" " " "")


;;Maaaaybe we could still use tokenize but remove whitespaces beforehand???
(define mystr "  10 + 5       *2")
;(map string->symbol
;     (map list->string (tokenize (string->list (string-replace mystr " " "")))))

(string->list mystr)

(define (filterWhitespaces charList)
  (define (white? c) ;A bit overkill
    (or (char=? c #\space)
        (char=? c #\tab)
        (char=? c #\page)
        (char=? c #\linefeed)
        (char=? c #\page)
        (char=? c #\return)))
  (cond ((null? charList) '());There is a built-in procedure for what I just did
        ((char-whitespace? (car charList)) (filterWhitespaces (cdr charList)))
        (else (cons (car charList) (filterWhitespaces (cdr charList))))))

(filterWhitespaces (string->list mystr))
;And now finally you could map char->symbol on this list.

;; the symbol a:  'a
;; the character a: #\a
;; are they equal? no.
;(equal? 'a #\a) ;; produces #f
;; converting a character to a symbol:
(define (char->symbol ch)
  (string->symbol (string ch)))

;(char->symbol #\a) ;;=> produces 'a

;; converting a symbol to a character
(define (symbol->char sym)
  (match (string->list (symbol->string sym))
    ((list ch) ch)
    (other (error 'symbol->char 
                  "expected a one-character symbol, got: ~s" sym)))) 

;(symbol->char 'a) ;; => produces #\a

;Tools and algorithms problem...
;Don't know the tools ... Don't have an algorithm idea...
;Where do we go?? Nobody know...
;Let's assume we have gotten somewhere (point A) and see if we can figure it out onwards to the end
;If we do figure it out there will be more motivation to get to point A