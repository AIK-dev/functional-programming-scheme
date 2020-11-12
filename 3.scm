
(define (is-operation? ch)
  (or (char=? ch #\+)
      (char=? ch #\/)
      (char=? ch #\*)
      (char=? ch #\^)
      (char=? ch #\-)))

(define zero 0);So we dont have number starting with zero
(define fin  1);Number finished processing
(define oper 2);
(define starter 3)
(define unfin 4)

(define (expr-valid? str)
  (define (iterator flag idx)
    (let ((leng (- (string-length str) 1)))
      (if (> idx leng)
             (not (= oper flag));Making sure we don't terminate with op symbol

             (let ((current (string-ref str idx)))
               (cond ((char-numeric? current)
                      
                      (cond ((char=? #\0 current)
                             (cond 
                               ((or (= flag oper) (= flag starter))
                                (iterator zero (+ idx 1)))
                               ((= flag unfin)
                                (iterator unfin (+ idx 1)))
                               (else #f)))
                            ((or (= flag unfin) (= flag starter) (= flag oper))
                             (iterator unfin (+ idx 1)))
                            (else #f)))
                  
                     ((char-whitespace? current)
                      (if (or (= flag unfin) (= flag zero))
                          (iterator fin (+ idx 1))
                          (iterator flag (+ idx 1))))
                     ((is-operation? current)
                      (if (or (= flag starter) (= flag oper))
                          #f
                          (iterator oper (+ idx 1))))
                     (else #f))))))
      (iterator starter 0))

;Removes whitespaces from input expression
(define (remove-whitespace str)
  (let ((leng (- (string-length str) 1)))
    (define (iter result idx)
      (cond((> idx leng) result)
           ((char-whitespace? (string-ref str idx))
            (iter result (+ idx 1)))
           (else
            (iter (string-append result (string (string-ref str idx))) (+ idx 1)))))
    (iter "" 0)))

(define (precedence ch)
  (cond ((char=? ch #\+) 2)
         ((char=? ch #\/) 3)
         ((char=? ch #\*) 3)
         ((char=? ch #\^) 4)
         ((char=? ch #\-) 2)
         (else 0)))

(define (string-tail string start)
  (substring string start (string-length string)))


(define (str-reverse s)
  (let ((len (- (string-length s) 1)))
    (define (iter idx res)
      (if (> idx len) res
          (iter (+ idx 1)
                (string-append (string (string-ref s idx)) res))))
    (iter 0 "")))

(define (expr-rp-process expr)

  (define (top stack) (string-ref stack 0))
  (define (pop stack) (string-tail stack 1))
  (define (empty? stack) (string=? stack ""))
  (define (push x stack) (string-append (string x) stack))

  (let ((len (- (string-length expr) 1)))
    (define (iter idx opStack output)
      (cond ((and (> idx len) (empty? opStack))
             (str-reverse output))

            ((> idx len)
             (iter idx
                   (pop opStack)
                   (string-append (string (string-ref opStack 0)) output)))

            ((is-operation? (string-ref expr idx))
             (cond ((empty? opStack)
                    (iter (+ idx 1)
                          (string (string-ref expr idx))
                          (string-append "," output)))

                   ((<= (precedence (string-ref expr idx))
                        (precedence (string-ref opStack 0)))
                    (iter idx
                          (pop opStack)
                          (string-append (string (top opStack)) output)))
            
                   (else (iter
                          (+ idx 1)
                          (push (string-ref expr idx) opStack)
                           (string-append "," output)) )))
            ((char-numeric? (string-ref expr idx))
                    (iter (+ idx 1)
                          opStack
                          (string-append (string (string-ref expr idx)) output)))

            (else "Interference")))
    (iter 0 "" "")) )

(define (expr-rp expr)
  (expr-rp-process (remove-whitespace expr)))


(define (expr-eval-process expr)

  (define expr-len (- (string-length expr) 1))
  
  (define (execute-operation char-op left right)
    (cond ((char=? char-op #\+)
           (+ left right))
          ((char=? char-op #\-)
           (- left right))
          ((char=? char-op #\^)
           (expt left right))
          ((char=? char-op #\*)
           (* left right))
          ((char=? char-op #\/)
           (/ left right))
          (else "Implosion")))

  (define (char-to-int my-char)
    (- (char->integer my-char) (char->integer #\0)))
  
  ;Gives the value of the first number in postfix expresion
  (define (parse-first-number str)
    (define (iter index my-result)
      (if (or (> index expr-len)
              (char=? (string-ref str index) #\,))
          my-result
          (iter (+ index 1) (+ (* my-result 10) (char-to-int (string-ref str index))))))
    (iter 0 0))

  ;Finds the index just after the first "," or returns 0 if there is none
  (define (position str)
    (define (iter index)
      (cond ((> index expr-len) 0)
            ((char=? (string-ref str index) #\,)
             (+ index 1))
            (else (iter (+ index 1)))))
    (iter 0))
  
  
    (define (iter index left right extra st-size)
      (cond ((> index expr-len) left)
            ((and (is-operation? (string-ref expr index)) (= st-size 2))
             (iter (+ index 1) (execute-operation (string-ref expr index) left right) 0 0 1))
            ((and (is-operation? (string-ref expr index)) (= st-size 3))
             (iter (+ index 1) left (execute-operation (string-ref expr index) right extra) 0 2))
            ((and (char-numeric? (string-ref expr index)) (= st-size 2))
             (iter (+ index 1) left (+ (* right 10) (char-to-int (string-ref expr index))) 0 2))
            ((and (char-numeric? (string-ref expr index)) (= st-size 3))
             (iter (+ index 1) left right (+ (* extra 10) (char-to-int (string-ref expr index))) 3))
            (else
             (iter (+ index 1) left right extra (+ st-size 1)))))
  ;If we meet "," start filling right
  ;Position the index after first "," store first number in left, start right with 0
  (iter (position expr) (parse-first-number expr) 0 0 2))

(define (expr-eval expr)
  (expr-eval-process (expr-rp expr)))

(remove-whitespace "12 + 1133 + 12")
(expr-eval "10*20+30")
(expr-rp "10^5+20*30")
(expr-valid? "101+10+13+12")
(expr-eval "13+14+15+7324")
(expr-eval "5")
(expr-rp "7^2^3*5+20*30")
(expr-eval "445+34*3+4*30*40+50*54+343")