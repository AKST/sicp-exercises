(define (sq x) 
  (* x x))

(define (abs x)
  (if (< x 0) (- x) x))

(define (print x)
  (display x)
  (newline))

;; 1.3
(define (sq-sum-of-biggers a b c)
  (define (sum-of-sqs x y) 
    (+ (sq x) (sq y)))

  (define (is-smallest? a b c) 
    (and (< a b) (< a c)))

  (cond ((and (is-smallest? a b c)) (sum-of-sqs b c))
        ((and (is-smallest? b a c)) (sum-of-sqs a c))
        (else                       (sum-of-sqs a b))))

;; 1.7
(define (sqrt x)
  ; the soultion to this was to define good-enough?
  ; in a way it is relative to the previous guess,
  ; & this makes all the difference, hence making it
  ; more sensitive to small & large values because
  ; it is relative.

  (define (improve guess)
    (/ (+ guess (/ x guess)) 2))

  (define (good-enough? guess oldguess)
    (< (abs (- guess oldguess)) 
       (* .001 guess)))

  (define (iter guess oldguess)
    (if (good-enough? guess oldguess)
        guess
        (iter (improve guess) guess)))

  (iter 1.0 2.0))
  
;; 1.8 (cubed root)
(define (curt x)
  ; based on newtons method 
  ; (x / y^2 + 2y) / 3

  (define (improve guess)
    (/ (+ (/ x (sq guess)) guess guess) 
       3))

  (define (good-enough? guess oldguess)
    (< (abs (- guess oldguess)) 
       (abs (* .001 guess))))

  (define (iter guess oldguess)
    (if (good-enough? guess oldguess)
        guess
        (iter (improve guess) guess)))

  (iter 1.0 2.0))

;; 1.12
(define (pascal row col)
  ; calucaluting pascals 
  ; triangle via recrusive means
  (cond ((or (< row col) (< row 0)) 0)
        ((or (= col row) (= col 0)) 1)
        ((or (= col 0) (= col (- row 1))) row)
        (else (+ (pascal (- row 1) (- col 1))
                 (pascal (- row 1) col)))))

(define (fast-expr b n)
  (cond ((= n 0) 1)
        ((even? n) (sq (fast-expr b (/ n 2))))
        (else (* b (fast-expr b (- n 1))))))

;; 1.16
(define (fast-it-expr b n)
  (define (iter acc n)
    (cond ((= n 0)    acc)
          ((even? n) (iter (sq acc)  (/ n 2)))
          (else      (iter (* acc b) (- n 1)))))
  (iter b n))


;; 1.17
(define (remainder a b)
  (define (iter a left-over)
    (cond ((< left-over 0) a)
          ((= left-over 0) 0)
          ((> left-over 0) (iter left-over (- left-over b)))))
    (iter a (- a b)))
    
(define (double a) (+ a a))

(define (halve a) (/ a 2))

(define (even? n) (= (remainder n 2) 0))

(define (mul a b)
  (if (= b 0)
    0
    (+ a (mul a (- b 1)))))

(define (fast-mul a b)
  (cond ((= b 1) a)
        ((= b 0) 0)
        ((even? b) (double (fast-mul a (halve b))))
        (else (+ a (fast-mul a (- b 1))))))

;; 1.18
(define (fast-iter-mul a b)
  (define (iter acc n)
    (cond ((= n 1) acc)
          ((= n 0) acc)
          ((even? n) (iter (double acc) (halve n)))
          (else      (iter (+ a acc) (- n 1)))))
  (iter a b))

;; TODO 1.21 -> 1.28


;; 1.29
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;; ???

;; 1.30

(define (sum term a next max)
  (define (iter a acc)
    (if (< a max)
        acc
        (iter (next a) (+ (term a) acc))))
  (iter a 0))

;; 1.31

(define (product term a next max)
  (define (iter a acc)
    (if (< a max)
        acc
        (iter (next a) (* (term a) acc)))))

(define (factorial n)
  (define (term a) a)
  (define (next a) (+ a 1))
  (product term 1 next n))


; 1.32

(define (accumulator combinator base-value term a next max)
  (define (iter a acc)
    (if (< a max)
        0
        (iter (next a)
              (combinator (term a) acc))))
  (iter a base-value))


