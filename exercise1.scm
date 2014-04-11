(define (sq x) 
  (* x x))

(define (abs x)
  (if (< x 0) (- x) x))

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
  
;; 1.8
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
