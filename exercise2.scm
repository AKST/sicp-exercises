
;; 2.1


(define (single-sign n) 
  (cond ((<= n 0) 1)
        (else -1)))


(define (calc-sign n d)
  ;; calculates the sign of a rational number
  (let ((n-sign (single-sign n))
        (d-sign (single-sign d)))
    (cond ((= n-sign d-sign)  1)
          ((> d-sign n-sign) -1)
          (else 1))))


(define (Make-rat n d)
  ;; - n & d are arounded to their gcd
  ;; - d is always positive
  (let* ((an (abs n))
         (ad (abs d))
         (g (gcd an ad))
         (sign (calc-sign n d)))
    (cons (* sign (/ an g)) (/ ad g))))

;; 2.2

(define (average . xs)
  (/ (apply + xs) (length xs))) 

;;; Point abstraction

(define (point x y) (cons x y))
(define (x-coord p) (car p))
(define (y-coord p) (cdr p))

(define (seg start end) (cons start end))
(define (start s) (car s))
(define (end s) (cdr s))

(define (middle-point p)
  (let 
    ((x (average
          (x-coord (start p))
          (x-coord (end p))))
     (y (average
          (y-coord (start p))
          (y-coord (end p))))) 
    (point x y)))

(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))

(define (my-reverse items)
  (define (iter in out)
    (if (null? in)
        out
        (iter (cdr in) 
              (cons (car in) out))))
  (iter items #nil))
                 
;; 2.19
(define (old-count-change amount)
  ;; old implementation

  (define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
          ((= kinds-of-coins 2) 5)
          ((= kinds-of-coins 3) 10)
          ((= kinds-of-coins 4) 25)
          ((= kinds-of-coins 5) 50)))

  (define (cc amount kinds)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (= kinds 0)) 0)
          (else (+ (cc amount (- kinds 1))
                   (cc (- amount (first-denomination kinds)) kinds)))))

  (cc amount 5))

(define (cc amount coins)
  (define first-denomination car)
  (define expect-first-denomination cdr)
  (define no-more? null?)

  (define (impl amount coins)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (no-more? coins)) 0)
          (else
            (+ (impl amount (expect-first-denomination coins))
               (impl (- amount (first-denomination coins)) coins)))))

  (impl amount coins))

;; 2.21
(define (sq x) (* x x))

(define (sq-list-1 items)
  (if (null? items)
      #nil
      (cons (sq (car items)) 
            (sq-list-1 (cdr items)))))

(define (sq-list-2 items)
  (map sq items))

;; 2.23
(define (each fn elems)
  (if (null? elems) 
    #nil
    (begin 
      (fn (car elems))
      (each fn (cdr elems)))))

;; 2.28

(define (fringe x)
  (cond ((null? x) (list))
        ((not (pair? x)) (list x))
        (else (append (fringe (car x))
                      (fringe (cdr x))))))

;; 2.30
(define (square-tree x)
  (cond ((null? x) (list))
        ((not (pair? x)) (* x x))
        (else (cons (square-tree (car x))
                    (square-tree (cdr x))))))

;; 2.31
(define (tree-map fn x)
  (define (impl x)
    (cond ((null? x) (list))
          ((not (pair? x)) (fn x))
          (else (cons (impl (car x))
                      (impl (cdr x))))))
  (impl x))

;; 2.32
;;
;; This function generates all sub sets of a set
;;
;; (1 2 3) -> ((1) (1 2) (2 3) (1 3) (2) (3) ...)
;;

(define (subsets s)
  (if (null? s) (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) 
                          rest)))))



;; some helper for next questions

(define (accumulate op initial sequence)
  (if (null? sequence) initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


;; 2.33

(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) #nil sequence))

(define (enumerate-tree tree)
  (cond ((null? tree) #nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

;; 2.34
;;
;; This models coefficients
;;
;; ((a(n)x + a(n-1))x + a(n-2))x + a(n-3)
;;


(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coef higher-terms) 
                (+ (* x higher-terms) this-coef))
              0
              coefficient-sequence))

;; 2.35 

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))

;; 2.36
;;
;; applies an op to every elem in a matrix

(define (accumulate-n op init seqs)
  (if (null? (car seqs)) #nil
      (cons (accumulate   op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;; 2.37

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

;; transforms like so
;;
;; ((1 2 3)      ((1 4 7)
;;  (4 5 6)  ->   (2 5 8)
;;  (7 8 9))      (3 6 9))
;;
(define (transpose mat)
  (accumulate-n cons #nil mat))

;; 
;;            ((1 2)
;;             (3 4))
;;              | |
;; ((1 2)  -> ((a b)     a = 1 * 1 * 2 * 3
;;  (3 4)  ->  (c d) 
;;  (5 6)  ->  (e f)
;;  (7 8)) ->  (g h))    g = 7 * 1 * 8 * 3

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))


;; helper
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest) result
        (iter (op result (car rest)) (cdr rest))))
  (iter initial sequence))


;; 2.38
(define (fold-right op initial sequence)
  (if (null? sequence) initial 
      (op (car sequence) 
          (fold-right op initial (cdr sequence)))))

;; helper
(define (flatmap proc seq)
  (accumulate append #nil (map proc seq)))

;; 2.39

(define (r-reverse sequence)
  (fold-right (lambda (x y) (append y '(x))) #nil sequence))

(define (l-reverse sequence)
  (fold-left (lambda (x y) (cons y x)) #nil sequence))

;; helper

(define (enumerate-interval low high)
  (if (> low high) #nil
      (cons low (enumerate-interval (+ low 1) high))))

;; 2.40

(define (unique-pairs n) 
  (flatmap (lambda (i)
      (map (lambda (j) (list i j))
           (enumerate-interval 1 (- i 1))))
      (enumerate-interval 1 n)))

;; 2.41

(define (ordered-triplet n)
    (flatmap (lambda (i)
      (flatmap (lambda (j) 
        (map (lambda (k) (list i j k)) 
             (enumerate-interval 1 (- j 1))))
        (enumerate-interval 1 (- i 1)))) 
      (enumerate-interval 1 n)))

(define (ordered-triplet-sum n s)
  (define (triplet-sum? triplet)
    (= s (accumulate + 0 triplet)))
  (define (make-triplet-sum triple)
    (append triple (list (accumulate + 0 triple))))
  (map make-triplet-sum
    (filter triplet-sum? (ordered-triplet n))))


