;; helper functions

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;; 2.45
(define (requal? a b)
  (cond ((and (null? a) (null? b)) #t)
        ((or  (null? a) (null? b)) #f)
        ((and (pair? a) (pair? b)) 
         (and (requal? (car a) (car b))
              (requal? (cdr a) (cdr b))))
        ((or (pair? a) (pair? b)) #f)
        (else (eq? a b))))

