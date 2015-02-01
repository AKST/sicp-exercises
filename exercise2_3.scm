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

;;

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))

        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))

        ((product? exp)
         (make-sum
           (make-product (multipler exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multipler exp) var)
                         (multiplicand exp))))

        ((exponent? exp)
         (make-product
           (make-product (exponent exp)
                         (make-exponent
                           (base exp)
                           (make-sum (exponent exp) -1)))
           (deriv (base exp) var)))

        (else
          (error "unknown expressin type: DERIV" exp))))


(define variable? symbol?)

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (is-op? op) (lambda (x) (and (pair? x) (eq? (car x) op)))) 

(define (sum? x) (is-op? '+))
(define (product? x) (is-op? '*))
(define (exponent? x) (is-op? '**))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-exponent n e)
  (cond ((=number? e 1) n)
        ((=number? e 0) 1)
        ((=number? n 1) 1)
        ((=number? n 0) 0)
        ((and (number? m1) (number? m2)) (expt m1 m2))
        (else (list '** n e))))

(define (make-product m1 m2)
  (cond ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((=number? m1 0) 0)
        ((=number? m2 0) 0)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define addend cadr)
(define augend caddr)
(define multipler cadr)
(define multiplicand caddr)
(define base cadr)
(define exponent caddr)

;; sets as an unordered list


(define (ul-element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (ul-element-of-set? x (cdr set)))))

(define (ul-adjoin-set x set)
  (if (ul-element-of-set? x set)
    set
    (cons x set)))

(define (ul-intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((ul-element-of-set? (car set1) set2)
         (cons (car set1)
               (ul-intersection-set (cdr set1) set2)))
        (else (ul-intersection-set (cdr set1) set2))))
        
;; 2.59

(define (ul-union-set set1 set2)
  (if (null? set2) 
    set1
    (ul-union-set (ul-adjoin-set (car set2) set1) (cdr set2))))

;; 2.60

;;
;; since element-of-set? behaviour does not change, do not change
;;
;; the only behaviour that changes is adjoin-set and union-set,
;; but union-set, is implemented in terms of adjoin-set, adjoin-set
;; should be the only funciton that changes
;;

;; set as a ordered list

(define (ol-element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (ol-element-of-set? x (cdr set))))) 

(define (ol-intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1))
          (x2 (car set2)))
      (cond ((= x1 x2) 
             (cons x1 (ol-intersection-set (cdr set1) (cdr set2))))
            ((< x1 x2)
             (ol-intersection-set (cdr set1) set2))
            ((< x2 x1)
             (ol-intersection-set set1 (cdr set2)))))))

;; 2.61

(define (ol-adjoin-set x set)
  (cond ((null? set) (cons x '()))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        ((> x (car set)) (cons (car set)
                               (ol-adjoin-set x (cdr set))))))

;; 2.62

(define (ol-union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((= (car set1) (car set2))
         (cons (car set1) (ol-union-set (cdr set1) (cdr set2))))
        ((< (car set1) (car set2))
         (cons (car set1) (ol-union-set (cdr set1) set2)))
        (else (cons (car set2) (ol-union-set set1 (cdr set2))))))

;; binary tree sets  

(define entry car)

(define left-branch cadr) 

(define right-branch caddr) 

(define (make-tree entry left right) 
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set)) 
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

;; 2.63

(define (tree->list-1 tree)
  (if (null? tree) 
    '()
    (append (tree->list-1 (left-branch tree))
            (cons (entry tree)
                  (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree)
                                        result-list)))))
  (copy-to-list tree '()))

;; a.
;;   yes they produce the same result
;; b.
;;   tree->list-1 is O(log n)
;;   tree->list-2 is O(n log n) 
;;
;; tree->list-1 is more complicated as it iterates over the right
;; side twice once in turning it into a and a second time in appending
;; it to the left side.

;; 2.64
    
(define (list->tree elements)
  ;; note this only works for ordered lists

  (define (partial-tree elts n)
    (if (= n 0)
        (cons '() elts)
        (let* ((left-size (quotient (- n 1) 2))
               (left-result (partial-tree elts left-size))
               (left-tree (car left-result))
               (non-left-elts (cdr left-result))
               (right-size (- n (+ left-size 1)))
               (this-entry (car non-left-elts))
               (right-result (partial-tree (cdr non-left-elts) right-size))
               (right-tree (car right-result))
               (remaining-elts (cdr right-result)))
            (begin
              (map display (list n " l " left-size " " left-tree)) 
              (newline)
              (map display (list n " r " right-size " " right-tree)) 
              (newline)
              (cons (make-tree this-entry left-tree right-tree)
                    remaining-elts)))))

  (car (partial-tree elements (length elements))))

;; a
;;   list->tree
;; b
;;   order of growth is O(n)

;; 2.65

(define (union-set tree-1 tree-2) 
  ;; time   O(2*(n+m))
  ;; growth O(n or m)
  (define (union-list set-1 set-2) 
    (cond ((null? set-1) set-2)
          ((null? set-2) set-1)
          ((= (car set-1) (car set-2))
           (cons (car set-1) 
                 (ol-union-set (cdr set-1) (cdr set-2))))
          ((< (car set-1) (car set-2))
           (cons (car set-1) 
                 (ol-union-set (cdr set-1) set-2)))
          (else (cons (car set-2) (ol-union-set set-1 (cdr set-2))))))
  (list->tree
    (union-list 
      (tree->list-2 tree-1) 
      (tree->list-2 tree-2))))

(define (intersection-set tree-1 tree-2)
  ;; time   O(2*(n+m))
  ;; growth O(n or m)
  (define (intersection-list set-1 set-2)
    (if (or (null? set-1) (null? set-2))
      '()
      (let ((x-1 (car set-1))
            (x-2 (car set-2)))
        (cond ((= x-1 x-2)
               (cons x-1 (intersection-list (cdr set-1)
                                            (cdr set-2))))
              ((< x-1 x-2) (intersection-list (cdr set-1) set-2))
              ((> x-1 x-2) (intersection-list set-1 (cdr set-2)))))))
  (list->tree 
    (intersection-list 
      (tree->list-2 tree-1)
      (tree->list-2 tree-2))))

;; 2.66


(define (lookup given-key set-of-records)

  (define entry-key (caar set-of-records))

  (cond ((null? set-of-records) false)
        ((= given-key entry-key) 
         (cdar set-of-records))
        ((< given-key entry-key)
         (lookup given-key (left-branch set-of-records)))
        ((> given-key entry-key)
         (lookup given-key (right-branch set-of-records)))))






