
;;                _   _               ____    _____ _  _  
;;  ___  ___  ___| |_(_) ___  _ __   |___ \  |___ /| || |  
;; / __|/ _ \/ __| __| |/ _ \| '_ \    __) |   |_ \| || |_ 
;; \__ \  __/ (__| |_| | (_) | | | |  / __/ _ ___) |__   _|
;; |___/\___|\___|\__|_|\___/|_| |_| |_____(_)____(_) |_|  
;;  _            __  __                                                  _ _             
;; | |__  _   _ / _|/ _|_ __ ___   __ _ _ __     ___ _ __   ___ ___   __| (_)_ __   __ _ 
;; | '_ \| | | | |_| |_| '_ ` _ \ / _` | '_ \   / _ \ '_ \ / __/ _ \ / _` | | '_ \ / _` |
;; | | | | |_| |  _|  _| | | | | | (_| | | | | |  __/ | | | (_| (_) | (_| | | | | | (_| |
;; |_| |_|\__,_|_| |_| |_| |_| |_|\__,_|_| |_|  \___|_| |_|\___\___/ \__,_|_|_| |_|\__, |
;;                                                                                 |___/ 
;; Data structure design
;; 

(define-syntax unless 
  (syntax-rules ()
    ((unless condition body ...)
     (if (not condition)
         (begin body ...)
         '()))))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x)
  (cadr x))

(define (weight-leaf x)
  (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

;; tree = leaf | node
;;
;; leaf = (symbol:'leaf 
;;          (symbol 
;;            (number ():'())))
;;
;; node = (tree 
;;          (tree 
;;            (()[symbol] 
;;              (number ():'()))))
;;

;; decoding

(define (decode bits tree)
  (define (decode-impl bits current-branch)
    (unless (null? bits) 
      (let ((next-branch (choosen-branch (car bits) current-branch)))
           (if (leaf? next-branch)
             (cons (symbol-leaf next-branch)
                   (decode-impl (cdr bits) tree))
             (decode-impl (cdr bits) next-branch)))))
  (decode-impl bits tree))

(define (choosen-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

;; building tree

(define (adjoin-set x set)
  " leaf -> tree -> tree "
  (cond ((null? set) (list x))
        ;; x is larger than the head of the set
        ((< (weight x) (weight (car set))) 
         (cons x set))
        ;; x is lesser than the head of the set
        (else
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (unless (null? pairs)
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair) (cadr pair))
                  (make-leaf-set (cdr pairs))))))

;; exercise 2.67

;; adabbca

(define tree-2.67
  (make-code-tree 
    (make-leaf 'A 4)
    (make-code-tree
      (make-leaf 'B 2)
      (make-code-tree 
        (make-leaf 'D 1)
        (make-leaf 'C 1)))))

(define message-2.67
  '(0 1 1 0 0 1 0 1 0 1 1 1 0 ))

;; exercise 2.68

(define (encode message tree)
  (unless (null? message)
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(define (encode-symbol symbol tree)

  (define (encode-symbol-impl-tail tree path)
    ;; ineffient due to the tail append 
    (cond ((leaf? tree) path)
          ((memq symbol (symbols (left-branch tree)))
           (encode-symbol-impl-tail 
             (left-branch tree)
             (append path '(0))))
          ((memq symbol (symbols (right-branch tree)))
           (encode-symbol-impl-tail 
             (right-branch tree)
             (append path '(1))))))
 
  (define (encode-symbol-impl-heavy tree)
    (cond ((leaf? tree) '())
          ; evaluates (left-branch tree) twice
          ((memq symbol (symbols (left-branch tree)))
           (cons 0 (encode-symbol-impl-heavy (left-branch tree))))
          ; redundant
          ((memq symbol (symbols (right-branch tree)))
           (cons 1 (encode-symbol-impl-heavy (right-branch tree))))))

  (define (encode-symbol-impl tree)
    (unless (leaf? tree)
       (let ((left (left-branch tree))) 
         (if (memq symbol (symbols left))
             (cons 0 (encode-symbol-impl left))
             (cons 1 (encode-symbol-impl (right-branch tree)))))))

  (if (and (not (null? tree)) (memq symbol (symbols tree)))  
    (encode-symbol-impl tree)
    (error "missing symbol -- ENCODE-SYMBOL" symbol)))

;; exercise 2.69

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (if (= (length leaf-set) 1)
    (car leaf-set)
    (let ((first  (car  leaf-set))
          (second (cadr leaf-set))
          (rest   (cddr leaf-set)))
      (successive-merge 
        (adjoin-set (make-code-tree first second) rest)))))

;; exercise 2.70

(define alphabet-2.70
  '((A 2)  (BOOM 1) (GET 2) (JOB 2) 
    (NA 16) (SHA 3) (YIP 9) (WAH 1)))

;; alphabet-2.70 consists of 5 bits with
;; the lowerest scoring symbols wah & boom
;; having the following encodings
;;
;; wah  = 11010
;; boom = 11011
;;
;; smallest number of bits is 1 for the symbol na

;; exercise 2.71

;;   /\
;;  1  \
;;    / \
;;   /\ /\
;;  2 3 4 5

;;   /\
;;  1 /\
;;   /\ \
;;  2 /\ \
;;    3 4 \
;;        /\
;;       /  \
;;      /    \
;;     /\    /\
;;    5 /\  8 /\
;;     6 7   9  10

;; exercise 2.72
;;
;; (define (encode-symbol symbol tree)
;; 
;;   (define (encode-symbol-impl-tail tree path)
;;     ;; ineffient due to the tail append 
;;     (cond ((leaf? tree) path)
;;           ((memq symbol (symbols (left-branch tree)))
;;            (encode-symbol-impl-tail 
;;              (left-branch tree)
;;              (append path '(0))))
;;           ((memq symbol (symbols (right-branch tree)))
;;            (encode-symbol-impl-tail 
;;              (right-branch tree)
;;              (append path '(1))))))
;; 
;;   (define (encode-symbol-impl-heavy tree)
;;     (cond ((leaf? tree) '())
;;           ; evaluates (left-branch tree) twice
;;           ((memq symbol (symbols (left-branch tree)))
;;            (cons 0 (encode-symbol-impl-heavy (left-branch tree))))
;;           ; redundant
;;           ((memq symbol (symbols (right-branch tree)))
;;            (cons 1 (encode-symbol-impl-heavy (right-branch tree))))))
;; 
;;   (define (encode-symbol-impl tree)
;;     (unless (leaf? tree)
;;        (let ((left (left-branch tree))) 
;;          (if (memq symbol (symbols left))
;;              (cons 0 (encode-symbol-impl left))
;;              (cons 1 (encode-symbol-impl (right-branch tree)))))))
;; 
;;   (if (and (not (null? tree)) (memq symbol (symbols tree)))  
;;     (encode-symbol-impl tree)
;;     (error "missing symbol -- ENCODE-SYMBOL" symbol)))
;;
;; There are different approaches too implement encode-symbol-impl
;; here I've included at least 2 methods a tail recursive version 
;; and a normally recrusive version.
;; 
;; The complexity of the encode-symbol-impl-tail is slower due to
;; it's appending to the end of the bit path one option is to cons
;; on the front and reverse the order, but that means a second iteration
;; over the list. This is still better than iterating over the processed
;; symbols every iteration of encode-symbol-impl-tail.
;;
;; the none tail recrusive encode-symbol-impl version does not suffer from
;; this problem but it awaits the edge case before releasing stack frames.
;;
;; The complexity of both implementations are bound to the complexity of memq,
;; which is used to see if the symbol is in either the left side or right side,
;; of the huffman tree
;;
;; + N
;;   at the start you iterate over the total collected symbols which has the 
;;   complexity of N.
;;
;; log(N)
;;   the log(N) as you only have to search half the symbols when deciding which 
;;   branch to continue down, and this repeats for each iteration and it's branches.
;; 
;; So worst case is likely N+log(N)


