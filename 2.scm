;2.1
(define (make-rat n d)
    (if (< d 0)
        (cons (- n) (- d))
        (cons n d)))

;2.2
(define (make-segment start end)
    (cons start end))

(define (start-segment segment)
    (car segment))

(define (end-segment segment)
    (cdr segment))

(define (make-point x y)
    (cons x y))

(define (x-point point)
    (car point))

(define (y-point point)
    (cdr point))

(define (midpoint-segment segment)
    (let  ((start-point (start-segment segment))
          (end-point (end-segment segment)))
        (make-point 
          (/ (+ (x-point start-point) (x-point end-point)) 2)
          (/ (+ (y-point start-point) (y-point end-point)) 2))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(print-point (midpoint-segment (make-segment (make-point 2 4) (make-point 6 6))))


;2.3
;按愿望思维 假设 rect-width 可以获取矩形的宽, rect-height 获取矩形的高
(define (perimeter rect)
  (+ (* 2 (width-of-rect rect)) (* 2 (height-of-rect rect))))

(define (area rect)
  (* (width-of-rect rect) (height-of-rect rect)))

;使用宽和高构建矩形
(define (make-rect width height) 
  (cons width height))

(define (width-of-rect rect)
  (car rect))

(define (height-of-rect rect)
  (cdr rect))
;使用线段构建矩形

;2.4
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p g) p)))

(define (cdr z) 
  (z (lambda (p q) q)))

;2.5
(define (cons a b)
  (*  (expt 2 a) 
      (expt 3 b)))

(define (car z)
  (if (= 0 (remainder z 2))
      (+ 1 (car (/ z 2)))
      0))

(define (cdr z)
  (if (= 0 (remainder z 3))
      (+ 1 (cdr (/ z 3)))
      0))

;2.6
;不理解

;2.7
(define (make-interval a b) (cons a b))
(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))

;2.8
(define (sub-interval x y) 
  (make-interval 
    (- (lower-bound x) (lower-bound y))
    (- (upper-bound x) (lower-bound y))))

;2.17
(define (last-pair x) 
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

;2.18
(define (reverse x)
  (define (impl l p)
    (if (null? l)
        p
        (impl (cdr l) (cons (car l) p))))
  (impl x nil))

;2.19
(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (null? coin-values))


;2.20
(define (getParity x) (if (= (remainder x 2) 0) 0 1))
(define (same-parity . l)
  (define (impl x parity)
    (cond ((null? x) nil)
          ((= (getParity (car x)) parity) (cons (car x) (impl (cdr x) parity)))
          (else (impl (cdr x) parity))))
          
  (if (not (null? l))
      (impl l (getParity (car l)))))

;2.21
(define (square-list items) 
  (if (null? items) 
      nil
      (cons (expt (car items) 2) (square-list (cdr items)))))

(define (square-list items) (map (lambda (x) (expt x 2)) items))

;2.22
;2.23
(define (for-each f l)
  (if (not (null? l))
      (begin
        (f (car l))
        (for-each f (cdr l)))))

(for-each (lambda (x) (newline) (display x)) (list 57 321 88))

;2.24
(1 (2 (3 4)))

;2.25
;11 3 (5 7) 9)
(car (cdr (car (cdr (cdr (list 11 3 (list 5 7) 9))))))
;((7))
(car (car (list (list 7))))
;11 12 13 (4 15 (6 7))))))
(car (cdr (car (cdr (cdr (car (cdr (cdr (cdr (list 11 12 13 (list 4 15 (list 6 7))))))))))))

;2.26
(1 2 3 4 5 6)
((1 2 3) 4 5 6)
((1 2 3) (4 5 6))

;2.27
(define (reverse x)
  (define (impl l p)
    (if (null? l)
        p
        (impl (cdr l) 
              (cons
                    (if (pair? (car l))
                        (impl (car l) nil)
                        (car l))
                    p))))
  (impl x nil))

;2.28
(define (fringe x)
  (cond ((null? x) nil)
        ((not (pair? x)) (list x))
        (else 
          (append (fringe (car x))
                  (fringe (cdr x))))))

;2.30
(define (square x) (* x x))
(define (square-tree-recursive x)
  (cond ((null? x) nil)
        ((not (pair? x)) (square x))
        (else (cons (square-tree-recursive (car x))
                    (square-tree-recursive (cdr x))))))
(define (square-tree-iterative x)
  (map  (lambda (s) 
          (cond ((null? s) nil)
                ((not (pair? s)) (square s))
                (else (square-tree-iterative s))))
        x))

;2.31
(define (square x) (* x x))
(define (tree-map procedure tree)
  (map  (lambda (s) 
          (cond ((null? s) nil)
                ((not (pair? s)) (procedure s))
                (else (tree-map procedure s))))
        tree))
(define (square-tree tree) (tree-map square tree))

;2.32
;2.33
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate 
    (lambda (x y) 
      (cons (p x) y))
    nil
    sequence))

(define (append seq1 seq2) 
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate 
    (lambda (x y)
      (+ 1 y)) 
    0 
    sequence))


;2.35
(define (count-leaves tree)
    (accumulate +
                0
                (map (lambda (sub-tree)
                         (if (pair? sub-tree)          
                             (count-leaves sub-tree)
                             1))
                     tree)))

;2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs)) 
      nil
      (cons (accumulate op init (map car seqs)) 
            (accumulate-n op init (map cdr seqs)))))

;2.38
;3 / 2
;1 / 6
;(1 (2 (3 (()))))
;(((() 1) 2) 3)
;op 需符合结合律, / 和 list 都不符合

;2.39
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (reverse sequence)
  (fold-right (lambda (x y) (cons y x)) nil sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

;2.40

(define (unique-pairs n)
  (accumulate
    append 
    nil 
    (map 
      (lambda (i) 
        (map 
          (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
      (enumerate-interval 1 n))))

(define (prime-sum-pairs n)
    (map make-pair-sum
         (filter prime-sum? (unique-pairs n))))

;2.41
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))
      
(define (filter predicate sequence) 
  (cond 
    ((null? sequence) nil)
    ((predicate (car sequence)) 
     (cons (car sequence) 
           (filter predicate (cdr sequence)))) 
    (else (filter predicate (cdr sequence)))))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
  sequence))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (p n s)
  (filter 
    (lambda 
      (pair)
      (= (+ (car pair) (cadr pair) (caddr pair)) s))
    (accumulate
      append
      nil
      (accumulate 
        append
        nil
        (let 
          ((list1 (enumerate-interval 1 n)))
          (map 
            (lambda (i) 
              (let
                ((list2 (remove i list1)))
                (map 
                  (lambda (j)
                    (map 
                      (lambda (k) (list i j k))
                      (remove j list2)))
                  list2)))
            list1))))))


;2.44
(define (up-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))


;2.45
(define (split op1 op2)
  (define (impl painter n)
    (if (= n 0)
        painter
        (let ((smaller (impl painter (- n 1))))
          (op1 painter (op2 smaller smaller)))))
  impl)

;2.46
(define (make-vect x y)
  (cons x y))

(define (xcor-vect vect)
  (car vect))

(define (ycor-vect vect)
  (cdr vect))

(define (add-vect a b)
  (make-vect 
            (+ (xcor-vect a) (xcor-vect b)
            (+ (ycor-vect b) (ycor-vect b)))))

(define (sub-vect a b)
  (make-vect 
            (- (xcor-vect a) (xcor-vect b)
            (- (ycor-vect b) (ycor-vect b)))))

(define (scale-vect s v)
  (make-vect 
            (* s (xcor-vect v)
            (* s (ycor-vect v)))))

;2.47
(define (origin-frame f) (car f))
(define (edge1-frame f) (cadr f))
(define (edge2-frame f) (caddr f))

(define (origin-frame f) (car f))
(define (edge1-frame f) (cadr f))
(define (edge2-frame f) (cddr f))

;2.48
(define (make-segment s e)
  (cons s e))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

;2.53
;(a b c)
;((george))
;(y1 y2)
;y1
;#f
;#f
;#t
;(red shoes blue socks)

;2.54
(define (equal? a b)
  (cond ((eq? a b) #t)
        ((or (null? a) (null? b)) #f)
        ((not (and (pair? a) (pair? b))) #f)
        ((equal? (car a) (car b)) (equal? (cdr a) (cdr b)))
        (else #f)))


;2.56
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var
                         (multiplicand exp)))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation (base exp) (- (exponent exp) 1)))
                       (deriv (base exp) var)))
        (else
          (error "unknown expression"))))

(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))
(define (base exp)
  (cadr exp))
(define (exponent exp)
  (caddr exp))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list '** base exponent))))

;2.57
(define (make-sum addend . augend)
  (if (null? (cdr augend))
      (cond ((=number? addend 0) (car augend))
            ((=number? (car augend) 0) addend)
            ((and (number? addend) (number? (car augend))) (+ addend (car augend)))
            (else (list '+ addend (car augend))))
      (cons '+ (cons addend augend))))

(define (augend exp)
  (if (null? (cdddr exp))
      (caddr exp)
      (apply make-sum (cdddr exp))))
;(+ 1 2 3) -> (+ 1 (+ 2 3)) 乘法逻辑类似 

;2.59
(define (union-set set1 set2)
  (define (impl set result)
    (cond ((null? set) result)
          ((element-of-set? (car set) result)
           (impl (cdr set) result))
          (else (impl (cdr set) (cons (car set) result)))))
  (impl (append set1 set2) '()))

;2.60
;element-of-set? union-set? 两个操作不需要改变，时间复杂度为 O(n) 和 O(n2)
;adjoin-set 不需要检查直接添加 O(1)
;intersection-set 交集应该是不重复的吧，那么需要遍历 result ，时间复杂度为 O(n2)
;重复表只适合频繁插入. 虽然重复与不重复表的时间复杂度相似，但当到达一定数据量时，差距还是蛮大的

;2.61
(define (adjoin-set x set)
  (if (or (< x (car set)) (= x (car set)))
      (cons x set)
      (cons (car set) (adjoin-set x (cdr set)))))

;2.62
;大致逻辑同 intersection-set

;2.63
;a 产生的结果都一样，中序遍历

;2.66
;与二叉搜索树查找逻辑一致，节点为 ((key value) left-branch right-branch)

;2.67
;ADABBCA

;2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (look-up x set)
  (if (eq? x (car set))
      #t
      (look-up x (cdr set))))

(define (encode-symbol message tree)
  (cond ((leaf? tree) '())
        ((look-up message (left-branch tree)) (cons 0 (encode-symbol message (left-branch tree))))
        ((loop-up message (right-branch tree)) (cons 1 (encode-symbol message (right-branch tree))))
        (else (error "bad message" message))))

;2.69
(define (successive-merge sets)
  (if (> (length sets) 1)
      (successive-merge (adjoin-set (make-code-tree (car sets) (cadr sets)) (cddr sets)))
      (car sets)))


;2.73
;a.以运算符为类型标志，number? 和 same-variable? 不是以运算符做判断条件.
(define (install-sum-package)
  (define (make-sum x y)
        (cond ((=number? x 0)
                y)
              ((=number? y 0)
                x)
              ((and (number? x) (number? y))
                (+ x y))
              (else (list '+ x y))))
  (put 'deriv 'make-sum make-sum)
  (put 'deriv '+ (lambda (operands, var)
                   (make-sum
                     (deriv (car operands) var)
                     (deriv (cadr operands) var)))))

;2.74
;根据分支机构类型进行任务派发，每个分支机构需提供相应的选择函数及类型标志
;a
(define (get-record branch employee) ((get 'get-record branch) employee))
;b
(define (get-salary branch employee) ((get 'get-salary branch) employee))
;c
(define (find-employee-record branchs employee)
  (map (lambda (branch) (get 'find-employee-record branch employee)) branchs))

;2.75
(define (make-from-mag-ang x y)
    (define (dispatch op)
        (cond ((eq? op 'real-part)
                (* x (cos y)))
              ((eq? op 'imag-part)
                (* x (sin y)))
              ((eq? op 'magnitude)
                x)
              ((eq? op 'angle)
                y)
              (else
                (error "Unkonw op  -- MAKE-FROM-MAG-ANG" op))))
    dispatch)

;2.76
;显示分派的通用性操作，不管添加数据对象类型还是新操作，都需要对通用性函数以及具体函数进行修改
;数据导向通过维护二维表，通用性函数通过类型和操作从二维表中取得具体函数进行调用. 适合新类型和新操作
;消息传递是以过程代替对象，实现更为简洁，不需要维护表存储子系统具体函数. 但其返回的是一个过程，
;若新加操作，之前的存储数据对象将无法应用新操作. 更适合新类型
