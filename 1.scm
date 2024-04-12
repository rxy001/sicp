;1.1
;- 10
;- 12
;- 8
;- 3
;- 6
;- a
;- b
;- 19
;- #f
;- 4
;- 16
;- 6
;- 16


;1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

;1.3
(define (square x) (* x x))
(define (sum-of-squares x y)
    (+ (square x)
       (square y)))

(define (sum a b c) 
  (cond ((and (>= a b) (> b c)) (+ a b)) 
        ((and (>= a c) (> c b)) (+ a c))
        ((and (>= b c) (> c a)) (+ b c))
    ))

;1.4
;a + |b|

;1.5
;正则序：返回 0 ，应用序：栈溢出

;1.6
(if <predicate> <consequent> <alternative>)
;如果 <predicate> 为真值，则求值 <consequent>，否则求值 <alternative>. 这表明 if 是正则序

(define (new-if predicate then-clause else-clause)
    (cond (predicate then-clause)
          (else else-clause)))
;scheme 是应用序求值，因此 new-if 为应用序，那么 sqrt-iter 将会无限循环

;1.7
(define (good-enough? old-guess new-guess) 
    (> (/ (abs (- new-guess old-guess) old-guess)) 0.001))

;1.8
;只用改变求平方根的过程中的 improve

(define (imporve) (/ (+ (/ x (square y)) (* 2 y)) 3))

;1.9
(define (+ a b) 
    (if (= a 0)
        b
        (inc (+ (dec a) b))))
;递归计算过程

(define (+ a b) 
    (if (= a 0)
        b
        (+ (dec a) (inc b))))
;迭代计算过程

;1.11
;递归：
(define (f n) 
    (if (< n 3) 
        n
        (+ (f (- n 1)) 
           (* 2 (f (- n 2))) 
           (* 3 (f (- n 3))))))
    

;迭代：
(define (impl a b c n)
    (if (= n 2)
        c
        (impl b c (+ (* 3 a) (* 2 b) c) (- n 1))))
(define (f n)
    (if (< n 3)
        n
        (impl 0 1 2 n)))

;类似于斐波那契
;1.12
(define (pascal row col)
  (cond ((> col row)
          (error "unvalid col value"))
        ((or (= col 0) (= row col))
          1)
        (else (+ (pascal (- row 1) (- col 1))
                 (pascal (- row 1) col)))))

;1.13 1.14 1.15
;空

;1.16
(define (expt b n)
    (expt-iter b n 1))

(define (even? n)
    (= (remainder n 2) 0))

(define (square a) (* a a))

(define (expt-iter b n a)
    (cond ((= n 0) a)
          ((even? n) (expt (square b) (/ n 2) a))
          (else (expt b (n - 1) (* a b)))))

;1.17 1.18
(define (double a) (* 2 a))
(define (halve a) (/ a 2))
(define (even? n)
    (= (remainder n 2) 0))

(define (multi a b) (multi-iter a b 0))

(define (multi-iter a b p)
    (cond ((= b 0) p)
          ((even? b) (multi-iter (double a) (halve b) p))
          (else (multi-iter a (- b 1) (+ p a)))))

(let ((x 3)
      (y (+ x 2)))
    (* x y)) 

;1.19
;应用 T 2次等同于一次, 计算 T 2次方，然后跟 T 对比可得
;q = 2pq + q2
;p = p2 + q2
;算法没细看.

;1.20
(define (gcd a b) 
    (if (= b 0)
        a
        (gcd b (% a b))))

(gcd 40 (% 206 40)) ;(gac 40 6)
(gcd (% 206 40) (% 40 (% 206 40))) ;(gac 6 4) if 执行 1 次 %
(gcd (% 40 (% 206 40)) (% (% 206 40) (% 40 (% 206 40)))) ;(gac 4 2) if 执行 2 次 %
(gcd (% (% 206 40) (% 40 (% 206 40))) (% (% 40 (% 206 40)) (% (% 206 40) (% 40 (% 206 40))))) ;(gac 2 0)if 执行 4 次 %

;最后一次调用 gcd if 执行 7 %
;返回 a 再执行 4 次 总共 1 + 2 + 4 + 7 + 4 = 18

;应用序 4 次