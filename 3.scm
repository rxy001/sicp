;3.1
(define (accumulator initial)
  (let ((sum (+ 0 initial)))
       (lambda 
              (x) 
              (begin (set! sum (+ sum x)) 
                     sum))))

;3.2
(define (make-monitored fn)
  (let ((count 0))
       (lambda 
          (x)
          (if (eq? x 'how-many-calls?)
              count
              (begin (set! count (+ count 1)) (fn x))))))

;3.3
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient funds")) 
  (define (deposit amount)
    (set! balance (+ balance amount)) balance)
  
  (define (dispatch p m)
    (if (eq? p password)
        (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "nunknowa request -- MAKE-ACCOUNT"
                       m)))
        (error "Incorrect password")))

  dispatch)

;3.4
(define (call-the-cops) (error "call-the-cops"))

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount)) balance)
  
  (let ((count 0))
       (begin 
          (define (dispatch p m)
            (if (>= count 2)
                (call-the-cops)
                (if (eq? p password)
                  (begin (set! count 0)
                         (cond ((eq? m 'withdraw) withdraw)
                              ((eq? m 'deposit) deposit)
                              (else (error "nunknowa request -- MAKE-ACCOUNT"
                                            m))))
                  (begin (set! count (+ count 1))
                         (error "Incorrect password")))))
          dispatch)))



;3.7
(define (make-joint account original-password another-password)
  (lambda (password m)
    (if (eq? password another-password)
        (account original m)
        (error 'make-joint incorrect password'))))

;3.8
(define f
    (lambda (first-value)
        (set! f (lambda (second-value) 0))
        first-value))

;3.14
(define (mystery x) 
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
             (set-cdr! x Y) 
             (loop temp x))))
  (loop x '()))
;x (1 2 3)

;x (1)
;temp (2 3)

;x (2 1)
;temp (3)

;temp '()
;x (3 2 1)
;翻转表. v 为 (a) w (d c b a)，首次调用 loop 修改 v 的 cdr, 但是后来递归中所修改的都为新建的序对 temp 

;3.17
;维护一个表 exists，递归 pairs，每取出一个序对后遍历 exists 判断是否已经存在，若不存在将序对加入 exists. exists 的长度即为结果 
(define (count-pairs pairs)
  (define (impl x list)
    (if (and (pair? x)
             (false? (memq x list)))
        (impl (car x)
              (impl (cdr x) (cons x list)))
        list))
          
  (length (impl pairs '())))

;3.18
(define (loop? x)
  (define (impl x list)
    (if (pair? x)
        (if (memq x list)
            #t
            (impl (cdr x) (cons x list)))
        #f))
  (impl x '()))

(define (make-queue) (cons '() '()))

(define (empty-queue? queue) (and (pair? queue) (null? (car queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
       (cond ((empty-queue? queue)
              (set-car! queue new-pair) (set-cdr! queue new-pair) 
              queue)
             (else 
              (set-cdr! (cdr queue) new-pair) 
              (set-cdr! queue new-pair) 
              queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue) (error "This queue is empty"))
        (else (set-car! queue (cdr (car queue))) queue)))

;3.21
;当删除最后一个数据时未重置 (cdr queue)

;3.22
(define (make-queue)
  (let ((front-ptr '()) (rear-ptr '()))

       (define (empty?) (null? front-ptr))

       (define (insert! item)
        (let ((new-pair (cons item '())))
             (cond ((empty?) (set! front-ptr new-pair) (set! rear-ptr new-pair) front-ptr)
                   (else (set-cdr! rear-ptr new-pair) (set! rear-ptr new-pair) front-ptr))))

       (define (delete!)
        (cond ((empty?) (error "This queue is empty"))
          (else (set! front-ptr (cdr front-ptr)) front-ptr)))         

       (define (dispatch m)
        (cond ((eq? m 'insert!) insert!)
              ((eq? m 'delete!) delete!)
              ((eq? m 'empty?) empty?)))
        dispatch))

(define (insert-queue! queue item) ((queue 'insert!) item))
(define (delete-queue! queue) ((queue 'delete!)))
(define (empty-queue? queue) ((queue 'empty?)))

;3.23
;双向才能满足O(1)，节点结构 (item (prev next))


;一维表格
(define (make-table) (list 'table))

(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? (caar records) key) (car records))
        (else (assoc key (cdr records)))))

(define (look-up key table)
  (let ((record (assoc key (cdr table))))
       (if record 
           (cdr record)
           #f)))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
       (if record
           (set-cdr! record value)
           (set-cdr! table (cons (cons key value) (cdr table))))))


;二维表格
(define (make-table) (list 'table))

(define (look-up key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
       (if subtable
           (let ((record (assoc key-2 subtable)))
                (if record
                    (cdr record)
                    #f))
           #f)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table)))
       (if subtable
           (let ((record (assoc key-2 subtable)))
                (if record
                    (set-cdr! record value)
                    (set-cdr! subtable (cons (cons key-2 value) (cdr subtable)))))
           (set-cdr! table (list key-1 (cons key-2 value) (cdr table)))))))
;3.24
(define (make-table same-key?)
  (let ((table (list 'table)))
       (define (assoc key records)
         (cond ((null? records) #f)
               ((same-key? key (caar records)) (car records))
               (else (assoc key (cdr records)))))

       (define (look-up key-1 key-2)
         (let ((subtable (assoc key-1 (cdr table)))
              (if subtable
                  (let ((record (assoc key-2 (cdr subtable)))
                       (if record 
                           (cdr record)
                           #f)))
                  #f))))

       (define (insert! key-1 key-2 value)
          (let ((subtable (assoc key-1 (cdr table)))
               (if subtable
                  (let ((record (assoc key-2 subtable)))
                        (if record
                            (set-cdr! record value)
                            (set-cdr! subtable (cons (cons key-2 value) (cdr subtable)))))
                  (set-cdr! table (list key-1 (cons key-2 value) (cdr table)))))))

       (define (dispatch m)
        (cond ((eq? m 'loop-up) look-up)
              ((eq? m 'insert) insert!)
              (else (error "Unknown operation -- Table" m))))

       dispatch))

;3.28
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
         (after-delay or-gate-delay (lambda () (set-signal! output new-value)))))
  
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or a b)
  (if (and (= a 0) (= b 0))
      0
      1))


;3.29
(define (or-gate a1 a2 output)
  (let ((a3 (make-wire)) (a4 (make-wire)) (a5 (make-wire)))
       (inverter a1 a3)
       (inverter a2 a4)
       (and-gate a3 a4 a5)
       (inverter a5 output))
  'done)


;3.30
(define (ripple-carry-adder list-A list-B list-S C)
    (define (iter A B S value-of-c)
        (if (and (null? A) (null? B) (null? S))
            'ok
            (let ((Ak (car A))
                  (Bk (car B))
                  (Sk (car S))
                  (remain-A (cdr A))
                  (remain-B (cdr B))
                  (remain-S (cdr S))
                  (Ck (make-wire)))
                (set-signal! Ck value-of-c)
                (full-adder Ak Bk Ck Sk C)
                (iter remain-A remain-B remain-S (get-signal C)))))
    (iter list-A list-B list-S (get-signal C)))

;3.31

;反门完整实现
(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not s)))
         (after-delay inverter-delay (lambda () (set-signal! output new-value))))
         
  (add-action! input invert-input)))

(define (logical-not s)
  (cond ((= s 1) 0)
        ((= s 0) 1)
        (else (error "Invaild signal" s))))

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (get-signal) 
      signal-value)

    (define (set-signal! new-value) 
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))))
    
    (define (accept-action-procedure procedure)
      (set! action-procedures (cons procedure action-procedures))
      (procedure))
    
    (define (dispatch m)
      (cond ((eq? m 'get-signal) get-signal)
            ((eq? m 'set-signal!) set-signal!)
            ((eq? m 'add-action!) accept-action-procedure)
            (else (error "Unknown opeartion" m))))
    dispatch))

(define (call-each procedures)
  (if (not (null? procedures))
      (begin ((car procedures))
             (call-each (cdr procedures)))))


(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda)
                  action
                  the-agenda)))

(define (make-agenda)
  (list '(0))
  
(define (current-time agenda)
  (car agenda)))

(define (sugments agenda)
  (cdr sugments))

(define (empty-agenda? agenda)
  (null? (sugments agenda)))

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (make-segment time queue) 
  (cons time queue))

(define (segment-time segment)
  (car segment))

(define (segment-queue segment)
  (cdr segment))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  
  (define (make-new-time-segment time action)
    (let ((queue (make-queue)))
         (insert-queue! queue action)
         (make-segment time queue)))

  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segment)) action)
        (let ((rest (cdr segments)))
             (if (belongs-before? rest)
                 (set-cdr! segments (cons (make-new-time-segment time action) rest))
                 (add-to-segments! rest)))))
                 
  (let ((segments (sugments agenda)))
       (if (belongs-before? segments)
           (set-segments agenda (cons (make-new-time-segment time action)))
           (add-to-segments! segments))))


;约束系统实现
(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum 
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? sum) (has-value? a1))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? sum) (has-value? a2))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))

  (define (process-forget-value)
    (forget-value! a1)
    (forget-value! a2)
    (forget-value! sum)
    (process-new-value)
    'done)
    
  (define (me req)
    ((cond ((eq? req 'I-have-new-value) process-new-value)
           ((eq? req 'I-lost-my-value) process-forget-value)
           (else (error "Unknown request -- ADDER" req)))))

  (connect a1 me)
  (connect a2 me)
  (connect sum me)

  me)

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((and (has-value? m1) (= (get-value m1) 0)) 
           (set-value! product 0 me))
           
          ((and (has-value? m2) (= (get-value m2) 0))
           (set-value! product 0 me))
          
          ((and (has-value? m1) (has-value? m2))
           (set-value! product (* (get-value m1) (get-value m2)) me))

          ((and (has-value? m1) (has-value? product))
           (set-value! m2 (/ (get-value product) (get-value m1)) me))

          ((and (has-value? m2) (has-value? product))
           (set-value! m1 (/ (get-value product) (get-value m2)) me))))
           
  (define (process-forget-value)
    (forget-value! m1)
    (forget-value! m2)
    (forget-value! product)
    (process-new-value)
    'done)
    
  (define (me req)
    (cond ((eq? req 'I-have-new-value) process-new-value)
          ((eq? req 'I-lost-my-value) process-forget-value)
          (else (error "Unknown request -- MULTIPLIER" req))))
  
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me req)
    (error "Unknown operation --CONSTANT" req))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-new-value))

(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (for-each-expect expection procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) expection) (loop (cdr items))
          (else (procedure (car items))
                (loop (cdr items))))))
  (loop list))

(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value new-value new-informant)
      (cond ((not (has-value? me))
             (begin 
               (set! value new-value)
               (set! informant new-informant)
               (for-each-expect informant inform-about-value constraints)))
            ((not (= value new-value))
             (error "Contradiction" (list value new-val)))
            (else 'ignored)))
    
    (define (forget-my-value recractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-expect retractor inform-about-no-value constraints))
          'ignored))
          
    (define (connect new-constraint)
      (if (not (memq new-constraint constraint)
          (set! constraint (cons new-constraint constraints))))
      (if (has-value? me)
          (inform-about-value new-constraint)))
          
    (define (me req)
      (cond ((eq? req 'has-value?) (if informant true false))
            ((eq? req 'set-value!) set-my-value)
            ((eq? req 'forget-value!) forget-my-value)
            ((eq? req 'connect) connect)
            ((eq? req 'value) value)
            (else (error "Unknown operation -- CONNECTOR" req))))
            
    me))

(define (has-value? connector)
  (connector 'has-value?))

(define (get-value connector)
  (connector 'value))

(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))

(define (forget-value! connector retractor)
  ((connector 'forget-value!) retractor))

(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

;3.33
(define (averager a b c)
  (let ((u make-connector) (i make-connector))
    (adder a b u)
    (multiplier u i c)
    (constant (/ 1 2) i)))

;3.34
;不能逆向计算，无法求解 a

;3.35
(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0 -- SQUARER" (get-value b))
            (set-value! a (sqrt (get-value b) me)))
        (if (has-value? a)
            (set-value! b (square (get-value a) me))
            'ignored)))
            
  (define (process-forget-value)
    (forget-value! a)
    (forget-value! b)
    (process-new-value)
    'done)

  (define (me req)
    (cond ((eq? req 'I-have-new-value) process-new-value)
          ((eq? req 'I-lost-my-value) process-forget-value)
          (else (error "Unknown request -- SQUARER" req))))
          
  (connect a me)
  (connect b me)
  me)

;3.37
(define (cv value)
  (constant value (make-connector)))

(define (c* a1 a2)
  (let ((b (make-connector)))
    (multiplier a1 a2 b)
    b))

(define (c- a1 a2)
  (let ((b (make-connector)))
    (adder b a2 a1)
    b))

(define (c/ a1 a2)
  (let ((b (make-connector)))
    (multiplier b a2 a1)
    b))

;3.38
;a 45 35 50 40 

;3.39
;101 121 11 100

;3.41
;不需要，balance 不涉及当前账户的修改，会正确返回当前的账户余额. 即使 withdraw/deposit 执行时间过长，返回修改
;之前的值

;3.42
;没看出来对结果有什么影响

;3.44
;不对，transfer 不需要考虑账户余额获取与设置之间又发生变化

;3.45 
;不能重复 serialize 过程，不然将导致无法执行过程

(define (my-map proc . list)
  (define (impl-multi proc list)
      (if (null? (car list))
          '()
          (cons (apply proc (impl-single car list))
                (impl-multi proc (impl-single cdr list)))))

  (define (impl-single proc list)
      (if (null? list)
          '()
          (cons (proc (car list))
                (impl-single proc (cdr list)))))

  (if (= (length list) 1) 
      (impl-single proc (car list))
      (impl-multi proc list)))

;stream 在 DrRacket 中可以使用 cons-stream 创建、delay 创建延迟计算对象、force 获取延迟计算对象的值
;3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons 
        (apply proc (map stream-car argstreams))
        (lambda () (apply stream-map (cons proc (map stream-cdr argstreams)))))))

;3.51
(define (stream-car x) (car x))
(define (stream-cdr x) ((cdr x)))

(define (stream-filber pred stream)
  (cond ((stream-null? stream) '())
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filber pred (stream-cdr stream))))
        (else (stream-filber pred (stream-cdr stream)))))

(define (stream-enumerate-interval a b)
  (if (> a b)
      '()
      (cons a (lambda () (stream-enumerate-interval (+ a 1) b)))))

(define (stream-map proc list)
  (if (null? list)
      '()
      (cons (proc (stream-car list))
            (lambda () (stream-map proc (stream-cdr list))))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define x (stream-map show (stream-enumerate-interval 0 10)))

(define (show) (display-line x) x)

(stream-ref x 5)

;3.52
;seq sum = 1
;y sum = 6
;z sum = 10
;sum = 136
;sum = 210
;结果不同，每次计算延迟对象，都会将值添加到 sum

;3.53
;1 2 4 8 16 32 ...
;2的幂

;3.54
(define (mul-streams m1 m2)
  (stream-map * m1 m2))

(define (integers-starting-from n)
  (cons n (lambda () (integers-starting-from (+ n 1)))))

(define integers (integers-starting-from 1))

(define factorials (cons 1 (lambda () (mul-streams (stream-cdr integers) factorials))))

;3,55
(define (partial-sums s)
  (define sums (cons (stream-car s)
                     (lambda ()
                       (stream-map + (stream-cdr s) sums))))
  sums)


;3.56
(define s (cons-stream 1 
                       (merge (merge (scale-stream s 2) 
                                     (scale-stream s 3)) 
                              (scale-stream s 5))))

;3.57
(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons 
        (apply proc (map stream-car argstreams))
        ;recursive
        (lambda () (apply stream-map (cons proc (map stream-cdr argstreams)))))))

(define fibs 
  (cons 0
        (lambda () 
          (cons 1
                ;add
                (lambda ()
                  (add-streams (stream-cdr fibs)
                               fibs))))))
;存在优化时为 n
;通过 fibs 流源代码可以看出，从第 2 个斐波那契数开始， cdr 都是 stream-map 的返回流
;例如在求第 2 个数时 此时 argstreams (1 #proc:add)、(0 1) 返回流 (1 #proc:recursive3) 

;求第 3 个数时调用 #proc:recursive3 可以看出后面的 (map stream-cdr argstreams), argstreams 即为上述的两个流，
;map 求值后 stream-map 返回 (2 #proc:recursive4) 此时 argstreams 为 (1 #proc:recursive3) 、(1 #proc:add)

;求第 4 个数时调用 #proc:recursive4，(map stream-cdr argstreams) 此时 argstreams 为上述的两个流

;我们从上述代换，可以推断出如果不存在优化的话，将出现大量的重复计算
;返回流的 cdr (lambda () (apply stream-map (cons proc (map stream-cdr argstreams)))) 是使所需加法成倍增加的原因


;3.58
(cons-stream 1 (expand 3 7 10))
;(expand 3 7 10)
(cons-stream 4 (expend 2 7 10))
;(expend 2 7 10)
(cons-stream 2 (expend 6 7 10))
;(expend 6 7 10)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
(cons-stream 8 (extend 4 7 10))
;1 4 2 8 有啥规律？？？？？

;3.63
(define (stream-car x)
  (car x))

(define (stream-cdr x)
  (force (cdr x)))


(define (averager a b)
  (/ (+ a b) 2))

(define (sqrt-improve guess x)
  (averager guess (/ x guess)))

(define (sqrt-stream-2 x)
  (define guesses (cons-stream 1 
                              (stream-map (lambda (guess) (sqrt-improve guess x))
                                          guesses)))
  guesses)

(define (sqrt-stream-1 x)
  (display-line 'sqrt)
  (cons-stream 1
               ;stream
               (stream-map (lambda (guess) (sqrt-improve guess x))
                           (sqrt-stream-1 x))))

(define (stream-map proc list)
  (display 'map)
  (if (null? list)
      '()
      (cons-stream (proc (stream-car list))
                   (stream-map proc (stream-cdr list)))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(stream-ref (sqrt-stream-2 10) 3)
(stream-ref (sqrt-stream-1 10) 3)
;sqrt-stream-1
;(sqrt-stream-1 10) 创建一个初始流，记stream1
;stream-ref-3 (stream-cdr stream1)
;调用 stream-map 前先创建初始流记 stream2，stream-map 的 list 为 stream2
;调用 (stream-map proc stream2)
;返回 (cons-stream 1 (stream-map proc (stream-cdr stream2)))

;再次进入 stream-ref-2，s 为 (cons-stream 1 (stream-map proc (stream-cdr stream2)))
;stream-cdr 计算上述流 cdr (stream-map proc (stream-cdr stream2))
;stream2 为新建的，其 cdr 还未计算过，先创建初始流记 stream3, 
;执行 (stream-map (lambda (guess) (sqrt-improve guess x)) stream3) 
;返回一个新流 (cons-stream 1 (stream-map proc (stream-cdr stream3))) ，
;该流即为 (stream-cdr stream2) 的值, 执行 (stream-map proc (cons-stream 1 (stream-map proc (stream-cdr stream3)))), 
;返回 (cons-stream xxx (stream-map proc (cons-stream 1 (stream-map proc (stream-cdr stream3)))))
;该过程存在1次重复计算

;(stream-cdr stream2)

;再次进入 stream-ref-1，s 为 (cons-stream xxx (stream-map proc (cons-stream 1 (stream-map proc (stream-cdr stream3)))))
;需要计算 (stream-map proc (cons-stream 1 (stream-map proc (stream-cdr stream3))))
;细看该过程，(stream-cdr stream3) 的值重复计算了(ref-3、ref-2)
;(stream-map proc (stream-cdr stream3)) 重复计算了(ref-2)

;由此可以看出此过程产生很多新的流，导致大量的重复计算
;流 是真的难调试

;sqrt-stream-2
;(sqrt-stream-2 10) 创建一个初始流，记 guesses
;进入stream-ref-3，求值 (stream-cdr guesses),
;直接调用 stream-map，不会需要再创建初始流了
;list 指向 guesses
;返回 (cons-stream 1 (stream-map proc (stream-cdr guesses))) 记 result1

;进入stream-ref-2, 求值 (stream-map proc (stream-cdr guesses))
;(stream-cdr guesses) 在 stream-ref-3 求值过了即result1，对同一个流求值可以直接引用缓存
;list 指向 result1
;代换为 (stream-map proc result1) 并计算
;返回 (cons-stream XXX (stream-map proc (stream-cdr result1))) 记 result2

;进入stream-ref-1 
;求值 (stream-map proc (stream-cdr result1))
;(stream-cdr result1) 代换下相同于求值 (stream-map proc (stream-cdr guesses))，已经在 ref-2 计算过了
;即为 result2 
;(stream-map proc result2)

;由此可以看出来每次计算都引用上次的结果，不存在重复计算

;3.64
(define (stream-limit stream n)
  (let ((first (stream-ref stream 0)) (second (stream-ref stream 1)))
    (if (< (abs (- first second)) n)
        second
        (stream-limit second n))))


;int-pairs 实现
(define (int-pairs)
  (define (impl i j)
    (if (or (< i j) (= i j))
        (cons-stream (cons i j)
                     (impl (+ 1 i) j))
        (impl 1 (+ j 1))))
  (impl 1 1))

;3.66
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s2) (interleave (stream-cdr s2) s1))))

(define (pairs s t)
  (cons-stream 
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (display-20 n)
  (if (< 0 n)
      (cons (stream-ref (pairs integers integers) (- 20 n)) (display-20 (- n 1)))
      '()))
;((1 1) (2 2) (1 2) (3 3) (1 3) (2 3) (1 4) (4 4) (1 5) (2 4) (1 6) (3 4) (1 7) (2 5) (1 8) (5 5) (1 9) (2 6) (1 10) (3 5))
;(1 100) j 的位置 2n-1 199 前面有198个
;1 2 4 8 16 ，(i j) i = j 符合 2 的 n - 1 幂

;3.67
(define (pairs s t)
  (cons-stream 
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
      (interleave
        (stream-map (lambda (x) (list x (stream-car t))) (stream-cdr s))
        (pairs (stream-cdr s) (stream-cdr t))))))

;3.68
;不行，爆栈. lisp 为应用序求值，陷入了 pairs 无限递归中

;3.69
(define (triples s t u)
  (cons-stream (list (stream-car s) (stream-car t) (stream-car u))
               (interleave 
                 (stream-map (lambda (x)
                                (cons (stream-car s) x)) 
                             (pairs t u))
                 (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define (phythagorean-numbers)
  (define (stream-filter pred s)
    (if (stream-null? s)
        '()
        (if (pred (stream-car s))
            (cons-stream (stream-car s) (stream-filter pred (stream-cdr s)))
            (stream-filter pred (stream-cdr s)))))

  (define (square x) (* x x))
  (stream-filter (lambda (p) (= (+ (square (car p)) (square (cadr p))) (square (caddr p)))) (triples integers integers integers)))

