;4.1
(define (list-of-values-1 exps env)
  (if (no-operands? exps)
      '()
      (let ((first-value (eval (first-operand exps) env)))
        (cons first-value (list-of-values (rest-operands exps) env)))))

(define (list-of-values-2 exps env)
  (if (no-operands? exps)
      '()
      (let ((second-value (list-of-values (rest-operands exps) env)) 
            (first-value (eval (first-operand exps) env)))
        (cons first-value second-value))))

;4.2
;a. application 没有 tag，那么只要 exp 是序对都会当成过程调用
;b
(define (eval exp env)
  (cond ((call-procedure? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))))

;4.3
(define (eval exp env)
  (if (self-evaluating? exp)
      exp
      (let ((proc (get 'eval (car exp))))
        (if proc
            (proc exp env)
            (if (application? exp)
                (apply (eval (operator exp) env)
                       (list-of-values (operands exp) env))
                (else (error "Unknown expression type -- EVAL" exp)))))))

;4.4
;and
(define (and? exp)
  (tagged-list? exp 'and))

(define (and-clauses exp) (cdr exp))

(define (first-predicate clauses) (car clauses))

(define (rest-predicate clauses) (cdr clauses))

(define (last-predicate? clauses) (null? (cdr clauses)))

(define (and->if exp)
  (and-expand-clauses (and-clauses exp)))

(define (and-expand-clauses clauses)
  (if (null? clauses)
      'true
      (make-if (first-predicate clauses)
               (if (last-predicate? clauses)
                   (first-predicate clauses)
                   (and-expand-clauses (rest-predicate clauses)))
               'false)))

;or
(define (or? exp)
  (tagged-list? exp 'or))

(define (or->if exp)
  (or-expand-clauses (or-clauses exp)))

(define (first-predicate clauses) (car clauses))

(define (rest-predicate clauses) (cdr clauses))

(define (last-predicate? clauses) (null? (cdr clauses)))

(define (or-expand-clauses clauses)
  (if (null? clauses)
      'false
      (make-if (first-predicate clauses)
               (first-predicate clauses)
               (or-expand-clauses (rest-predicate clauses)))))

;4.5
;cond
(define (clause-test clause) (car clause))

(define (clause-recipient clause) (caddr clause))

(define (cond-procedure-clause? clause)
  (cond ((not (= (length clause) 3)) #f)
        ((not (eq? (cadr clause) '=>)) #f)
        (else #t)))

(define (expand-clauses clauses env)
  (if (null? clauses)
      'false
      (let ((first (car clauses)) (rest (cdr clauses)))
        (cond ((cond-else-clause? first) (if (null? rest)
                                             (sequence->exp (cond-actions first))
                                             (error "ELSE clause isn't last -- COND->IF" clauses)))
              ((cond-procedure-clause? first) (let ((pred (eval (list (clause-recipient first) 
                                                                      (eval (clause-test first) 
                                                                            env)) 
                                                               env)))
                                                (make-if pred 
                                                         pred
                                                         (expand-clauses rest env))))
              (else (make-if (cond-predicate first)
                             (sequence->exp (cond-actions first))
                             (expand-clauses rest env)))))))

;assoc 需要改成下列形式. 因为 assoc 返回的是 (b 2)，元循环求值器将会对 b 求值
(list 'assoc (lambda (x y) (list 'quote (assoc x y))))


;4.6
(define (let? exp) (tagged-list exp 'let))

(define (let-clauses exp) (cdr exp))

(define (let->combination clauses)
  (cons (make-lambda (map car (car clauses))
                     (cdr clauses))
        (map cadr (car clauses))))


;4,7
(let* ((x 3)
       (y (+ x 2))
       (z (+ x y 5)))
  (* x z)
  (+ x z))

(let ((x 3))
  (let ((y (+ x 2)))
    (let ((z (+ x y 5)))
      (* x z))))


(define (let*? exp) (tagged-list? exp 'let*))

(define (let*-clauses exp) (cdr exp))

(define (make-let vars body)
  (append (list 'let vars) body))

(define (let*->nested-lets clauses)
  (if (null? (cdar clauses))
      (make-let (car clauses) (cdr clauses))
      (make-let (list (caar clauses))
                (list (let*->nested-lets (cons (cdar clauses) (cdr clauses)))))))


;4.8
(define (make-define-procedure variable parameters body)
  (list 'define (append (list variable) parameters) body))

(define (let->combination-variable clause)
  (car clause))

(define (let->combination-parameters clause)
  (map car (cadr clause)))

(define (let->combination-default-values clause)
  (map cadr (cadr clause)))

(define (let->combination-procedure clause)
  (caddr clause))

(define (let->combination clauses base-env)
  (if (symbol? (let->combination-variable clauses))
      (let ((variable (let->combination-variable clauses)) 
            (parameters (let->combination-parameters clauses))
            (procedure (let->combination-procedure clauses))
            (default-values (let->combination-default-values clauses)))
           (let ((env (extend-environment (list variable)
                                          (list procedure)
                                          base-env)))
                (eval (make-define-procedure variable
                                             parameters
                                             procedure)
                      env)
                (eval (cons variable default-values) env)))
      (eval (cons (make-lambda (map car (car clauses))
                               (cdr clauses))
                  (map cadr (car clauses)))
            base-env)))

;4.11
(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (define (impl variables values)
    (if (null? variables)
        '()
        (cons (cons (car variables) (car values)) 
          (impl (cdr variables) (cdr values)))))
  (impl variables values))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
             (env-loop (enclosing-environment env)))
            ((eq? (caar frame) var)
             (cdar frame))
            (else (scan (cdr frame)))))

    (if (eq? env the-empty-environment))
        (error "Unbound variable" var)
        (scan (first-frame env)))
  
  (env-loop env))

;4.12

(define unbound_var 'UNBOUND_VAR)

(define (scan-env env error-proc proc)
  (if (eq? env the-empty-environment)
      (error-proc)
      (let ((val (scan-frame (first-frame env) proc)))
           (if (eq? val unbound_var)
               (scan-env (enclosing-environment env) error-proc proc)
               val))))

(define (scan-frame frame proc)
  (define (impl vars vals)
    (if (null? vars)
        unbound_var
        (let ((flag (proc vars vals)))
          (if (eq? flag #t)
              (impl (cdr vars) (cdr vals))
              flag))))
  (impl (frame-variables frame)
        (frame-values frame)))

(define (lookup-variable-value var env)
  (scan-env env 
            (lambda () (error "Unbound variable --LOOKUP" var))
            (lambda (vars vals)
              (if (eq? (car vars) var)
                  (car vals)
                  #t))))

(define (set-variable-value! var val env)
  (scan-env env
            (lambda () (error "Unbound variable --SET!" val))
            (lambda (vars vals)
              (if (eq? (car vars) var)
                  (set-car! vals val)
                  #t))))

(define (define-variable! var val env)
  (let ((flag (scan-frame (first-frame env)
                         (lambda (vars vals)
                           (if (eq? (car vars) var)
                               (set-car! vals val)
                               #t)))))
       (if (eq? flag unbound_var)
           (add-binding-to-frame! var val (first-frame env)))))

;4.13
;只删除第一个框架中的约束
(define (make-unbound! var env)
  (let ((prev-vars '())
        (prev-vals '()))
    (scan-frame (first-frame env)
                (lambda (vars vals)
                  (if (eq? (car vars) var)
                      (begin 
                        (set-cdr! prev-vars (cdr vars))
                        (set-cdr! prev-vals (cdr vals)))
                      (begin
                        (set! prev-vars vars)
                        (set! prev-vals vals)
                        #t))))))

;4,14
(define (map proc list)
  (if (null? list)
      '()
      (cons (proc (car list))
            (map proc (cdr list)))))

(eval '(define (map proc list)
  (if (null? list)
      '()
      (cons (proc (car list))
            (map proc (cdr list)))))
         the-global-environment)

;在调用 (eval '(map (lambda (a) (+ a 1)) '(1 2 3)) the-global-environment) 时
;lambda 表达式，将被转化为内部结构 (list 'procedure parameters body env) 作为参数，要处理该结构必须通过 eval
;而系统版本 map 其 body 是不通过 eval 处理的，因此无法处理此 lambda 结构
;当键入定义 map 时，元循环求值器会将其定义为内部结构，其 body 是通过 eval 来处理

;4.16
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? (car vars) var)
             (car vars))
            ((eq? (car vars) '*unassinged*)
             (error "This variable unassinged yet" var))
            (else (scan (cdr vars) (cdr vals)))))
    
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame))))))

;b
(define (scan-out-define proc-body)
  (define variables '())
  (define (transform exp)
    (if (eq? 'define (car exp))
        (let ((variable (definition-variable exp)))
          (set! variables (cons (list variable '*unassigned*) variables))
          (list 'set! variable (definition-value exp)))
        exp))
        
  (define (scan exps)
    (if (null? exps)
      '()
      (cons (transform (car exps)) (scan (cdr exps)))))

  (let ((body (scan proc-body)))
    (list 'let variables body)))

;c
;procedure-body, 使用时在转换, 不浪费性能

;4.17
;扫描转换，会生成 let 表达式，let 同时也是通过 lambda 实现的派生表达式，因此会多创建一个环境框架
;变量的定义与求值，都是在同一个环境中，不会产生不同的行为方式


;不使用 let 定义，还是使用 define, 扫描过程体，将变量的声明提升到最开始，之前的声明转化赋值
;类似于 js var

;4.18
(define (solve f y0 dt)
  (define y (itegral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(
  let ((y '*unassigned*)
       (dy '*unassigned*))
    (let ((a (itegral (delay dy) y0 dt))
          (b (stream-map f y)))
      (set! y a)
      (set! dy b))
)
;在未给 y 赋值之前，就提前计算 (stream-map f y). 即 e2 表达式中依赖 v 变量，求值时将报错

;4.19
;支持 Alyssa, 在定义之前使用变量报错,可以避免产生 bug.
;Eva 定义变量时的值表达式采用延时求值

;4.20
;题目看的一知半解. 以下是个人理解
;练习 4.6 中实现的 let 是 lambda 的派生表达式.
(let ((a 1) (b (+ 2 a)))
  (+ a b))
;那么变量 b 的值表达式求值将报错，无法引用变量 a
;letrc 有点类似于 练习 4.7 的 let* 形式, b 是 3, 而 let* 是通过嵌套 let 实现的.
;letrc 中 b 同样是 3. 变量 a b 具备同样的作用域，且变量约束也是同样建立的
(letrc ((v1 exp1) (v2 exp2))
  <body>)
;转换为
(let ((v1 'unassigned) (v2 'unassigned))
  (set! v1 exp1)
  (set! v2 exp2)
  <body>)
;转换为 lambda
(lambda (v1 v2)
 ((set! v1 exp1)
  (set! v2 exp2)
  <body>)
 ('unassigned 'unassigned))

;4.21
((lambda (n) 
  ((lambda (fact)
     (fact fact n)) 
   (lambda (ft k)
     (if (= k 1) 
         1
         (* k (ft ft (- k 1)))))))
 10)

;4.23
(define (eval exp env)
  ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp)
         (analyze-quoted exp))
        ((variable? exp)
         (analyze-variable exp))
        ((assignment? exp)
         (analyze-assignment exp))
        ((definition? exp)
         (analyze-definition exp))
        ((if? exp)
         (analyze-if exp))
        ((lambda? exp)
         (analyze-lambda exp))
        ((begin? exp)
         (analyze-sequence (begin-actions exp)))
        ((cond? exp)
         (analyze (cond->if exp)))
        ((application? exp)
         (analyze-application exp))
        (else (error "Unknown expression type" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
   (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
  (lambda (env)
    (define-variable! var (vproc env) env)
    'ok)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp)))) 
    (lambda (env)
      (if (true? (pproc env)) 
          (cproc env)
          (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp)))) 
    (lambda (env) (make-procedure vars bproc env))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs) 
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs)) 
              (cdr rest-procs))))
  (let ((procs (map analyze exps))) 
    (if (null? procs)
        (error "Empty sequence -- ANALYZE")) 
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp)))) 
    (lambda (env)
      (execute-application (fproc env)
                           (map (lambda (aproc) (aproc env)) aprocs)))))

(define (execute-application proc args) 
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args)) 
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc) 
                              args
                              (procedure-environment proc))))
                              
        (else (error "Unknown procedure type - EXECUTE-APPLICATION" proc))))

;该优化手段分离了语法分析和执行的过程，对同一表达式只进行一次分析。
;分析的结果是一个可执行的过程，其内部通过闭包引用了表达式各个部分的分析结果。

;例如
(define (loop n)
  (if (n > 0)
      (loop (- n 1))
      'ok))

;采用原来的解释器，每次调用 (loop (- n 1)) 时，都会对 loop 的过程体进行分析.
;在定义 loop 时并未对其过程体进行分析，只是简单讲其转化为内部结构。在调用时，获取 loop 的过程，在进行分析求值
;采用优化过的解释器，在语法分析时，会对 loop 的过程体进行分析，其结果就保存在 loop 的分析结果过程中.
;多次调用 loop 避免了多次的过程体分析

;4.25
(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

(unless (= b 0)
        (/ a b)
        0)

(define (factorial n)
  (unless (= n 1)
          (* n (factorial (- n 1)))
          1))

(factorial 5)
;应用序，无限递归
;正则序，120

;4.27
;input: count
;output: 1
;input: w
;output: 10
;input: count
;output: 2
;在定义 w 时，由于是正则序求值因此会调用一次 (id (id 10)), 实参 (id 10)，不会求值
;那么 w 的值就是 ('thunk (id 10) env) 因此第一次输入 count，值为1
;输入 w 将会对 w 求值 即求值 ('thunk (id 10) env)

;如果惰性求值没有记忆，那么 count 的值等于 w 的调用次数 + 1，否则 w 调用次数大于等于1时，count一直为 2

;4.28
(define (g x) (+ x 1)) 
(define (f g x) (g x))

;4.29
;(square (id 10)) 100
;count 2
;如果存在记忆功能  count 为 1

;4.30
;a) 执行 (eval (proc (car items)) env), 将强迫 proc 求值

;b) 原来的 eval-sequence (p1 1) (1 2), (p2 1) ('thunk 1 env); 
; Cy's eval-sequence (p1 1) (1 2) (p2 1) (1 2)

;c) (actual-value (proc (car items)) env) 代换下 (force-it (eval (proc (car items)) env))
;正常情况下 proc 已经经过 eval 取得最终值了，那么 (force-it p) 还是返回的 p，对于 a 实例没有影响

;d) 原文中的。更符合正则序求值仅在需要时求值的观点

;4.31
(define (new-delay memo)
  (lambda (exp env)
    (list 'thunk exp memo env)))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp obj)
  (cadr obj))

(define (thunk-memo obj)
  (caddr obj))

(define (thunk-env obj)
  (cadddr obj))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk)
  (cadr evaluated-thunk))

(define lazy-parameter (new-delay  #f))

(define lazy-memo-parameter (new-delay  #t))

(define lazy 'lazy)

(define lazy-memo 'lazy-memo)

(define (eval exps env)
  ((application? exp)
         (eval-apply (actual-value (operator exp) env)
                     (operands exp)
                     env)))

(define (procedure-pure-parameters procedure)
  (map (lambda (parameter) 
        (if (pair? parameter)
            (car parameter)
            (parameter)))
       (procedure-parameters procedure)))

(define (eval-apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure 
                                    (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-pure-parameters procedure)
             (list-of-process-args (procedure-parameters procedure) arguments env)
             (procedure-environment procedure))))
        (else
         (error "Unknown procedure type --APPLY" procedure))))

(define (list-of-process-args parameters args env)
  (if (no-operands? args)
      '()
      (cons (cond ((not (pair? (car parameters))) (actual-val (car args) env))
                  ((eq? (caar parameters) lazy) (lazy-parameter (car args) env))
                  ((eq? (caar parameters) lazy-memo) (lazy-memo-parameter (car args) env)))
            (list-of-process-args (cdr parameters)
                                  (cdr args)
                                  env))))

(define (force-it obj)
  (cond ((thunk? obj)
         (if (eq? (thunk-memo obj) #t)
             (let ((result (actual-value (thunk-exp obj) (thunk-env obj))))
                   (set-car! obj 'evaluated-thunk)
                   (set-car! (cdr obj) result)
                   (set-cdr! (cdr obj) '())
                   result)
                   (actual-value (thunk-exp obj) (thunk-env obj))))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))