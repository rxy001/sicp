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
  (if (self-evalutaing? exp)
      exp
      (let ((proc (get 'eval (car exp))))
        (if proc
            (proc exp env)
            (if (application? exp)
                (apply (eval (operator exp) env)
                       (list-of-values (operands exp) env))
                (else (error "Unknown expression type -- EVAL" exp)))))))

;4.4
(define (and? exp)
  (tagged-list? exp 'and))

(define (and-clauses exp) (cdr exp))

(define (first-predicate clauses) (car clauses))

(define (rest-predicate clauses) (cdr clauses))

(define (and->if exp)
  (expand-clauses (and-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'true
      (make-if (first-predicate clauses)
               (expand-clauses (rest-predicate clauses))
               'false)))

(define (or? exp)
  (tagged-list? exp 'or))

(define (or->if exp)
  (expand-clauses (or-clauses exp)))

(define (first-predicate clauses) (car clauses))

(define (rest-predicate clauses) (cdr clauses))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (make-if (first-predicate clauses)
               'true
               (expand-clauses (rest-predicate clauses)))))

;4.5
