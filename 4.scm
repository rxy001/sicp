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
