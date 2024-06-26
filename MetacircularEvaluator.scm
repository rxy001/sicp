(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((let? exp) (let->combination (let-clauses exp) env))
        ((let*? exp) (eval (let*->nested-lets (let*-clauses exp)) env))
        ((and? exp) (eval (and->if exp) env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp env) env))
        ((application? exp)
         (eval-apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else 
          (error "Unknown expression type -- EVAL" exp))))

(define (eval-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else 
         (error "Unknown procedure type --APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))


(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        (else #f)))

;if 
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternaitve)
  (list 'if predicate consequent alternaitve))


;variable
(define (variable? exp) (symbol? exp))


;quote
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))


;assignment
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))


;definition 两种格式
;'(define variable value)
;'(define (variable parameters) body)
(define (definition? exp) (tagged-list? exp 'define))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ;formal parameters
                   (cddr exp)))) ;body


;lambda '(lambda (parameters) body)
(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


;begin '(begin)
(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))


;procedure 调用
(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))


;cond
(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp env)
  (expand-clauses (cond-clauses exp) env))

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




;boolean
(define (true? x) (not (eq? x false)))

(define (false? x) (eq? x false))


;procedure
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))


;environment
(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few argments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) 
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
      (if (eq? env the-empty-environment)
          (error "Unbound variable -- SET!" var)
          (let ((frame (first-frame env)))
            (scan (frame-variables frame)
                  (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))


(define primitive-procedures 
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'assoc (lambda (x y) (list 'quote (assoc x y))))
        (list 'cadr cadr)
        (list 'eval eval)
        (list '< <)
        (list '+ +)
        (list '- -)
        (list '= =)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc))) primitive-procedures))


(define (setup-environment)
  (let ((initial-env (extend-environment (primitive-procedure-names)
                                         (primitive-procedure-objects)
                                         the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))


(define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
    (primitive-implementation proc) args))

(define input-prompt "；；； M-Eval input:")

(define output-prompt "；；； M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)))
      (display object)))

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


(define (let? exp) (tagged-list? exp 'let))

(define (let-clauses exp) (cdr exp))

(define (let*? exp) (tagged-list? exp 'let*))

(define (let*-clauses exp) (cdr exp))

(define (make-let vars body)
  (append (list 'let vars) body))

(define (let*->nested-lets clauses)
  (if (null? (cdar clauses))
      (make-let (car clauses) (cdr clauses))
      (make-let (list (caar clauses)) 
                (list (let*->nested-lets (cons (cdar clauses) (cdr clauses)))))))

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

(define (map proc list)
  (if (not? (null? list))
      (begin 
        (proc (car list))
        (map proc (cdr list)))))