; core.scm -- implements evaluation as well as special forms.
;
; Uses the error procedure defined in driver.scm and arity-error
; defined in primitives.scm. May also use other procedures defined in
; primitives.scm.
;
; Project UID 539e8badf616b510473c4657a8f7f9e717dc3ca9

(load "env.scm")
(load "error.scm")
(load "primitives.scm")

; Evaluates the given expression in the given environment.
(define (scheme-eval datum env)
  (cond ((or (number? datum)     ; self-evaluating objects
             (boolean? datum)
             (char? datum)
             (string? datum)
             (procedure? datum)
         )
         datum
        )
        ((and (pair? datum) (list? datum))  ; combinations
         ; The result of evaluating the first element must be a host
         ; procedure, representing either a client procedure or
         ; special form. Use procedure? to check this. Send the 'call
         ; message to the host procedure, along with the necessary
         ; arguments, to invoke it.
         (let ((proc (get-proc datum env)))
           (if (procedure? proc)
               (proc 'call '() (map-eval env (cdr datum)))
               (error "not a procedure" proc)
           )
         )
        )
        ; Add a case for symbols (identifiers) here.
        ((symbol? datum)
         (if (env 'contains datum)
             (env 'get datum)
             (error "unknown identifier" datum)
         )
        )
        (else (error "cannot evaluate" datum))
  )
)

(define (get-proc datum env)
  (cond ((null? datum) (error "cannot evaluate null"))
        ((procedure? (car datum)) (car datum))
        ((list? (car datum)) (scheme-eval (car datum) env))
        ((symbol? (car datum)) (if (env 'contains (car datum))
                                   (env 'get (car datum))
                                   (error "unknown procedure symbol" (car datum))
                               )
        )
        (else (error "cannot evaluate procedure" datum))
  )
)


; Implements the begin form, which consists of a sequence of
; expressions.
(define (scheme-begin env . args)
  (begin-helper env args)
)

(define (begin-helper env args)
  (if (= 1 (length args))
      (scheme-eval (car args) env)
      (begin (scheme-eval (car args) env)
             (begin-helper env (cdr args)) 
      )
  )
)


; Implements a conditional, with a test, a then expression, and an
; optional else expression.
(define (scheme-if env . args)
  (if (scheme-eval (car args) env)
      (cadr args)
      (if (> (length args) 2)
          (scheme-eval (caddr args) env)
      )
  )
)


; Implements the quote form.
(define (scheme-quote env . args)
  (car args)
)


; Returns an object representing a user-defined lambda procedure with
; the given name, formal parameters, list of body expressions, and
; definition environment.
;
; The returned object should accept a message and any number of
; arguments.
;
; For the 'call message, it should:
; 1) Check whether the given number of arguments matches the expected
;    number (plus the initial environment argument). Use arity-error
;    to signal an error.
; 2) Evaluate the arguments in the given (dynamic) environment.
; 3) Extend the definition environment with a new frame.
; 4) Bind the formal parameters to the argument values in the new
;    frame.
; 5) Evaluate the body expressions in the new environment.
;
; For the 'to-string message, it should produce a string with the
; following format:
;   [lambda procedure <name>]
; where <name> is the name passed in to primitive-procedure.
(define (lambda-procedure name formals body parent-env)
  (let ((nm name)
        (frmls formals)
        (bd body)
        (prnt parent-env))
    (lambda (message . args)
      (case message
        ('call (let ((arg-list (get-args args)))
                 (if (= (length frmls) (length arg-list))
                     (let ((frm (lambda-args (frame prnt) frmls arg-list)))
                       (body-call '() bd frm)
                     )
                     (arity-error nm (length frmls) (length (cdr args)))
                 )
               )
        )
        ('to-string (string-append "[lambda procedure " (symbol->string nm) "]"))
      ) ; case
    ) ; lambda
  ) ; let
) ; define

(define (body-call prev body env)
  (if (= 1 (length body))
      (scheme-eval (car body) env)
      (body-call (scheme-eval (car body) env) (cdr body) env)
  )
)

(define (lambda-args frm frmls args)
  (if (null? args)
      frm
      (begin (frm 'insert (car frmls) (car args))
             (lambda-args frm (cdr frmls) (cdr args))
      )
  )
)

(define (eval-args frame env formals args)
  (if (null? args)
      frame
      (begin (frame 'insert (car formals) (car args))
             (eval-args frame env (cdr formals) (cdr args))
      )
  )
)


; Implements the lambda form. Returns the newly created procedure.
;
; You are only required to support a fixed (non-variadic) number of
; arguments. You may choose to support other forms or signal an error.
;
; Use lambda-procedure to create the actual representation of the
; procedure.
(define (scheme-lambda env . args)
  (lambda-procedure "lambda" (car args) (cdr args) env)
)


; Implements the define form. Returns the symbol being defined.
;
; You must support both variable and procedure definitions.
; For procedure definitions, you are only required to support a fixed
; (non-variadic) number of arguments. You may choose to support other
; forms or signal an error.
;
; For procedure definitions, use lambda-procedure to create the actual
; representation of the procedure.
(define (scheme-define env . args)
  (let ((key (car args))
        (value (cadr args)))
    (if (list? key)
        (define-insert (car key) (lambda-procedure (car key) (cdr key) value env) env)
        (define-insert key value env)
    )
  )
)

(define (define-insert key value env)
  (env 'insert key value)
  value
)


; Implement the mu form here.


; Returns an object respresenting the given library implementation for
; a special form.
;
; The returned object should accept a message and any number of
; arguments.
;
; For the 'call message, it should:
; 1) Invoke the library implementation on the arguments.
;
; For the 'to-string message, it should produce a string with the
; following format:
;   [syntax <name>]
; where <name> is the name passed in to primitive-procedure.
(define (special-form name native-impl)
  (let ((nm name)
        (ni native-impl))
    (lambda (message . args)
      (case message
        (call (apply ni args))
        (to-string (string-append "[syntax " (symbol->string nm) "]"))
      )
    )
  )
)


; Adds special forms to the given environment and returns the
; environment.
(define (add-special-forms env)
  (env 'insert 'begin (special-form 'begin scheme-begin))
  (env 'insert 'if (special-form 'if scheme-if))
  (env 'insert 'quote (special-form 'quote scheme-quote))
  (env 'insert 'lambda (special-form 'lambda scheme-lambda))
  (env 'insert 'define (special-form 'define scheme-define))
  ; Insert the mu form here.
  env
)
