; env.scm -- provides a dictionary abstraction, as well as a frame ADT
; that represents an environment.
;
; Project UID 539e8badf616b510473c4657a8f7f9e717dc3ca9

; Returns a dictionary object that can be used by passing messages to
; it.
;
; Does not do any error checking.
;
; > (define d (dictionary))
; > (d 'contains 'x)
; #f
; > (d 'insert 'x 3)
; > (not (d 'contains 'x))
; #f
; > (d 'get 'x)
; 3
; > (d 'insert 'x 4)
; > (d 'get 'x)
; 4
; > (d 'length)
; 1
;
; You will need to use some subset of set!, set-car!, set-cdr!. You
; may find the assoc procedure useful.
(define (dictionary)
  (let ((dict '()))
    (lambda (message . args)
      (case message
        (length (length dict))
        (contains (assoc (car args) dict))
        (get (cdr (assoc (car args) dict)))
        (insert (if (assoc (car args) dict)
                    (change-entry (car args) (cadr args) dict)
                    (set! dict (cons (cons (car args) (cadr args)) dict))
                )
        )
        ; (copy dict)
      )
    )
  )
)

; change key value to val given key & dictionary
(define (change-entry key val dict)
  (if (equal? key (caar dict))
      (set-cdr! (car dict) val)
      (change-entry key val (cdr dict))
  )
)


; Returns an object that represents a frame in an environment, with
; the given parent. The parent must either be the empty list or
; another frame object.
;
; Does not do any error checking.
;
; > (define f1 (frame '()))
; > (define f2 (frame f1))
; > (f1 'insert 'x 3)
; > (f1 'get 'x)
; 3
; > (f2 'get 'x)
; 3
; > (f2 'insert 'x 4)
; > (f1 'get 'x)
; 3
; > (f2 'get 'x)
; 4
; > (f2 'insert 'y -7)
; > (f2 'get 'y)
; -7
; > (f1 'contains 'y)
; #f
(define (frame parent)
  (let ((prnt (make-copy parent))
        (dict (dictionary)))
    (lambda (message . args)
      (case message
        (contains (or (dict 'contains (car args))
                      (prnt 'contains (car args))
                   ) ; or
        ) ; contains
        (get (cdr (or (dict 'contains (car args))
                      (prnt 'contains (car args))
                  ) ; or
             ) ; cdr
        ) ; get
        (insert (dict 'insert (car args) (cadr args)))
      )
    )
  )
)

(define (make-copy prnt)
  (if (null? prnt)
      (dictionary)
      prnt
  )
)