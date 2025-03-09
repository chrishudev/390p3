;;; Test cases for Scheme, Phase 1.

;;; Project UID 539e8badf616b510473c4657a8f7f9e717dc3ca9

(load "env.scm")
(load "assert.scm")

; dictionary tests

(display "Running dictionary tests...") ; 1
(newline)

; student code
;(define d0 (dictionary))
;(d0 'insert 'x 3)
;(d0 'insert 'lst (list 4))
;(assert-true (d0 'contains 'x))     ; 2
;(assert-equal (d0 'get 'x) 3)       ; 3
;(define d1 d0)
;(assert-true (d1 'contains 'x))     ; 3
;(assert-equal (d1 'get 'x) 3)       ; 4
;(d1 'insert 'y 5)
;(assert-true (d0 'contains 'y))     ; 5
;(assert-equal (d0 'get 'y) 5)       ; 6
;(assert-true (d1 'constains 'y))    ; 7
;(assert-equal (d1 'get 'y) 5)       ; 8
;(display "Passed student tests!")
;(newline)
; student code

(define dict1 (dictionary))
(assert-equal (dict1 'length) 0)
(assert-false (dict1 'contains 3))

(dict1 'insert 3 5)
(assert-equal (dict1 'length) 1)
(assert-true (dict1 'contains 3))
(assert-equal (dict1 'get 3) 5)

(dict1 'insert -1 -3)
(assert-equal (dict1 'length) 2)
(assert-true (dict1 'contains 3))
(assert-true (dict1 'contains -1))
(assert-equal (dict1 'get 3) 5)
(assert-equal (dict1 'get -1) -3)

(dict1 'insert 'a #f)
(dict1 'insert 'b '())
(dict1 'insert 'c 0)
(dict1 'insert 'd "")
(assert-true (dict1 'contains 'a))

(define dict2 (dictionary))
(dict2 'insert 'x 3)
(dict2 'insert 'y 5)
(dict2 'insert 'y 8)
(assert-true (dict2 'contains 'x))
(assert-true (dict2 'contains 'y))
(assert-equal (dict2 'length) 2)
(assert-equal (dict2 'get 'x) 3)
(assert-equal (dict2 'get 'y) 8)

(dict2 'insert 'a #f)
(dict2 'insert 'b '())
(dict2 'insert 'c 0)
(dict2 'insert 'd "")
(assert-true (dict2 'contains 'a))
(assert-true (dict2 'contains 'b))
(assert-true (dict2 'contains 'c))
(assert-true (dict2 'contains 'd))
(assert-equal (dict2 'get 'a) #f)
(assert-equal (dict2 'get 'b) '())
(assert-equal (dict2 'get 'c) 0)
(assert-equal (dict2 'get 'd) "")

; frame tests

(display "Running frame tests...")      ; 26
(newline)

; student code
; student code

(define env (frame '()))
(assert-false (env 'contains 'x))       ; 27

(env 'insert 'x 3)
(env 'insert 'lst (list 4))
(assert-true (env 'contains 'x))        ; 28
(assert-equal (env 'get 'x) 3)          ; 29

(define subenv1 (frame env))
(assert-equal (subenv1 'get 'x) 3)      ; 30
(assert-equal (subenv1 'get 'lst) '(4)) ; 31

(subenv1 'insert 'y 5)
(assert-false (env 'contains 'y))       ; 33
(assert-true (subenv1 'contains 'y))    ; 34

(define subenv2 (frame env))
(assert-false (subenv2 'contains 'y))   ; 35

(subenv2 'insert 'y 6)
(assert-equal (subenv1 'get 'y) 5)      ; 36
(assert-equal (subenv2 'get 'y) 6)      ; 37

(define subenv3 (frame subenv2))
(assert-equal (subenv3 'get 'x) 3)      ; 38
(assert-equal (subenv3 'get 'y) 6)      ; 39

(subenv3 'insert 'z 7)
(assert-false (env 'contains 'z))       ; 40
(assert-false (subenv1 'contains 'z))   ; 41
(assert-false (subenv2 'contains 'z))   ; 42
(assert-true (subenv3 'contains 'z))    ; 43

(subenv3 'insert 'y 8)
(assert-equal (subenv1 'get 'y) 5)      ; 44
(assert-equal (subenv2 'get 'y) 6)      ; 45
(assert-equal (subenv3 'get 'y) 8)      ; 46

(subenv3 'insert 'y 9)
(assert-equal (subenv1 'get 'y) 5)      ; 47
(assert-equal (subenv2 'get 'y) 6)      ; 48
(assert-equal (subenv3 'get 'y) 9)      ; 49

(env 'insert 'a #f)
(env 'insert 'b '())
(env 'insert 'c 0)
(env 'insert 'd "")
(assert-true (subenv3 'contains 'a))    ; 50
(assert-true (subenv3 'contains 'b))    ; 51
(assert-true (subenv3 'contains 'c))    ; 52
(assert-true (subenv3 'contains 'd))
(assert-equal (subenv3 'get 'a) #f)
(assert-equal (subenv3 'get 'b) '())
(assert-equal (subenv3 'get 'c) 0)
(assert-equal (subenv3 'get 'd) "")

(display "Done.")
(newline)
