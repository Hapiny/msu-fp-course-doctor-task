#lang scheme/base
(require racket/trace)

(define (visit-doctor name)
    (printf "Hello, ~a!\n" name)
    (printf "What seems to be the trouble?")
    (doctor-driver-loop name '()))

(define (doctor-driver-loop name responses)
    (newline)
    (print '**)
    (let ((user-response (read)))
        (cond 
        ((equal? user-response '(goodbye)) 
            (printf "Goodbye, ~a!\n" name)
            (printf "See you next week\n"))
        (else (print (reply user-response))
            (doctor-driver-loop name (remember-answer responses user-response))))))

(define (reply user-response)
    (let ((num (random 2)))
        (cond ((= num 0) (qualifier-answer user-response))
              (else (hedge)))))

(define (remember-answer responses ans)
    (if (member ans responses) responses
        (cons ans responses)))
            
(define (qualifier-answer user-response)
    (append (select-beginning-answer) (change-person user-response)))

(define (pick-random lst) (list-ref lst (random (length lst))))

(define (select-beginning-answer) 
    (pick-random '((you seem to think that)
                   (you feel that)
                   (why do you believe that)
                   (why do you say that)
                   (who told you that)
                   (are your parents know that)
                   (how long do you think that))))


(define (change-person phrase)
    (many-replace phrase 
      '((am are) 
        (are am)
        (i you) 
        (you i)
        (me you)
        (mine yours) 
        (yours mine)
        (my your) 
        (your my)
        (myself yourself) 
        (yourself myself))))

; (define (many-replace lst result replacement-pairs)
;         (cond ((null? lst) (reverse result))
;               (else (let ((pat-rep (assoc (car lst) replacement-pairs)))
;                         (many-replace (cdr lst) 
;                                       (cons (if pat-rep (cadr pat-rep) (car lst)) result) 
;                                       replacement-pairs)))))

(define (many-replace lst replacement-pairs)
    (map (lambda (word) (let ((x (assoc word replacement-pairs)))
                            (if x (cadr x) word))) lst))

(trace many-replace)

(define (hedge)
    (pick-random '((please go on)
                   (many people have the same sorts of feelings)
                   (many of my patients have told me the same thing)
                   (please continue)
                   (tell me more in detail)
                   (it's very interesting, please continue)
                   (my wife often says so))))