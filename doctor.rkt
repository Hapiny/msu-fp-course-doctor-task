#lang scheme/base
(require racket/trace)

(define (visit-doctor name stop-word num-clients)
    (printf "Hello, ~a!\n" name)
    (printf "What seems to be the trouble?")
    (doctor-driver-loop stop-word num-clients name '()))

(define (ask-patient-name)
    (printf "**    Invitation of the next patient    **\n")
    (printf "Next!\n")
    (printf "Who are you?\n")
    (print '**)
    (car (read)))

(define (doctor-driver-loop stop-word counter name responses)
    (newline)
    (print '**)
    (let ((user-response (read)))
        (cond 
        ((equal? user-response '(goodbye)) 
            (printf "Goodbye, ~a!\n" name)
            (printf "See you next week\n")
            (if (= counter 1) (printf "My work for today is over!\n")
                (let ((new-client (ask-patient-name)))
                    (if (equal? new-client stop-word) '(time to go home)
                        (visit-doctor new-client stop-word (- counter 1))))))
        (else (print (reply user-response responses (list 
                                                        (list (lambda (u h) #t) 1 (lambda (u h) (hedge)))
                                                        (list (lambda (u h) #t) 3 (lambda (u h) (qualifier-answer u)))
                                                        (list (lambda (u h) (not (null? h))) 4 (lambda (u h) (history-answer u h)))
                                                        (list (lambda (u h) (trigger-pred u)) 15 (lambda (u h) (theme-answer u))))))
            (doctor-driver-loop stop-word counter name (remember-answer responses user-response))))))

(define (reply user-response responses strategies) 
    (let ((good-strategies (filter (lambda (x) (not (null? x))) (map (lambda (x)
                                (if ((car x) user-response responses)
                                    (list (cadr x) (caddr x))
                                    '()))
                                strategies))
         )) 
        ((car (pick-random-with-weights good-strategies)) user-response responses)
    )
)

(define (history-answer user-response history)
    (append '(earlier you said that) (change-person (pick-random history))))

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

;(trace many-replace)

(define (hedge)
    (pick-random '((please go on)
                   (many people have the same sorts of feelings)
                   (many of my patients have told me the same thing)
                   (please continue)
                   (tell me more in detail)
                   (it's very interesting, please continue)
                   (my wife often says so))))


(define keys '( 
    (
        (depressed suicide exams university)
        (
            (when you feel depressed, go out for ice cream)
            (depression is a disease that can be treated)
            (stop crying weakling !)
            (always look for occasions for joy)
        )
    )
    (
        (mother father parents brother sister uncle aunt grandma grandpa)
        (
            (tell me more about your * , i want to know all about your *)
            (why do you feel that way about your * ?)
            (what is your relationship with your * ?)
            (how often do you see your family ?)
        )
    )
    (
        (weather rain snow thunder)
        (
            (worse weather is expected)
            (what time of year do you like more ?)
            (i hope you took an umbrella if it rained)
        )
    )
    (
        (sport football tennis basketball swimming)
        (
            (do you have a favorite sport ?)
            (do you follow the sporting events ?)
            (do you do sports ?)
        )
    )
    (
        (university scheme lections)
        (
            (your education is important)
            (how many time do you spend to learning ?)
            (you don`t need to attend lections)
            (Steve Jobs and Bill Gates don`t have higher education)
        )
    )))

(define key-words
    (foldl (lambda (group y) (append (filter (lambda (w) (not (member w y))) (car group)) y))
        '() keys))

(define (trigger-pred user-response) 
    (ormap (lambda (word) (if (member word key-words) #t #f)) user-response))

(define (theme-answer user-response)
    (define (match ur)
        (filter (lambda (x) (member x key-words)) ur))
    (define (helper group word)
        (if (not (member word (car group))) '()
            (cadr group)))
    (let ((key (pick-random (match user-response))))
        (many-replace 
            (pick-random
                (foldl (lambda (x result) (append (helper x key) result)) '() keys))
                (list (list '* key)))))

(define (sum-weights lst)
    (foldl (lambda (x result) (+ result (car x))) 0 lst))

; (define (pick-random-with-weights lst)
;     (let ((rnd (+ 1 (random (sum-weights lst)))))
;         (foldl (lambda (x y) 
;             (let ( (count (+ (car y) (car x)))
;                    (a1 (println (+ (car y) (car x))))
;                    (a2 (println y))
;                    (a3 (printf "Сurrent weight ~a\n" (car x)))
;                    (a4 (printf "Random ~a\n"rnd)) ) 
;                 (cond ((>= count rnd) (printf "БОЛЬШЕ\n") (printf "Count = ~a\n\n" count) (cons count (cdr y)))
;                     (else (printf "МЕНЬШЕ\n") (printf "Count = ~a\n\n" count) (cons count (cdr y))))))
;                (list 0 '()) lst)))

(define (pick-random-with-weights lst)
    (define (helper lst rnd counter result)
        (if (null? lst) result
            (if (>= (+ counter (caar lst)) rnd) 
                (cond ((null? result) (cons (cadar lst) '())))
                (helper (cdr lst) rnd (+ counter (caar lst)) result) 
            )
        )
    )
    (let ((r (+ 1 (random (sum-weights lst))))) 
        (helper lst r 0 '()))
)