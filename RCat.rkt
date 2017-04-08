#lang racket

(define (machine ips ports protocols)
  (define open-tcp '())
  (define open-udp '())
  (define (add-upd port)
    (set! open-udp (cons port open-udp)))
  (define (add-tcp port)
    (set! open-tcp (cons port open-tcp)))
  (define (check-uport port)
    "stub")
  (define (check-tport port)
    "stub")
  (define (dispatch message)
    (cond((eq? (car message) 'tports) open-tcp)
         ((eq? (car message) 'uports) open-udp)
         ((eq? (car message) 'tport) (check-tport (cdr message)) )
         ((eq? (car message) 'uport) (check-uport (cdr message)) )
         (else error "Bad moves, dude")))
  dispatch)

; from https://docs.racket-lang.org/reference/eval-model.html#%28part._thread-model%29

; Holy moly Engines look perfect for us!
; http://stackoverflow.com/questions/17252830/racket-run-thread-for-fixed-amount-of-time
; #lang racket
;
;(require racket/engine)
;
;(define e (engine
;           (λ (_)
;             ;; just keep printing every second
;             (let loop ()
;               (displayln "hi")
;               (sleep 1)
;               (loop)))))
;
;;; run only for 2 seconds
;(engine-run 2000 e)
;> (define foo '(1 2))
;> foo
;'(1 2)
;> (set! foo (cons 2 (cdr foo)))
;> shit
;'(2 2)
;> (set! foo (cons 2 foo))
;> foo
;'(2 2 2)