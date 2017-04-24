#lang racket
(require racket/system)
(require racket/tcp)
(define machine-list '())
; refactor and comment
; build out network
; start poster writing
; preparing code samples / documentation for readme 


(display "NCat usage\n")
(display "Usage examples")

(define (RCat targets ports protocols)
  (if (regexp-match? #rx".*-.*" targets)
      (ips->machines targets ports protocols)
      (machine targets ports protocols)))


(define (ips->machines targets ports protocols)
  (let ((target-machine-ips '()))
  (define (add-machine-alive ip)
    (set! target-machine-ips (cons ip target-machine-ips)))
  (define (check-uports port) "stub")
  (define (probe-ping addr)
    (thread (lambda ()
              (let ((ping-input '()))
                (if (regexp-match? #rx".*64.*" (read-string 4096 (car (process (string-append "ping -c 3 " addr)))))
                    (set! machine-list (cons (machine addr ports protocols) machine-list));add-machine-alive addr)
                    "No connection detected")))))
  (define (dispatch message)
    (set! machine-list (map (lambda (open-ip) (machine open-ip ports protocols)) target-machine-ips))
    (cond((eq? (car message) 'up) target-machine-ips)
         ((eq? (car message) 'machines) machine-list )
         ((eq? (car message) 'tport) (check-tports (cdr message)) )
         ((eq? (car message) 'uport) (check-uports (cdr message)) )
         (else error "Bad moves, dude")))
  (begin
    (map (lambda (target-ip) (probe-ping target-ip) ) (range->list targets))
  dispatch)))

;> (define z (ips->machines "8.8.8.8-9" "53" "t"))
;> (z '(machines))
;'(#<procedure:dispatch>)
;> (define test (z '(machines)))
;> (car test)
;#<procedure:dispatch>
;> ((car test) '(ip))
;"8.8.8.8"
;> ((car test) '(tport 53))
;#t
;> 

(define (all-tports)
  (for-each (lambda (machine-dispatch) (printf "IP:\n~a\nOpen ports:\n~s\n\n" (machine-dispatch '(ip)) (machine-dispatch '(tports))  ))  machine-list))
;dont use me
(define (check-tports port)
  (map (lambda (machine-dispatch) (if(machine-dispatch (list 'tport port)) (machine-dispatch '(ip)) " ")) machine-list))

; convert from range of ips to a list of ips
; (range->list "192.168.1-15") -> '("192.168.1.1" ... "192.168.1.15")
(define (range->list targets)
  (let*((range(regexp-split #rx"-" targets))
        (octets (regexp-split #rx"\\." (car range)))
        (three-octets (string-append (car octets) "." (cadr octets) "." (caddr octets) "."))
        (start (cadddr octets))
        (end (cadr range))
        (individual-machines-int (enum-range-i (string->number start) (string->number end)) )
        (individual-machines-string (map number->string individual-machines-int))
        (subnet (map (lambda (x) (string-append three-octets x)) individual-machines-string) ))
    subnet))

; from ps3c
(define (enum-range-i a b)
  (define (enum-range-halper a b total)
    (if (> a b)
        total
        (enum-range-halper (add1 a) b (append total (list a) ))))
  (enum-range-halper a b '()))


(define (machine ip ports protocols)
  (define open-tcp '())
  (define open-udp '())
  (define (add-upd port)
    (set! open-udp (cons port open-udp)))
  (define (add-tcp port)
    (set! open-tcp (cons port open-tcp)))
  (define (check-uport port)
    (if (memq port open-udp) #t #f))
  (define (check-tport port)
    (if (memq (string->number port) open-tcp) #t #f))
  (define (probe-tcp ip port)
    (thread (lambda () (with-handlers ([exn:fail? (lambda (exn) exn )])
                         (if
                          (let-values (((input output) (tcp-connect ip port))) (list input output))
                          (add-tcp port)
                          "No connection detected")))))
  (define (dispatch message)
    (cond((eq? (car message) 'tports) open-tcp)
         ((eq? (car message) 'uports) open-udp)
         ((eq? (car message) 'ip) ip)
         ((eq? (car message) 'tport) (check-tport (cadr message)) )
         ((eq? (car message) 'uport) (check-uport (cadr message)) )
         (else error "Bad moves, dude")))
  (begin (map (lambda (x) (probe-tcp ip x)) (enum-ports ports)) dispatch))


(define (enum-ports ports)
  (if
   (regexp-match? #rx".*-.*" ports)
  (let*((range(regexp-split #rx"-" ports))
        (start (car range))
        (end (cadr range))
        (port-range-numbers(enum-range-i (string->number start) (string->number end)))
        (port-range-strings (map number->string port-range-numbers)))
    port-range-numbers) (list (string->number ports)) ))



;;; notes

; ping concept + notes on reading port streams
;> (define ping-input '())
;> (define x (car (process (string-append "ping -c 3 " addr))))
;> (if (eof-object? (read-line x)) "hit EOF" (set! ping-input (cons (read-line x) ping-input )))
;> ping-input
;'("64 bytes from 192.168.1.1: icmp_seq=0 ttl=64 time=13.079 ms")