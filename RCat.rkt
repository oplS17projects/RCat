#lang racket
(require racket/system)
(require racket/tcp)
(require racket/udp)
(define machine-list '())

(define port_match(file->lines "common_ports.txt"))
(for-each (lambda (x) (regexp-split #rx"\t" x)) port_match)
(define string-port-to-name (map (lambda (x) (regexp-split #rx"\t" x)) port_match))

; refactor and comment
; start poster writing
; preparing code samples / documentation for readme 

;> (ips->machines "192.168.1.1-10" "1-443" "t")
;> (all-tports)
;IP: 192.168.1.1
;Open ports:
;	23	(Telnet)
;	80	(HTTP)
;	53	(Domain Name System (DNS))
;	443	(HTTPS)
;
;IP: 192.168.1.8
;Open ports:


(display "NCat usage\n")
(display "Usage examples")

; Add more user guidance here

; Checks to see if the user gives a single IP address or a range of IP addresses
; If given a range, takes each IP individually and adds to a list of IPs
(define (RCat targets ports protocols)
  (if (regexp-match? #rx".*-.*" targets)
      (ips->machines targets ports protocols)
      (machine targets ports protocols)))

; Takes a range of IP addresses ( noted by '-' ) and pings each system. We wait for a response before
; adding creating an individual machine object and adding it to the list of machine objects in the global environment
(define (ips->machines targets ports protocols)
  (define (probe-ping addr)
    (thread (lambda ()
              (let ((ping-input '()))
                (if (regexp-match? #rx".*64.*" (read-string 4096 (car (process (string-append "ping -c 3 " addr)))))
                    (set! machine-list (cons (machine addr ports protocols) machine-list));add-machine-alive addr)
                    "No connection detected")))))
    (for-each (lambda (target-ip) (probe-ping target-ip) ) (range->list targets)))


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
  (for-each (lambda (machine-dispatch)
              (begin (printf "IP: ~a\nOpen ports:\n" (machine-dispatch '(ip))) (machine-dispatch '(tports)) (printf "\n")) )
            machine-list))

;    >>>>>>>>>>TO BE CODED<<<<<<<<<<<<
(define (all-uports)
  ("stub") )

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
; iterative function to create and return a list of number sequentially within a given range
(define (enum-range-i a b)
  (define (enum-range-halper a b total)
    (if (> a b)
        total
        (enum-range-halper (add1 a) b (append total (list a) ))))
  (enum-range-halper a b '()))

;; Machine object
;  takes an IP address[string], port(s)[string], and protocol[string]
;  returns dispatch procedure
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
  (define (probe-udp ip port) "stub")
  (define (probe-tcp ip port)
    (thread (lambda () (with-handlers ([exn:fail? (lambda (exn) exn )])
                         (if
                          (let-values (((input output) (tcp-connect ip port))) (list input output))
                          (add-tcp port)
                          "No connection detected")))))
  (define (dispatch message)
    (cond((eq? (car message) 'tports) (for-each (lambda (openport)
                                                  (begin
                                                    (printf "\t~a\t" openport)
                                 ;(display openport)
                                 ;(display "\t")
                                 (for-each (lambda (x) (cond
                                                         ((string=? (car x) (number->string openport))
                          (displayln (cdr x))))) string-port-to-name))) open-tcp))
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
;'("64 bytes from 192.168.1.1: icmp_seq=0 ttl=64 time=13.079 ms")(define machine-list '())

