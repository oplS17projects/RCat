#lang racket
(require racket/system)
(require racket/tcp)
(require racket/udp)
(define machine-list '())


;; preparing port matching file by splitting the file into a list of string
(define tcp_port_match(file->lines "common_tcp_ports.txt"))
(for-each (lambda (x) (regexp-split #rx"\t" x)) tcp_port_match)
;; break up each string by delimiting tab
(define tport-to-service (map (lambda (x) (regexp-split #rx"\t" x)) tcp_port_match))

; refactor and comment
; start poster writing
; preparing code samples / documentation for readme 

;; example outoutput
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


(displayln "How to use RCat")
(displayln "Usage for multiple machines :")
(displayln "\t> (ips->machines \"192.168.1.1-10\" \"1-443\" \"t\")")
(displayln "\t> (all-tports)")
(displayln "Usage for single machine : ")
(displayln "\t> (define router (machine \"192.168.1.1\" \"1-4096\" \"t\"))")
(displayln "\t> (router '(tports))")
           

; Add more user guidance here

; Checks to see if the user gives a single IP address or a range of IP addresses
; If given a range, each IP is subjected to a ICMP Ping to determine if the machine is alive or not to avoid unnecessary connection attempt with probes
; before adding to the machine-list in the global environment
(define (RCat targets ports protocols)
  (if (regexp-match? #rx".*-.*" targets)
      (ips->machines targets ports protocols)
      (machine targets ports protocols)))

; Takes a range of IP addresses ( denoted by '-' ) and pings each system. We count the machine as dead if we do not receive a response
; within three seconds, otherwise we are creating an individual machine object and adding it to the list of machines in the global environment
(define (ips->machines targets ports protocols)
  (define (probe-ping addr)
    (thread (lambda ()
              (let ((ping-input '()))
                (if (regexp-match? #rx".*64.*" (read-string 4096 (car (process (string-append "ping -c 3 " addr)))))
                    (set! machine-list (cons (machine addr ports protocols) machine-list));add-machine-alive addr)
                    "No connection detected")))))
    (for-each (lambda (target-ip) (probe-ping target-ip) ) (range->list targets)))

;; print out ip and tcp port of every machine in machine-list
(define (all-tports)
  (for-each
   (lambda (machine-dispatch) (begin
                                (printf "IP: ~a\nOpen TCP ports:\n" (machine-dispatch '(ip)))
                                (machine-dispatch '(tports))
                                (printf "\n")))
   machine-list))

;    >>>>>>>>>>NEEDS TESTING<<<<<<<<<<<<
(define (all-uports)
  (for-each (lambda (machine-dispatch) 
              (begin (printf "IP: ~a\nOpen UDP ports:\n" (machine-dispatch '(ip))) (machine-dispatch '(uports)) (printf "\n")) )
            machine-list))

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
  (define udp-socket (udp-open-socket) ) 
  (define (add-udp port)
    (set! open-udp (cons port open-udp)))
  (define (add-tcp port)
    (set! open-tcp (cons port open-tcp)))
  (define (check-uport port)
    (if (memq port open-udp) #t #f))
  (define (check-tport port)
    (if (memq (string->number port) open-tcp) #t #f))
  ; map across our list of open ports. For each port we print it out and then map across the list of strings loaded from the common ports text file
  ; we check every pair to see if it is the correct number then display the matching service if we find it.

  ; redesign this as an accumulation?
  ; accumulate across our list of open ports that will take the port number
  ; and traverse the tport-to-service list, checking to see if we have the service.
  ; if we do, print it.
  (define (match-ports tport-list) (for-each (lambda (openport)
                      (begin (printf "\t~a\t" openport)
                                 (for-each (lambda (x) (cond ((string=? (car x) (number->string openport)) (display (cdr x))))) tport-to-service)) (display "\n"))
                    tport-list))
  (define (probe-udp port)
    (thread (lambda ()
              (if
               (udp-connect! udp-socket ip port) (add-udp port) "No connection detected"))))
  (define (probe-tcp port)
    (thread (lambda () (with-handlers ([exn:fail? (lambda (exn) exn )])
                         (if
                          (let-values (((input output) (tcp-connect ip port))) (list input output))
                          (add-tcp port)
                          "No connection detected")))))
  (define (dispatch message)
    (cond((eq? (car message) 'tports) (match-ports open-tcp))
         ((eq? (car message) 'dtports) open-tcp)
         ((eq? (car message) 'uports) open-udp)
         ((eq? (car message) 'ip) ip)
         ((eq? (car message) 'tport) (check-tport (cadr message)) )
         ((eq? (car message) 'uport) (check-uport (cadr message)) )
         (else error "Bad moves, dude")))
  (begin (if (string=? protocols "t")
             (map (lambda (x) (probe-tcp x)) (enum-ports ports))
             (map (lambda (x) (probe-udp x)) (enum-ports ports)))
         dispatch))

;; break a range up ports in form "1-443"
;; return a list of numbers
(define (enum-ports ports)
  (if (regexp-match? #rx".*-.*" ports)
      (let*((range(regexp-split #rx"-" ports))
        (start (car range))
        (end (cadr range))
        (port-range-numbers(enum-range-i (string->number start) (string->number end)))
        (port-range-strings (map number->string port-range-numbers)))
        port-range-numbers)
      (list (string->number ports))))
