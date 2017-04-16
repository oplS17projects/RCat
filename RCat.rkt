#lang racket
(require racket/system)
(require racket/tcp)

; https://docs.racket-lang.org/guide/regexp-match.html
; > (regexp-split #rx":" "/bin:/usr/bin:/usr/bin/X11:/usr/local/bin")
; '("/bin" "/usr/bin" "/usr/bin/X11" "/usr/local/bin")
; > (regexp-split #rx" " "pea soup")
; '("pea" "soup")
; > (regexp-split #rx"" "smithereens")
; '("" "s" "m" "i" "t" "h" "e" "r" "e" "e" "n" "s" "")
; > (regexp-split #rx" +" "split pea     soup")
; '("split" "pea" "soup")
; > (regexp-split #rx" *" "split pea     soup")
; '("" "s" "p" "l" "i" "t" "" "p" "e" "a" "" "s" "o" "u" "p" "")

;; break up address beginning to end
;;   192.168.1.1-10 -> '("192.168.1.1" "10")
;; we want to return a new list of strings for every address between these endpoints -> '("192.168.1.1" "192.168.1.2" ... "192.168.1.10")
;; we can pick apart this list to create a new list of machine objects that we feed the individual IPs to

;; https://docs.racket-lang.org/reference/generic-numbers.html#%28part._.Number--.String_.Conversions%29


; > (number->string 3.0)
; "3.0"
; > (number->string 255 8)
; "377"
; > (string->number "3.0+2.5i")
; 3.0+2.5i
; > (string->number "hello")
; #f
; > (string->number "111" 7)
; 57
; > (string->number "#b111" 7)
; 7
; 

(define (RCat targets ports protocols)
  (if (regexp-match? #rx".*-.*" targets)
      (ips->machines targets ports protocols)
      (machine targets ports protocols))
  )

(define (ips->machines targets ports protocols)
  (define target-machines '())
  (define (add-machine-alive ip)
    (set! target-machines (cons ip add-machine-alive)))
  (define (check-tport port) "stub")
  (define (check-uport port) "stub")
  (define (dispatch message)
    (cond((eq? (car message) 'open) target-machines)
         ((eq? (car message) 'machines) "test")
         ((eq? (car message) 'tport) (check-tport (cdr message)) )
         ((eq? (car message) 'uport) (check-uport (cdr message)) )
         (else error "Bad moves, dude")))
  
    dispatch)
  

(define (range->list targets)
  ; convert from range of ips to a list of ips
  ; (range->list "192.168.1-15") -> '("192.168.1.1" ... "192.168.1.15")
  (let*((range(regexp-split #rx"-" targets))
        (three-octets (regexp-split #rx"\\." (car range)))
        (start (cadddr three-octets))
        (end (cadr range))
        (individual-machines-int (enum-range-i (string->number start) (string->number end)) )
        (individual-machines-string (map number->string individual-machines-int))
        (subnet (map (lambda (x) (string-append (car range) x)) individual-machines-string) ))
    subnet
    ))

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
    "stub")
  (define (check-tport port)
    "stub")
  (define (probe-tcp ip port)
    (thread (lambda () (if (with-handlers ([exn:fail? (lambda (exn) exn )])
    (let-values (((input output) (tcp-connect "8.8.8.8" port)))
                (list input output))) (add-tcp port) "NO"))))
  (define (dispatch message)
    (cond((eq? (car message) 'tports) open-tcp)
         ((eq? (car message) 'uports) open-udp)
         ((eq? (car message) 'tport) (check-tport (cdr message)) )
         ((eq? (car message) 'uport) (check-uport (cdr message)) )
         (else error "Bad moves, dude")))
  (begin (map (lambda (x) (probe-tcp ip x)) (enum-ports ports)) dispatch))

(define (enum-ports ports)
  (let*((range(regexp-split #rx"-" ports))
        (start (car range))
        (end (cadr range))
        (port-range-numbers(enum-range-i (string->number start) (string->number end)))
        (port-range-strings (map number->string port-range-numbers)))
    port-range-numbers))

; http://stackoverflow.com/questions/30625909/how-would-i-make-this-racket-code-dryer
(define (execute-command proc-name)
  (define proc (find-executable-path proc-name))
  (λ (args)
    (with-output-to-string
     (thunk (apply system* proc args)))))
; ((execute-command "ping") '("-c 3" "8.8.8.8"))
; (regexp-match? #rx"0x" "...some-string...")
;(if (regexp-match? #rx".*64.*" ((execute-command "ping") '("-c 3" "8.8.8.8"))) "Yes" "no")
;(if (regexp-match? #rx".*64.*" ((execute-command "ping") '("-c 3" "192.168.6.66"))) "Yes" "no")

; attempt a connection with a non existent port
;(with-handlers ([exn:fail? (lambda (exn) 'air-bag)])
;    (let-values (((input output) (tcp-connect "8.8.8.8" 5555)))
;                (list input output)))

;
; (define (worker ip port) (thread
;                 (lambda ()
;                   (if (with-handlers ([exn:fail? (lambda (exn) exn )])
;    (let-values (((input output) (tcp-connect ip port)))
;                (list input output))) (printf "~a~n" port) "FAIL") 
;                   )))
;
;(for ((i 60)) (thread
;                 (lambda () (if (with-handlers ([exn:fail? (lambda (exn) exn )])
;    (let-values (((input output) (tcp-connect "8.8.8.8" i)))
;                (list input output))) (printf "~a~n" i) "NO"))))


;(define (worker num) (thread
;                 (lambda ()
;                   (for ([i num])
;                     (printf "Working hard... ~a~n" i)))))
; 

; OK, engine might not be the best method to do thread creation. After some testing, it like engine expects to evaluate at runtime and then await a signal for execution of body.

; we will use open-tcp and open-udp as stacks. When the threaded connection attempt runs it can add a port with the add-tcp procedure to put it on the top of the stack.

; from https://docs.racket-lang.org/reference/eval-model.html#%28part._thread-model%29

; engine is a potential alternative to thread if we can figure out to call when we need

; http://stackoverflow.com/questions/17252830/racket-run-thread-for-fixed-amount-of-time
;(engine-run 2000 e)
;> (define foo '(1 2))
;> foo
;'(1 2)
;> (set! foo (cons 2 (cdr foo)))
;> foo
;'(2 2)
;> (set! foo (cons 2 foo))
;> foo
;'(2 2 2)
