#lang racket
(require racket/system)
; 
; http://stackoverflow.com/questions/30625909/how-would-i-make-this-racket-code-dryer
(define (execute-command proc-name)
  (define proc (find-executable-path proc-name))
  (λ (args)
    (with-output-to-string
     (thunk (apply system* proc args)))))
; ((execute-command "ping") '("-c 3" "8.8.8.8"))
; (regexp-match? #rx"0x" "...some-string...")
(if (regexp-match? #rx".*64.*" ((execute-command "ping") '("-c 3" "8.8.8.8"))) "Yes" "no")
(if (regexp-match? #rx".*64.*" ((execute-command "ping") '("-c 3" "192.168.6.66"))) "Yes" "no")

; attempt a connection with a non existent port
(with-handlers ([exn:fail? (lambda (exn) 'air-bag)])
    (let-values (((input output) (tcp-connect "8.8.8.8" 5555)))
                (list input output)))


;> (define-values (in1 out2) (tcp-connect "8.8.8.8." 53))
;> in1
;#<input-port:8.8.8.8.>
;> out2
;#<output-port:8.8.8.8.>
;> (read in1)
;#<eof>
;> (define-values (in3 out4) (tcp-connect "8.8.8.8." 533))
;; this throws an error
;tcp-connect: connection failed
;  address: 8.8.8.8.
;  port number: 533
;  step: 6
;  system error: Operation timed out; errno=60



;; Information about catching exceptions
;; We need to be prepared to catch the
;; tcp-connect: connection failed
;; error and in that case we can just execute a lamba with an empty body
;; since we will not be adding the attempted PORT to our list of OPEN_T_PORTS

;; The specific error we want to catch
;; https://docs.racket-lang.org/reference/exns.html#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._exn~3afail~3anetwork%29%29

;; General Racket Exceptions guide
;; https://docs.racket-lang.org/guide/exns.html

;> (error "crash!")
;crash!
;> (with-handlers ([exn:fail? (lambda (exn) 'air-bag)])
;    (error "crash!"))
;'air-bag

;; try catching [ exn::<...> (lambda (exn) ) ]
