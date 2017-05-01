
# Port Scanning and other Security applications with Racket

## Josh Everett 
### April 29, 2017

# Overview
The main component of our program is an application which can scan a target /24 subnet for active IP
addresses and compile a list of active ports within a desired range. The list of open ports can be
fed to a pattern matching function that maps a port to the name of the service associated with that port.

**Authorship note:** All of the code described here was written by myself.

# Libraries Used
The code uses four libraries:

```
(require racket/tcp)
(require racket/udp)
```

Both the tcp and udp racket libraries provide functionality to create connection attempts with a given port and 
IP address over each protocol.

# Key Code Excerpts

Below are individually labled segments of code from the project that embody key concepts from our OPL class.

## 1. Closures over and individual machine object.

The smallest element of our program is the Machine object which forms a closure over three arguments used 
in the creation of the object. All arguments are passed as strings, but differntiation between the strings
allows for dynamic run time behaviour. 

```racket 
(define (machine ip ports protocols)

...

  (define (dispatch message)
    (cond((eq? (car message) 'tports) (display-tports open-tcp))
         ((eq? (car message) 'uports) open-udp)
         ((eq? (car message) 'ip) ip)
         ((eq? (car message) 'tport) (check-tport (cadr message)) )
         ((eq? (car message) 'uport) (check-uport (cadr message)) )
         (else error "Bad moves, dude")))
  (begin (if (string=? protocols "t")
             (map (lambda (x) (probe-tcp x)) (enum-ports ports))
             (map (lambda (x) (probe-udp x)) (enum-ports ports)))
         dispatch))
 ```
A dozen or so lines have been gutted from the previous definition for the sake of brevity. We just need the understanding
that our machine object constructor evaluates differentely between "t" and "u" protocols-- and that the result of 
evaluating a machine procedure is a dispatch procedure that allows for message passing.
 
## 2. Expression evaluation ; differing between a single and multiple addresses

When the user interacts with the program they pass the target IP of their scan as a string - either a single address 
or a range of addresses within a /24 subnet mask (ie "192.168.1.1-254" ).
A range of IPs will be mapped over using the for-each procedure (as described by SICP, for-each is map
but without the list being returned, it is only evaluated for the side effects of the procedure) to detmerine
if the machine is alive. 
When called normally, ping will execute ad infinitum so we use the -c flag to limit to 3 pings. This way
even if the network lost a ping for some reason it is unlikely we would drop all 3.
We determine if the machine is alive by parsing the response code to our ping attempts looking for the numbers 64,
which signifies a ping response.
When we read a line indiciating a machine is alive we add the machine to a global list of dispatch procedures
that acts as a stack by using set!


```racket
(define (ips->machines targets ports protocols)
  (define (probe-ping addr)
    (thread (lambda ()
              (let ((ping-input '()))
                (if (regexp-match? #rx".*64.*" (read-string 4096 (car (process (string-append "ping -c 3 " addr)))))
                    (set! machine-list (cons (machine addr ports protocols) machine-list));add-machine-alive addr)
                    "No connection detected")))))
    (for-each (lambda (target-ip) (probe-ping target-ip) ) (range->list targets)))

```

## 3. Using Recursion to Organize Results

Because the connection attempts are threaded and our results will be non deterministic
before matching the open ports to their services we recursively sort the port list returned by the
machine's dispatch procedure.

This is done by using a recursive function as the procedure argument for accumulate/foldr.
As accumulate folds back it expects the list it is being handed to be sorted, then it will insert the element into the appropriate place.

```racket
(define (sort-iter element port-list)
    (cond((null? port-list) (cons element port-list))
         ((< (car port-list) element) (cons (car port-list) (sort-iter element (cdr port-list))))
         (else (cons element port-list))))
```

```racket
(define (display-tports list-of-ports)
    (match-tports (foldr (lambda (p q) (sort-iter p q ) ) '() list-of-ports)))
```


## 4. Using recursion to create a Fuzzer

Fuzzers are an essential tool in exploit development - buffer overflows in particular. In order to create
a viable exploit we need to crash a system by overflowing a variable within the program by forcing a buffer
larger than the program can handle. This can be done by creating increasinly large buffers and displaying their size
as we send them over the network. When the system crashes our Fuzzer will stop and we will know the size 
of a buffer that will crash the program

This can be easily accomplished using recursion :
```racket
(define-values (in out) (tcp-connect "192.168.1.13" 110))
(define (fuzz n)
        (if (displayln (read-line in))
            (begin
              (displayln "USER test" out)
              (flush-output out)
              (displayln n)
              (displayln (string-append "PASS " (make-string n #\A)) out)
              (flush-output out)
              (displayln (read-line in))
              (sleep 1)
              (fuzz (+ n 100))) (displayln (string-append "Broke at " n))))
```

This was a fuzzer that was specially made for a vulnerable service that requires we attempt a login to
get access to the variable we want to overflow - hence the USER and PASS arguments that are passed across a network connection.
