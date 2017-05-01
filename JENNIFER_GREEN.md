
# RCat: A Port Scannning Tool Using Racket

## Jennifer Green
### April 30, 2017

# Overview
Our program takes in as user input an IP address or range of addresses, a port number or range of port numbers, and either a "t" or a "u" to denote which protocol the user wishes the scan.  If given a range, it converts that range to a list, then connects to each port in each IP address.  If the port is open, the program then adds it to a list of open ports and matches it against another list of commonly used ports to see if it can determine it's use.



# Libraries Used
We used two libraries, racket/tcp and racket/udp.  

```
(require racket/tcp)
(require racket/udp)
```

# Key Code Excerpts

All code below was written by Jennifer Green except where marked otherwise.  

## 1. Organizing Data as Lists

```
(define udp_port_match(file->lines "common_udp_ports.txt"))
(for-each (lambda (x) (regexp-split #rx"\t" x)) udp_port_match)
...
(define uport-to-service (map (lambda (x) (regexp-split #rx"\t" x)) udp_port_match))
```


 
## 2. Using Recursion to Access Lists

```
  (define (match-udp-ports uport-list) (for-each (lambda (openport)
                      (begin (printf "\t~a\t" openport)
                                 (for-each (lambda (x) (cond ((string=? (car x) (number->string openport)) (display (cdr x))))) uport-to-service)) (display "\n"))
                    uport-list))
```                    

## 3. Using Lambda to Create Functions Utilizing Other Racket Libraries
```
  (define (probe-udp port)
    (thread (lambda ()
              (if
               (udp-connect! udp-socket ip port)
               (add-udp port)
               "No connection detected"))))
```               

## 4. Creating Closures and Using Dispatch
Note:  The majority of this code was written by Josh Everett

```  
(define (dispatch message)
    (cond((eq? (car message) 'tports) (display-tports open-tcp))
         ((eq? (car message) 'uports) (match-udp-ports open-udp))
         ((eq? (car message) 'ip) ip)
         ((eq? (car message) 'tport) (check-tport (cadr message)) )
         ((eq? (car message) 'uport) (check-uport (cadr message)) )
         (else error "Bad moves, dude")))
  (begin (cond ((string=? protocols "t")
             (map (lambda (x) (probe-tcp x)) (enum-ports ports)))
             ((string=? protocols "u")
             (map (lambda (x) (probe-udp x)) (enum-ports ports))))
         dispatch))
```
