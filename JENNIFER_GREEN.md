
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

In order to match our open ports against a list of common known ports, we first need to read in a file containing the information we need.  We do this, then we use map and regex to organize our information inside the list.  

```
(define udp_port_match(file->lines "common_udp_ports.txt"))
(for-each (lambda (x) (regexp-split #rx"\t" x)) udp_port_match)
...
(define uport-to-service (map (lambda (x) (regexp-split #rx"\t" x)) udp_port_match))
```

## 2. Using Recursion to Access Lists

Once we have our list of common UDP ports, we need to match that information up against our list of currently open UDP ports and display the result.  We use a lambda expression and begin to search through our common port list, checking the car of each node to see if it matches the number of the open port.  If it does, we display the number of the port and the service associated with it; if it doesn't, we just display the port number.  

```
  (define (match-udp-ports uport-list) (for-each (lambda (openport)
                      (begin (printf "\t~a\t" openport)
                                 (for-each (lambda (x) (cond ((string=? (car x) (number->string openport)) (display (cdr x))))) uport-to-service)) (display "\n"))
                    uport-list))
```                    

## 3. Using Lambda to Create Functions Utilizing Other Racket Libraries

This is how we try to get our program to determine if a UDP port is open or closed.  At the moment, it always returns open, which is obviously not what we would like it to do.  We went through several different versions of this function trying to get it to work correctly, but were unable to get it fully functional (possibly due to the limitations of the UDP system).  

```
  (define (probe-udp port)
    (thread (lambda ()
              (if
               (udp-connect! udp-socket ip port)
               (add-udp port)
               "No connection detected"))))
```

This screenshot shows the program NMap, a professional port scanner being run twice back to back with different UDP ports being lsted as open and closed.  False positives are apparently very common with port scanning software.  

[IMG]

## 4. Creating Closures and Using Dispatch
Note:  The majority of this code was written by Josh Everett.  The cond statement and lines dealing with udp were written by Jennifer Green.  

This is out dispatch function.  It takes in a message from the user and performs different operations depending on what the user wants done.  For example, if the user typed in '(uports), the program would run the function match-udp-ports.  

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
