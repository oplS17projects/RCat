# RCat

### Statement
- RCat is a port scanner scanner that can do basic port to service matching. 
- Capable of scanning a single or multiple machines over a network for open TCP ports. 
- Multiple machines over a /24 network may be scanned, but are subject to a ICMP ping before they are considered valid.
- UDP scanning is functional and unreliable as we discuss.

### Analysis of functional approaches and OPL concepts


Our project allows the option of several layers abstraction , 
  * Our 'subnet' objects are a closure over a list of machines we have pinged 
      and determined to be alive and several procedures.
  * Our machine object is a closure over an IP address, lists of open ports, and several procedures
  
      
Because both elements of our program form closures over lists we use map and variations of map constantly.
  
  * Breaking apart ranges of IP addresses.
  
```racket
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
```
At construction of our subnet object we map over a list of potential IP addresses 
making system calls to the ping target before we create an individual machine object
that forms a closure over over the address and its open ports.

```racket
(define (probe-ping addr)
    (thread (lambda ()
              (let ((ping-input '()))
                (if (regexp-match? #rx".*64.*" (read-string 4096 (car (process (string-append "ping -c 3 " addr)))))
                    (set! machine-list (cons (machine addr ports protocols) machine-list))
                    "No connection detected")))))
    (for-each (lambda (target-ip) (probe-ping target-ip) ) (range->list targets)))
```

By breaking apart port ranges we use both map recursive procedure to create an iterative process
  
```racket
; from ps3c
; iterative function to create and return a list of number sequentially within a given range
(define (enum-range-i a b)
  (define (enum-range-halper a b total)
    (if (> a b)
        total
        (enum-range-halper (add1 a) b (append total (list a) ))))
  (enum-range-halper a b '()))
```

Powerful quote from SICP
> "It may seem disturbing that we refer to a recursive procedure such as fact-iter as generating an iterative process.However, the process really is iterative: Its state is captured completely by its three state variables, and an interpreter need keep track of only three variables in order to execute the process."


Information about open and closed ports is presented by recursing down their respective lists and matching those numbers to a list of respective servers. 

```racket
(define (all-tports)
  (for-each
   (lambda (machine-dispatch) (begin
                                (printf "IP: ~a\nOpen TCP ports:\n" (machine-dispatch '(ip)))
                                (machine-dispatch '(tports))
                                (printf "\n")))
   machine-list))
   
```

We're using symbolic differentiation to evaluate procedures associated with the object, ie when we call our scanner we pass to it an IP, a list of ports and a list of protocols. These arguments are evaluated within the context of the machine object; passing a 't' signifies that we will be doing TCP, passing '(80) vs '(1 - 80) range of ports

### External Technologies
TCP and UDP 
TCP is a stateful protocol, it follows a set of rules governing connections. In order for a connection
to be established both client and server send a sequence of packets to one another (SYN/ACK) that set up 
handlers for speaking to one another.

UDP is a connectionless protocol and is much harder to determing whether a port is acception connections or not.
Think of video streaming (which happens over UDP) it doesn't matter if you miss a little bit of information during
the process in fact 

### Data Sets or other Source Materials
RCat can be used on LOCALHOST (our own machine can be found at 127.0.0.1)

We have created a list of common ports and matching service in a tab delimited text file sources include:
http://web.mit.edu/rhel-doc/4/RH-DOCS/rhel-sg-en-4/ch-ports.html

Virtual Network built out with VMWare Fusion and the following machines
  * Lubuntu
  * kali linux
  * metasploitable 1
  * metasploitable 2
  * kioptrix


### Deliverable and Demonstration
![net diagram](/demo/network_diagram.png?raw=true "test network diagram")

```racket
> (ips->machines "192.168.74.100-254" "1-443" "t")
> (all-tports)
> (all-tports)
IP: 192.168.74.164
Open TCP ports:

IP: 192.168.74.185
Open TCP ports:
	23	(Telnet)
	21	(FTP -- Control)
	25	(Simple Mail Transfer Protocol (SMTP))
	22	(SSH Remote Login Protocol)
	80	(HTTP)
	139	(NetBIOS Datagram Service)

IP: 192.168.74.135
Open TCP ports:
	139	(NetBIOS Datagram Service)
	22	(SSH Remote Login Protocol)
	80	(HTTP)

IP: 192.168.74.182
Open TCP ports:
	21	(FTP -- Control)
	58	(XNS mail)
	80	(HTTP)

IP: 192.168.74.184
Open TCP ports:
	139	(NetBIOS Datagram Service)
	23	(Telnet)
	111	(ONC RNC)
	21	(FTP -- Control)
	25	(Simple Mail Transfer Protocol (SMTP))
	53	(Domain Name System (DNS))
	22	(SSH Remote Login Protocol)
	80	(HTTP)

IP: 192.168.74.183
Open TCP ports:
	180	
	106	
	110	(POP3)
	25	(Simple Mail Transfer Protocol (SMTP))
	79	(Finger)
	135	
	139	(NetBIOS Datagram Service)
 ```

### Evaluation of Results

Our port scan matches that of NMap for TCP.
Because of the connectionless nature of UDP, the results of UDP scans are unreliable.
The issues can be recreated in NMap as well, NMap evaluates false positives consistently.

## Architecture Diagram

![Arch diagram](/demo/Projecet_Diagram.png?raw=true "Arch diagram")

RCat creates objects which form closures to over IP addresses and open ports of the machine, these objects being an abstraction for a host over a network.
 
Using racket/tcp and racket/udp libraries and threading we create connection attempts with ports on a target machine, and depending on the response determine if chosen ports are open or not.
  
If the ports are open we add them to the respective list of open ports. Because the non-deterministic nature
of network packets, our list of machines acts as a stack.

Using symbolic differentiation and message passing we interact with the machine object to retrieve information.
