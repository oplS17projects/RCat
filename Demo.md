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
  
      
Because we both elements of our program form closures over lists we use map and variations of map constantly.
  
  * breaking apart ranges of IP addresses.
  
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

  * breaking apart port ranges we use both map recursive procedure to create an iterative process
  
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




> Will you use data abstraction? How?
- We abstract individual IP addresses as 'Machine' objects and create a closure over the IP ( stored as a string ), a list of open TCP ports and a list of IP ports.

> Will you use recursion? How?
- Information about open and closed ports is presented by recursing down their respective lists and matching those numbers to a list of respective servers.
We have created threaded procedures to make connection attempts. These procedures will be evaluated by combining them into a list of targeted   [missing words?]

> Will you use map/filter/reduce? How? 
- At evaluation our machine object creates a list of ports to scan. This list is then mapped over by the individiual scan procedure, which creates a threaded connection attempt against the IP and port. 
Our lists of open ports is filtered over by the accessor procedures of the machine object to return appropriate information about the machine. 
ie (machine1 'open-port? '(22 80)) filters our list of open ports for port 22 and port 80.
The result is applied to another filter which maps over a list of pairings between ports and services and returns the matching port and service of the opened port
> Will you use functional approaches to processing your data? How?
- The intent is to recurse across all of our data structures during evaluations. We are actively avoiding using set! in favor of creating data structures that we recurse across.

> Will you build an expression evaluator, like we did in the symbolic differentatior and the metacircular evaluator?
- We're using symbolic differentiation to evaluate procedures associated with the object, ie when we call our scanner we pass to it an IP, a list of ports and a list of protocols. These arguments are evaluated within the context of the machine object; passing a 't' signifies that we will be doing TCP, passing '(80) will scan a single port vs '(1 80) two ports vs '(1 - 80) range of ports

### External Technologies
TCP and UDP discussion

### Data Sets or other Source Materials
RCat can be used on LOCALHOST (our own machine can be found at 127.0.0.1)

We have created a list of common ports and matching service in a tab delimited text file sources include:
http://web.mit.edu/rhel-doc/4/RH-DOCS/rhel-sg-en-4/ch-ports.html

Virtual Network built out with VMWare Fusion and the following machines
  * link Lowell VMWare store 
  * link Lubuntu
  * link kali linux
  * link metasploitable
  * link de-ice
  * link kioptrix
  * link vulnhub


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

> Compare virtual network results with that of NMap

## Architecture Diagram

![Arch diagram](/demo/Projecet_Diagram.png?raw=true "Arch diagram")

The goal is to create objects which form closures to over IP addresses and open ports of the machine, these objects being an abstraction for a host over a network.
 
Using racket/tcp and racket/udp libraries and threading we will create connection attempts with ports of a target machine, and depending on the response determine if chosen ports are open or not.
  
If the ports are open we add them to the respective list of open ports.

Using symbolic differentiation and message passing we interact with the machine object to retrieve information.


## Schedule


### First Milestone (Sun Apr 9)
* Machine Object with procedure stubs
 * constructor
 * accessors
 * scanner object stub


### Second Milestone (Sun Apr 16)
* data preparation complete for matching port number to service
* properly differentiating dropped connections from accepted connections

### Public Presentation (Mon Apr 24, Wed Apr 26, or Fri Apr 28 [your date to be determined later])
* Threading
* Building out network of virtual machines for demonstration

## Group Responsibilities

### Josh Everett @josh-everett
* test stub for _machine_ class constructor
* completing tcp connection acceptance/drop
* scanner object threading
* VM network


### Jennifer Green @goldenapplepie
* completing udp connection acceptance/drop
* creating data structures for holding port data list
* matching port data against list of known ports
