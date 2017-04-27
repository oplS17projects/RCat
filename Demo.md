# RCat

### Statement
RCat is a port scanner scanner that can do basic port to service matching. 
Capable of scanning a single or multiple machines over a network for open TCP ports. 
Multiple machines over a /24 network may be scanned, but are subject to a ICMP ping before they are considered valid.
UDP scanning is functional and unreliable as we discuss.

### Analysis

> Will you use data abstraction? How?
- We plan to abstract individual IP addresses as 'Machine' objects and create a closure over the IP ( stored as a string ), a list of open TCP ports and a list of IP ports.

> Will you use recursion? How?
- Information about open and closed ports will be presented by recursing down their respective lists and matching those numbers to a list of respective servers.
We will be creating threaded procedures to make connection attempts. These procedures will be evaluated by combining them into a list of targeted 

> Will you use map/filter/reduce? How? 
- At evaluation our machine object will create a list of ports to scan. This list will be mapped over by the individiual scan procedure, which will create a threaded connection attempt against the IP and port. 
Our lists of open ports will be filtered over by the accessor procedures of the machine object to return appropriate information about the machine. 
ie (machine1 'open-port? '(22 80)) will filter our list of open ports for port 22 and port 80.
The result will be applied to another filter which maps over a list of pairings between ports and services and returns the matching port and service of opened ports.

> Will you use object-orientation? How?
- Machine objects are the fundamental building block of our project. Ideally we would like to be able to differentiate individual machines from subnets or ranges of machines.
192.168.1.1 vs 192.168.1.1/24

> Will you use functional approaches to processing your data? How?
- The intent is to recurse across all of our data structures during evaluations. We will actively be avoiding using set! in favor of creating data structures that we rucurse across.

> Will you build an expression evaluator, like we did in the symbolic differentatior and the metacircular evaluator?
- We will be using symbolic differentiation to evaluate procedures associated with the object, ie when we call our scanner we pass to it an IP, a list of ports and a list of protocols. These arguments will be evaluated within the context of the machine object; passing a 't' signifies that we will be doing TCP, passing '(80) will scan a single port vs '(1 80) two ports vs '(1 - 80) range of ports

### External Technologies
TCP and UDP discussion

### Data Sets or other Source Materials
RCat can be used on LOCALHOST (our own machine can be found at 127.0.0.1)

We have created a list of common ports and matching service in a tab delimited text file sources include:
http://web.mit.edu/rhel-doc/4/RH-DOCS/rhel-sg-en-4/ch-ports.html

Virtual Network built out with VMWare Fusion and the following machines
> link Lowell VMWare store 
> link Lubuntu
> link kali linux
> link metasploitable
> link de-ice
> link kioptrix
> link vulnhub


### Deliverable and Demonstration
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

> new diagram

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
