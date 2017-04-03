# RCat

### Statement
RCat aims to be a network scanner for both TCP and UDP with basic service to port mapping.

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
Our project will depend on the ability to connect with systems at run time to store the results that are individual to that system. Sure, we could scan localhost, but what is the fun in that?

### Data Sets or other Source Materials
We will be building a set of data from a mapping of common ports to their services. This list will be used by the machine object to make a best guess at what the port might be running if open.
Method for storage and retrieval of this information is undecided at this point.
http://web.mit.edu/rhel-doc/4/RH-DOCS/rhel-sg-en-4/ch-ports.html

We will also be building out a series of virtual machines in a private subnet to be used as target machines for our demonstration and testing. 

### Deliverable and Demonstration
Our final product will be give results similar to when using netcat or NCat as a network scanner, but will map common ports to the service they are associated with. One of the bigger differentiators between our deliverable product and NMap is that NMap employs service and version detection by parsing responses and comparing with a database.

We would like a couple components to be involved in our demo. We will create a private network using several virtual machines exposing a variety of services that we will scan against. Some services/machines will be protected by firewalls and we will discuss the implications. We will also scan several internet facing sites that could be considered kosher to scan, ie http://scanme.nmap.org or google's public DNS servers such as 8.8.8.8

We will compare results of our scanner to actual open ports of the systems using netstat.
We would also like to directly compare our program to NMap, the defacto go-to opensource network scannner. We would like to compare the accuracy of our results and the runtime.

We can also compare with NetCat's scanning abilities.

### Evaluation of Results
Since we will be building our private network using virtual machines we will have access to the ports open on the machine. We can compare the results of our scanner with that of NMap as well as reference the machines port listing using netstat. A succesfull product means we should see results of our scans matching expectations.

We are looking to match common services with port numbers, but won't be doing any form of version detection. That means we could use netcat to open up port 80 on a virtual machine and our scanner would detect it as a generic web server.

## Architecture Diagram
![low resolution](/Low_Res.png?raw=true "low res architecture")

![high resolution](/machineObject.png?raw=true "whiteboard environemt diagram")

![high resolution](/High_Res.png?raw=true "high res architecture")



## Schedule

### Major components and challenges breakdown
* threading
* custodians
* exception handling
* UDP
* TCP
* Machine object
  * constructor
  * accessors
  * mutators (scanner procedure)
* Stretch goal 
  * subnet object
* Super stretch goal
  * file transfers and message passing over socket connection, emulating NCat
* Super, SUPER stretch goal 
  * SSL option, emulating NCat
* Can we?
  * compile our racket program into an executable file
   * on a seperate machine open a port using netcat to redirect input into new executable file.
   * using *RCat* on original host machine push the compiled executable binary to the waiting machine
   * on waiting machine run compiled binary
   * two copies of RC
  

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
test stub for _machine_ class constructor
completing tcp connection acceptance/drop
scanner object threading
VM network


### Jennifer Green @goldenapplepie

