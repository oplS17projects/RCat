# Project Title Goes Here (10 words maximum)

### Statement
RCat aims to be a high-level network scanner for both TCP and UDP with basic service detection.
### Analysis

>- Will you use data abstraction? How?
We plan to abstract individual IP addresses as 'Machine' objects and create a closure over the IP ( stored as a string ), a list of open TCP ports and a list of IP ports.
>- Will you use recursion? How?
Information about open and closed ports will be presented by recursing down their respective lists and matching those numbers to a list of respective servers.
We will be creating threaded procedures to make connection attempts. These procedures will be evaluated by combining them into a list of targeted 
>- Will you use map/filter/reduce? How? 
At evaluation our machine object will create a list of ports to scan. This list will be mapped over by the individiual scan procedure, which will create a threaded connection attempt against the IP and port. 
Our lists of open ports will be filtered over by the accessor procedures of the machine object to return appropriate information about the machine. 
ie (machine1 'open-port? '(22 80)) will filter our list of open ports for port 22 and port 80.
The result will be applied to another filter which maps over a list of pairings between ports and services and returns the matching port and service of opened ports.
>- Will you use object-orientation? How?
Machine objects are the fundamental building block of our project. Ideally we would like to be able to differentiate individual machines from subnets or ranges of machines.
192.168.1.1 vs 192.168.1.1/24
>- Will you use functional approaches to processing your data? How?
The intent is to recurse across all of our data structures to evaluate all calls.
>- Will you use state-modification approaches? How? (If so, this should be encapsulated within objects. `set!` pretty much should only exist inside an object.)

>- Will you build an expression evaluator, like we did in the symbolic differentatior and the metacircular evaluator?
We will be using symbolic differentiation to evaluate the object calls, ie when we call our scanner we pass to it an IP, a list of ports and a list of protocols. These arguments will be evaluated within the context of our objects; passing a 't' signifies that we will be doing TCP, passing (80) will scan a single port vs (1 80) two ports vs (1 - 80) range of ports
- Will you use lazy evaluation approaches?

>The idea here is to identify what ideas from the class you will use in carrying out your project. 


### External Technologies
Our project will depend on the ability to connect with systems dynamically at run time to produce results that are individual to that system. Sure, we could scan localhost, but what is the fun in that?

### Data Sets or other Source Materials
http://web.mit.edu/rhel-doc/4/RH-DOCS/rhel-sg-en-4/ch-ports.html

>If you will be working with existing data, where will you get those data from? (Dowload from a website? Access in a database? Create in a simulation you will build? ...)

>How will you convert your data into a form usable for your project?  

>If you are pulling data from somewhere, actually go download it and look at it before writing the proposal. Explain in some detail what your plan is for accomplishing the necessary processing.

>If you are using some other starting materials, explain what they are. Basically: anything you plan to use that isn't code.

### Deliverable and Demonstration
We would like a couple components to be involved in our demo. We will create a private network using several virtual machines exposing a variety of services that we will scan against. Some services/machines will be protected by firewalls and we will discuss the implications. We will also scan several internet facing sites that could be considered kosher to scan, ie http://scanme.nmap.org or google's public DNS servers such as 8.8.8.8

We will compare results of our scanner to actual open ports of the systems.
We would also like to directly compare our program to NMap, the defacto go-to opensource network scannner. We would like to compare the accuracy of our results and the runtime to that of NMaps.
We can also compare with NCat by using it as a scanner.


>Explain exactly what you'll have at the end. What will it be able to do at the live demo?

>What exactly will you produce at the end of the project? A piece of software, yes, but what will it do? Here are some questions to think about (and answer depending on your application).

>Will it run on some data, like batch mode? Will you present some analytical results of the processing? How can it be re-run on different source data?

>Will it be interactive? Can you show it working? This project involves a live demo, so interactivity is good.

### Evaluation of Results
Since we will be building our private network using virtual machines we will have access to the ports open on the machine. We can compare the results of our scanner with that of NMap as well as reference the machines port listing using netstat. 

We are looking to match common services with port numbers, but won't be doing any form of version detection. That means we could use netcat to open up port 80 on a virtual machine and our scanner would detect it as a generic web server.
## Architecture Diagram
>Upload the architecture diagram you made for your slide presentation to your repository, and include it in-line here.

>Create several paragraphs of narrative to explain the pieces and how they interoperate.

## Schedule
>Explain how you will go from proposal to finished product. 

>There are three deliverable milestones to explicitly define, below.

>The nature of deliverables depend on your project, but may include things like processed data ready for import, core algorithms implemented, interface design prototyped, etc. 

>You will be expected to turn in code, documentation, and data (as appropriate) at each of these stages.

>Write concrete steps for your schedule to move from concept to working system. 

### First Milestone (Sun Apr 9)
>Which portion of the work will be completed (and committed to Github) by this day? 

### Second Milestone (Sun Apr 16)
>Which portion of the work will be completed (and committed to Github) by this day?  

### Public Presentation (Mon Apr 24, Wed Apr 26, or Fri Apr 28 [your date to be determined later])
>What additionally will be completed before the public presentation?

## Group Responsibilities

### Josh Everett @josh-everett
test stub for _machine_ class

### Jennifer Green @goldenapplepie

