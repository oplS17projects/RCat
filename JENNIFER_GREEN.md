
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

## 1. 


 
## 2. 

## 3. 

## 4. 
