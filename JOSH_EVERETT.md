
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

  (begin (if (string=? protocols "t")
             (map (lambda (x) (probe-tcp x)) (enum-ports ports))
             (map (lambda (x) (probe-udp x)) (enum-ports ports)))
         dispatch))
 ```
A dozen or so lines have been gutted from the previous definition for the sake of brevity. We just need the understanding
that our machine object constructor evaluates differentely between "t" and "u" protocols-- and that the result of 
evaluating a machine procedure is a dispatch procedure that allows for message passing.
 
## 2. Differentiation between a single and multiple addresses

When the user interacts with the program they pass the target IP of their scan as a string - either a single address 
or a range of addresses within a /24 subnet mask (ie "192.168.1.1-254" ).
A range of IPs will be mapped over using the for-each procedure (as described by SICP, for-each is map
but without the list being returned, it is only evaluated for the side effects of the procedure) to detmerine
if the machine is alive.

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
machine's dispatch procedure

```
(define (list-children folder-id . next-page-token)
  (read-json
   (get-pure-port
    (string->url (string-append "https://www.googleapis.com/drive/v3/files?"
                                "q='" folder-id "'+in+parents"
                                "&key=" (send drive-client get-id)
                                (if (= 1 (length next-page-token))
                                    (string-append "&pageToken=" (car next-page-token))
                                    "")
;                                "&pageSize=5"
                                ))
    token)))
```
The interesting routine is ```list-all-children```. This routine is directly invoked by the user.
It optionally accepts a page token; when it's used at top level this parameter will be null.

The routine uses ```let*``` to retrieve one page of results (using the above ```list-children``` procedure)
and also possibly obtain a token for the next page.

If there is a need to get more pages, the routine uses ```append``` to pre-pend the current results with 
a recursive call to get the next page (and possibly more pages).

Ultimately, when there are no more pages to be had, the routine terminates and returns the current page. 

This then generates a recursive process from the recursive definition.

```
(define (list-all-children folder-id . next-page-token)
  (let* ((this-page (if (= 0 (length next-page-token))
                      (list-children folder-id)
                      (list-children folder-id (car next-page-token))))
         (page-token (hash-ref this-page 'nextPageToken #f)))
    (if page-token
        (append (get-files this-page)
              (list-all-children folder-id page-token))
        (get-files this-page))))
```

## 4. Filtering a List of File Objects for Only Those of Folder Type

The ```list-all-children``` procedure creates a list of all objects contained within a given folder.
These objects include the files themselves and other folders.

The ```filter``` abstraction is then used with the ```folder?``` predicate to make a list of subfolders
contained in a given folder:

```
(define (list-folders folder-id)
  (filter folder? (list-all-children folder-id)))
```

## 5. Recursive Descent on a Folder Hierarchy

These procedures are used together in ```list-all-folders```, which accepts a folder ID and recursively
obtains the folders at the current level and then recursively calls itself to descend completely into the folder
hierarchy.

```map``` and ```flatten``` are used to accomplish the recursive descent:

```
(define (list-all-folders folder-id)
  (let ((this-level (list-folders folder-id)))
    (begin
      (display (length this-level)) (display "... ")
      (append this-level
              (flatten (map list-all-folders (map get-id this-level)))))))
```
