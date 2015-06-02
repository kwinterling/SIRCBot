#lang racket

;; Test variables

(define test-msg ":WiZ!jto@tolsun.oulou.fi NICK Kilroy :Nick change\r\n")

;; Code

(define nick "ScmBot")
(define ident "scmbot")
(define realname "SchemeBot")
(define readbuffer "")
(define port 6667)

(define prefix-px
  (pregexp
   (string-append "(:"
                  "([[:graph:]]+)!"
                  "([[:graph:]]+)@"
                  "([[:graph:]]+) |)"
                  "([[:ascii:]]+)"
                  "(:([[:graph:]]*))|")))

(define not-prefix-px
  (pregexp
   (string-append "([[:ascii:]]+)"
                  "(:([[:graph:]]*))|")))

(define irc-msgs (make-hash))

(struct imsg (nick server msg post))

(define (irping imsg inp outp)
  (begin
    (display "PONG" outp)
    (display " " outp)
    (display (cadr imsg) outp)
    (display "\r\n")))
(hash-set! irc-msgs "PING" irping)

;; :nick!host msg :post

(struct pmsg
  (nick usern host body post))

(struct npmsg
  (body post))
 
(define (msg-match msg)
  (if (equal? (string-ref msg 0) #\:)
      (regexp-match prefix-px msg)
      (regexp-match not-prefix-px msg)))

(define (parse-pmsg msg)
  (let ((ms (msg-match msg)))
    (pmsg (list-ref ms 2) ; nick
          (list-ref ms 3) ; usern
          (list-ref ms 4) ; host
          (list-ref ms 5) ; body
          (list-ref ms 7)))) ; post
         

(define (irc-connect host port)
  (define rbuf "")
  (define rli '())
  (define-values (sckin sckout)
    (tcp-connect host port))
  (display (string-append "NICK" " " nick "\r\n") sckout)
  (display (string-append "USER" " " ident " " host " ur " ":" realname "\r\n") sckout)
  (define (loop)
    (set! rbuf (read-line sckin))
    (set! rli (string-split rbuf))
    (if (empty? rli)
        (set! rli (list "IGNORE"))
        0)
    (if (equal? (car rli) "EXIT")
        0
        (begin
          ((hash-ref irc-msgs (car rli)) rli
                                         sckin
                                         sckout)
          (loop))))
  (loop))

