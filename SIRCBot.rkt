#lang racket

;; Test variables

(define test-msg ":WiZ!jto@tolsun.oulou.fi NICK Kilroy\r\n")

;; Code

(define nick "ScmBot")
(define ident "scmbot")
(define realname "SchemeBot")
(define readbuffer "")
(define port 6667)

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

(define (parse-msg smsg)
  (let ((rs
         (map (lambda (mpart) (string-split mpart)) (string-split smsg ":"))))
    (car rs)))


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
                                         sckout)
          (loop))))
  (loop))

