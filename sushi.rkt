#lang racket/gui
(require racket/trace
         srfi/27)


;gui
(define frame
  (new frame%
       [label "Sushi"]
       [width 500]
       [height 500]))
(define cvs (new canvas% [parent frame]))

;text-field
(define proc-field
  (new radio-box%
     [parent frame]
     [choices '("+" "*" "^" "↑")]
     [style '(horizontal)]
     [label "give me a procedure!"]
     [callback (lambda (t e)
                 (send t get-selection))]))
(define num1-field
  (new text-field%
     [parent frame]
     [label "give me an integer!"]
     ))
(define num2-field
  (new text-field%
     [parent frame]
     [label "give me an integer!"]
     [callback (lambda (t e)
                 (send t get-value))]))
#|(define times-field
  (new text-field%
     [parent frame]
     [label "how many?"]
     [callback (lambda (t e)
                 (send t get-value))]))|#
(define btn
  (new button% [parent frame]
       [label "run"]
       [callback
        (lambda (b e)
          (let* ((dc (send cvs get-dc))
                 (proc (send proc-field get-selection))
                 (p (cond ((= proc 0) +)
                          ((= proc 1) *)
                          ((= proc 2) ^)
                          ((= proc 3) arrow)))
                 ;(t (string->number (send times-field get-value)))
                 (num1 (string->number (send num1-field get-value)))
                 (num2 (string->number (send num2-field get-value)))
                 (val (p num1 num2)))
            (let-values (((x y) (send cvs get-virtual-size)))
              (send dc set-font (make-object font%
                                 (log val)
                                 'default))
              (let loop ((i 0))
                (when (> val i)
                  (send dc set-text-foreground (make-object color%
                                             (random-integer 255)
                                             (random-integer 255)
                                             (random-integer 255)))
                  (send dc draw-text
                        (number->string val)
                        (random-integer x)
                        (random-integer y))
                  (loop (+ i 1)))))))]))
(define btn2
  (new button% [parent frame]
       [label "clear"]
       [callback
        (lambda (b e)
          (let ((dc (send cvs get-dc)))
            (send dc clear)))]))

(send frame show #t)



(define record false)

;strengthen
(define (strengthen func)
  (λ (x y)
    (if (= y 0)
        1
        (func x ((strengthen func) x (- y 1))))))
(define ^
  (strengthen *))
(define arrow
  (case-lambda [() (strengthen ^)]
               [(y) (if (< y 1)
                        (strengthen ^)
                        (strengthen (arrow (- y 1))))]
               [(x y) ((arrow) x y)]
               [r (error "wrong number of args")]))
;fish
(define (b m n)
  (cond ((= m 0) (f n))
        ((= n 0) (begin
                   (set! record (list->string (b (- m 1) 1)))
                   (b (- m 1) 1)))
        (else (b (- m 1) (b m (- n 1))))))
(define (f x)
  (+ x 1))
(define (g x)
  (b x x))
(trace b)