(define (member needle ls)
  (cond
   ((null? ls) #f)
   ((= needle (car ls)) #t)
   (else (member needle (cdr ls)))))

(print (member 1 '(1 2 3)))
(print (member 1 '(3 2 1)))
(print (member 1 '(9 8 7)))
;; OUTPUT: #t
;; OUTPUT: #t
;; OUTPUT: #f
