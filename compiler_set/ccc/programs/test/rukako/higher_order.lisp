(print (apply (label map (lambda (f x) (cond ((atom x) x) (t (cons (f (car x)) (map f (cdr x))))))) (quote (lambda (x) (cons x 4))) (quote (1 2 3))))
;;OUTPUT:((1 . 4) (2 . 4) (3 . 4))
