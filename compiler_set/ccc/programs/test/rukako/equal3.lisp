(print (apply (label equal (lambda (x y) (cond ((atom x) (cond ((atom y) (eq x y)) (t nil))) ((equal (car x) (car y)) (equal (cdr x) (cdr y))) (t nil)))) (quote (1 2 9)) (quote (3 2 9))))
;;OUTPUT:nil
