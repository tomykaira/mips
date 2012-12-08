((label equal (lambda (x y) (cond ((atom x) (cond ((atom y) (eq x y)) (t nil))) ((equal (car x) (car y)) (equal (cdr x) (cdr y))) (t f)))) (1 2 (3 . 5) (3 4) 9) (1 2 (3 . 5) (3 4) 9))
;;OUTPUT:t
