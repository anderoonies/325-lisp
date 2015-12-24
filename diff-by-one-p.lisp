;;; recursive
(defun diff-by-one-p (lst)
  (cond ((null (cdr lst)) t)
        (t
          (and (= 1 (abs (- (car lst) (cadr lst))))
               (diff-by-one-p (cdr lst))))))

;;; do
(defun diff-by-one-p (lst)
  (do* ((remainder lst (rest remainder))
        (prev (first remainder) (first remainder)))
    ((or (null (rest remainder))
         (/= 1 (abs (- (second remainder) prev))))
     (null (rest remainder)))))

;;; mapc and return
(defun diff-by-one-p (lst)
  (mapc #'(lambda (x y)
            (if (null y)
              (throw 'error)
              (unless (= 1 (abs (- y x)))
                (return-from diff-by-one-p nil))))
        lst (rest lst))
  t)

;;; every
(defun diff-by-one-p (lst)
  (every #'(lambda (x y)
             (if (null y)
               (throw 'error)
               (= 1 (abs (- y x)))))
         lst (rest lst)))
