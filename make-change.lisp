(defun make-change (sum &optional (vals '(25 10 5 1)))
  (values-list (get-coins sum vals)))

(defun get-coins (sum vals)
  (if (null vals)
    nil
    (let ((l (multiple-value-bind (num-coins remainder)
               (floor sum (car vals))
               (list num-coins remainder))))
      (cons (car l) (get-coins (cadr l) (cdr vals))))))
