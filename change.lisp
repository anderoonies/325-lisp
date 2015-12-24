(defun make-best-change (total coin-values)
  (construct-tables total)
  (do ((p 1 (1+ p)))
    ((= p total))
    (let ((min-coins (1+ total))
          (coin nil))
      (do ((i 1 (1+ i)))
        ((= i (length coin-values)))
        (let ((current-coin (elt coin-values i)))
          (cond ((<= current-coin p)
            (let ((num-coins (+ 1 (aref *memo-table*
                                        (- p current-coin)))))
              (cond ((< num-coins min-coins)
                     (setf min-coins num-coins)
                     (setf coin current-coin))))))))
      (setf (aref *memo-table* p) min-coins)
      (setf (aref *coin-table* p) coin)))
  *coin-table*)


(defun construct-tables (total)
  (defparameter *memo-table*
    (make-array total))
  (defparameter *coin-table*
    (make-array total)))

