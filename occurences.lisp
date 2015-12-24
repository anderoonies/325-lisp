;;; The critic complains about quoting nil,
;;; but instructs me to let it slide as '() is ok.
(defun occurrences (l)
  (let ((occs '()))
    (dolist (obj l)
      (cond ((null (get-occurrence obj occs))
             (push (cons obj 1) occs))
            (t (add-occurrence obj occs)))
     )
    (sort occs #'> :key #'cdr)))
             
(defun get-occurrence (val occs)
  (cdr (assoc val occs)))

(defun add-occurrence (val occs)
  (INCF (cdr (assoc val occs))))

;;; REVISED CODE
(defun occurrences (l)
  (do ((lst l (rest lst))
       (count-list nil (update-list (car lst) count-list)))
      ((null lst) (sort count-list #'> :key #'cdr))))

(defun update-list (val count-list)
  (let ((occ (cdr (assoc val count-list))))
    (if (null occ)
      (push (cons val 1) count-list)
      (incf (cdr (assoc val count-list))))
    count-list))

