;;; recursive
(defun position+ (lst &optional (idx 0))
  (position+ lst 0))

(defun position+ (lst &optional (idx 0))
  (if (null lst) nil
      (let ((el (car lst)))
        (cons (+ idx el) (position+ (rest lst) (1+ idx))))))

;;; iterative
(defun position+ (lst)
  (do ((l lst (rest l))
       (idx 0 (1+ idx))
       (result nil (cons (+ idx (car l)) result)))
       ((null l) (reverse result))))

;;; mapcar
(defun position+ (lst)
  (let ((idx -1))
    (mapcar (lambda (x) (incf idx) (+ x idx)) lst)))

