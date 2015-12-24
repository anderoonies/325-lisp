(defclass tokenizer ()         
  ((delim :accessor tr-delim
          :initform #\space)   
   (str :accessor tr-string)
   (index :accessor tr-index
          :initform 0)))

(defun make-tokenizer (str delim)
  (let ((tr (make-instance 'tokenizer)))
    (setf (tr-delim tr) delim)
    (setf (tr-string tr) str)
    tr))

(defun split-string (str &optional (delim #\space))
  (let ((tr (make-tokenizer str delim)))
    (do ((l nil (cons (next-token tr) l)))
      ((not (next-token-p tr)) (nreverse l)))))

(defmethod next-token-index ((tr tokenizer) current)
  (let ((current-char (char (tr-string tr) current))
        (next-index (1+ current)))
    (if (eql current-char (tr-delim tr))
      (position-if-not (lambda (x)
                         (eql x (tr-delim tr)))
                       (tr-string tr)
                       :start next-index)
      (position (tr-delim tr) (tr-string tr) :start next-index))))

(defmethod next-token-p ((tr tokenizer))
  (not (null (tr-index tr))))

(defmethod next-token ((tr tokenizer))
  (let ((start (tr-index tr))
        (end (next-token-index tr (tr-index tr))))
    (setf (tr-index tr) end)
    (if (null start)
      nil
      (let ((token (subseq (tr-string tr) start end)))
        (cond ((find (tr-delim tr) token)
               (unless (eql (tr-delim tr) #\space)
                 ""))
              (t
                token))))))
