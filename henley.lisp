(defparameter *words* (make-hash-table :size 10000))

(defconstant maxword 100)

;;;(defun read-text (pathname)
;;;  (with-open-file (s pathname :direction :input)
;;;    (let ((buffer (make-string maxword))
;;;          (pos 0))
;;;      (do ((c (read-char s nil :eof) 
;;;              (read-char s nil :eof)))
;;;          ((eql c :eof))
;;;        (if (or (alpha-char-p c) (char= c #\'))
;;;            (progn
;;;              (setf (aref buffer pos) c)
;;;              (incf pos))
;;;            (progn
;;;              (unless (zerop pos)
;;;                (see (intern (string-downcase 
;;;                               (subseq buffer 0 pos))))
;;;                (setf pos 0))
;;;              (let ((p (punc c)))
;;;                (if p (see p)))))))))

(defun read-stream (str fn)
  (let ((buffer (make-string maxword))
        (pos 0))
    (do ((c (read-char str nil :eof)
           (read-char str nil :eof)))
      ((eql c :eof))
      (cond ((or (alpha-char-p c) (char= c #\'))
             (setf (aref buffer pos) c)
             (incf pos))
            (t
              (unless (zerop pos)
                (funcall fn (subseq buffer 0 pos)))
              (setf pos 0))))))

(defun record-word (word)
  (see (intern (string-downcase word)))
  (let ((p (punc word)))
    (if p (see p))))

(defun read-text (pathname)
  (with-open-file (s pathname :direction :input)
    (read-stream s #'record-word))
  (hash-table-count *words*))

;;; go through the stream
;;; if we run into the current word, knock it off
;;; then we're looking for the word after it
;;; continue until there are no more words left.
;;; this would probably be easier to do just looking
;;; at the hash table but the problem specifies
;;; that henley needs to call read-stream
(defun henley-p (str &optional (pathname "paradise-lost.txt"))
  (with-open-file (s pathname :direction :input)
    (read-stream s (lambda (x)
                     (let ((buffer (make-string (length str)))
                           (pos 0))
                       (cond ((= (search x str) pos)
                              ( 

(defun punc (c)
  (case c
    (#\. '|.|) (#\, '|,|) (#\; '|;|) 
    (#\! '|!|) (#\? '|?|) ))

(let ((prev `|.|))
  (defun see (symb)
    (let ((pair (assoc symb (gethash prev *words*))))
      (if (null pair)
          (push (cons symb 1) (gethash prev *words*))
          (incf (cdr pair))))
    (setf prev symb)))

(defun generate-text (n)
  (do ((i 0 (1+ i))
       (word (random-next '|.|) (random-next word)))
    ((= n i)(terpri))
    (format t "~A " word)))

(defun random-next (prev)
  (let* ((choices (gethash prev *words*))
         (i (random (reduce #'+ choices 
                            :key #'cdr))))
    (dolist (pair choices)
      (if (minusp (decf i (cdr pair)))
          (return (car pair))))))
