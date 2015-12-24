(defpackage "BOGGLE"
  (:use :common-lisp trie)
  (:export :solve-boggle
           :load-words))

(in-package :boggle)

(defvar *trie* (make-trie))

(defun solve-boggle (board)
  (defparameter *adj* (make-adj board))
  (let* ((n (isqrt (length board)))
         (words (loop for row from 0 to (1- n)
                      append (loop for col from 0 to (1- n)
                                   append (find-word row col board *trie*)))))
    (sort
      (sort (remove-duplicates words) #'string<)
       #'> :key #'length)))

(defun make-adj (board)
    (let ((n (isqrt (length board))))
        (loop for row from 0 to (1- n)
              append (loop for col from 0 to (1- n)
                           append (list (get-neighbors row col board))))))

(defun valid-word-p (word)
  (> (length word) 2))

(defun find-word (row col board trie &optional (vlst nil))
  (let ((trie (next-subtrie trie row col board)))
    (unless (null trie)
      (let ((words (mapcan (lambda (neighbor)
                             (unless (member neighbor vlst :test #'equal)
                               (find-word (car neighbor) 
                                          (cdr neighbor) 
                                          board 
                                          trie 
                                          (acons row col vlst))))
                             (letter-neighbors row col))))
        (when (valid-word-p (trie-word trie))
          (push (trie-word trie) words))
        words))))

(defun letter-neighbors (row col)
  (elt *adj* (+ (* (isqrt (length *adj*)) row) col)))
    
(defun load-words (pathname)
  (with-open-file (stream pathname)
    (do ((line (read-line stream nil)
               (read-line stream nil)))
      ((null line))
      (add-word line *trie*))))

(defun get-neighbors (row col board)
  (loop for y from (1- row) to (1+ row)
        append (loop for x from (1- col) to (1+ col)
                     when (and (valid-letter-p y x (length board))
                               (diff-letter-p y x row col))
                     append (list (cons y x)))))

(defun valid-letter-p (col row size)
  (let ((s (isqrt size)))
    (and (< -1 row s)
         (< -1 col s))))

(defun diff-letter-p (y x row col)
  (not (and (= x col) (= y row))))

(defun get-letter (row col board)
  (elt board (+ (* row (isqrt (length board))) col)))

(defun next-subtrie (trie row col board)
  (let ((l (get-letter row col board)))
    (if (eql #\q l)
      (subtrie trie #\q #\u)
      (subtrie trie l))))
