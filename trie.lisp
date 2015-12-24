(defpackage "TRIE"
  (:use "COMMON-LISP")
  (:export "TRIE" 
           "MAKE-TRIE"
           "ADD-WORD"
           "SUBTRIE"
           "TRIE-WORD"
           "TRIE-COUNT"
           "MAPC-TRIE"
           "READ-WORDS"))

(in-package "TRIE")

(defstruct (trie 
             (:print-function
               (lambda (n s d)
                 (format s "#<~A - ~A subwords>"
                         (trie-word n)
                         (trie-count n)))))
  (word nil)
  (wordcount 0)
  (branches nil))

(defun trie-count (trie)
  (trie-wordcount trie))

(defun get-subtrie (branches key)
  (cdr (assoc key branches)))

(defun subtrie (trie &rest chars)
  (do ((keys (cdr chars) (cdr keys))
       (key (first chars) (first keys))
       (subtrie trie (and subtrie (explore-tree key subtrie nil))))
  ((null key) subtrie)))

(defun add-word (word trie &key (depth 0) (len (length word)))
  (cond ((= depth len)
         (setf (trie-word trie) word)
         trie)
        (t
          (incf (trie-wordcount trie))
          (add-word word (explore-tree (char word depth) trie t) :depth (1+ depth) :len len))))

(defun explore-tree (chr trie extend)
  (when extend
    (unless (assoc chr (trie-branches trie))
      (push (cons chr (make-trie)) (trie-branches trie))))
  (get-subtrie (trie-branches trie) chr))

(defun mapc-trie (fun trie)
  (let ((branches (trie-branches trie)))
    (mapc #'(lambda (br) (funcall fun (car br) (cdr br))) branches)
    trie))

(defun read-words (file trie)
  (with-open-file (stream file)
    (do ((line (read-line stream nil)
               (read-line stream nil)))
      ((null line))
      (add-word line trie)))
  trie)
