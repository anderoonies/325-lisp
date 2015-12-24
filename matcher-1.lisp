(in-package #:match-tests)

(defun ?not (x y lsts)
  (cond ((null lsts) nil)
        ((match-p (car x) y lsts) nil)
        (t lsts)))

(defun ?or (x y lsts)
  (cond ((null lsts) nil)
        ((null x) nil)
        (t (append (match-p (car x) y lsts)
                   (?or (cdr x) y lsts)))))

;;;(defun ?= (pat y lsts)
;;;  (if (null y) 
;;;    nil
;;;    (match-p (car pat) (call-fn pat y) lsts)))
;;;
;;;(defun ?= (pat y lsts)
;;;  (if (null y)
;;;    nil
;;;    (match-p (car pat) (apply (cadr pat) (list y (caddr pat))) lsts)))

(defun ?= (pat y lsts)
  (if (null y)
    nil
    (match-p (car pat) (apply (cadr pat) 
                              (push y (car (list (cddr pat))))) 
             lsts)))
                              

(defun call-fn (pat y)
  (destructuring-bind (fn &rest args)
   `(,(cadr pat) ,@(list y) ,(caddr pat))
   (apply fn (remove nil args))))
