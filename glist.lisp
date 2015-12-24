;;; Based on generated lists in Charniak, Riesbeck, McDermott and Meehan's
;;; AI Programming, 2nd ed. (Google "lisp lazy lists" for similar efforts.)

;;; Updates:
;;;
;;; 03/11/08 Changed GLIST-LIST to GEXTRACT [CKR]
;;; 01/15/06 Fixed some comments, added GCONS [CKR]
;;; 05/01/04 Added GLIST-LIST, generalized GPOP to handle places [CKR]

;;; A generated list (glist) is either a list, a list with
;;; a generator, or a generator that returns a glist.
;;; A generator is an object that initially contains a
;;; delayed evaluation form. A generator is created with
;;; DELAY and evaluated with FORCE. The glist functions use
;;; generators to implement generated (or lazy) lists.

;;; (DELAY exp ...) => generator  [macro]
;;;   Returns a generator, i.e., a delayed evaluation form.
;;;
;;; (GCONS x y) => glist [macro]
;;;   Same as (CONS x (DELAY y)).
;;;   
;;; GAPPEND, GCAR, GCDR, GNULL, GPOP
;;;   Equivalents of Lisp functions that handle generated 
;;;   lists.
;;;   Always use GCAR, GCDR, GNULL and GPOP to access elements
;;;   of generated lists. GCAR and GPOP always return an actual
;;;   element or NIL.
;;;   Always use GCONS or GAPPEND to construct generated lists.
;;;
;;; (GEXTRACT glist &optional limit) => list
;;;   Returns a list of the first limit elements in generator
;;;   Default: return all elements -- don't do this on 
;;;   infinite lists!


(defpackage #:glist
  (:use #:common-lisp)
  (:export #:delay #:gappend #:gcar #:gcdr #:gcons
           #:gextract #:gnull #:gpop))

(in-package #:glist)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public Forms and Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro delay (&rest exp)
  `(make-gen :closure #'(lambda () ,@exp)))

(defmacro gcons (x y)
  `(cons ,x (delay ,y)))

(defun gcar (glist) (car (normalize glist)))
(defun gcdr (glist) (cdr (normalize glist)))
(defun gnull (glist) (null (normalize glist)))

(defun gappend (glist1 glist2)
  (cond ((gnull glist1) glist2)
        (t
         (cons (gcar glist1)
               (delay (gappend (gcdr glist1) glist2))))))

;;; Based on fixed version of XPOP example in Hyperspec
(defmacro gpop (place &environment env)
  (multiple-value-bind (dummies vals new setter getter)
      (get-setf-expansion place env)
    (if (cdr new) (error "Can't expand (GPOP ~S)." place)
      `(let* (,@(mapcar #'list dummies vals) (,(car new) ,getter))
         (prog1 (gcar ,(car new))
           (setq ,(car new) (gcdr ,(car new)))
           ,setter)))))
        

(defun gextract (glist &optional limit)
  (do ((l nil (cons (gpop glist) l)))
      ((or (and limit (< limit 1))
           (gnull glist))
       (nreverse l))
    (and limit (decf limit))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Private Structures and Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; GEN -- structure
;;;   A generator that holds either a closure or the value
;;;   resulting from calling that closure.
;;;
;;; (FORCE generator) => value
;;;   Gets the value of the generator. The closure is
;;;   removed after calling so that it's never called more
;;;   than once.
;;;
;;; (NORMALIZE l) => list
;;;   Given a generator or a list beginning with a 
;;;   generator, repeatedly uses FORCE until there is a
;;;   "real" list, either empty, or starting with at least
;;;   one non-generator.

(defstruct (gen (:print-function print-gen))
  closure value)

(defun print-gen (gen stream depth)
  (declare (ignore depth))
  (format stream "#<gen ~S>"
          (or (gen-closure gen) (gen-value gen))))

(defun force (gen)
  (cond ((not (gen-p gen)) gen)
        ((null (gen-closure gen)) (gen-value gen))
        (t
         (psetf
           (gen-value gen) (funcall (gen-closure gen))
           (gen-closure gen) nil)
         (gen-value gen))))

(defun normalize (glist)
  (cond ((null glist) nil)
        ((gen-p glist) (normalize (force glist)))
        ((gen-p (car glist))
         (normalize (append (normalize (car glist)) (cdr glist))))
        (t glist)))

(provide "glist")
