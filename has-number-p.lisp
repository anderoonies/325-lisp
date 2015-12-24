(defun has-number-p (s)
  (if (listp s)
    (some #'has-number-p s)
    (numberp s)))
