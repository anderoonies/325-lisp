(defun horner (x &rest cfs)
  (reduce #'(lambda (cf1 cf2)
              (+ (* cf1 x) cf2))
          cfs))
