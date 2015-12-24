(defun max-min (vec &key (max nil) (min nil) (start 0) (end (length vec)))
  (cond ((= start end)
         (values max min))
        (t 
          (let* ((val (svref vec start))
                 (max (if (or (null max)
                            (> val max))
                        val
                        max))
                 (min (if (or (null min)
                            (< val min))
                        val
                        min)))
            (max-min vec :max max :min min :start (1+ start) :end end)))))

(defun max-min (vec &key maxv minv (start 0) (end (length vec)))
  (if (= start end)
    (values maxv minv)
    (let ((val (svref vec start)))
      (max-min vec 
               :maxv (max val (or maxv val))
               :minv (min val (or minv val))
               :start (1+ start)
               :end end))))

