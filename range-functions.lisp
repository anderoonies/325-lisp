(defun get-increment (start end)
  (if (> end start)
    1
    -1))

(defun map-range (fn start end)
  (loop with inc = (get-increment start end)
        for x = start then (+ x inc) 
        until (= x end)
        collect (funcall fn x)))

(defun every-range (fn start end)
  (loop with inc = (get-increment start end)
        for x = start then (+ x inc)
        until (= x end)
        always (funcall fn x)))

(defun find-range (fn start end)
  (loop with inc = (get-increment start end) 
        for x = start 
        then (+ x inc)
        until (= x end)
        when (funcall fn x)
          do (return x)))
