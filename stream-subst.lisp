(in-package :cs325-user)

;;; From Paul Graham's ANSI Common Lisp
;;; Figure 7.2, Page 129
;;;
;;; Requires fixed version of buf.lisp

(defun file-subst (old new file1 file2)
  (with-open-file (in file1 :direction :input)
     (with-open-file (out file2 :direction :output
                                :if-exists :supersede)
       (stream-subst old new in out))))

(defun stream-subst (old new in out &key (wildcard #\+))
  (let* ((pos 0)
         (len (length old))
         (buf (new-buf len))
         (from-buf nil))
    (do ((c (read-char in nil :eof)
            (or (setf from-buf (buf-next buf))
                (read-char in nil :eof))))
        ((eql c :eof))
      (cond ((matching-char-p (elt old pos) c wildcard)
             (incf pos)
             (cond ((= pos len)            ; 3
                    (princ new out)
                    (setq pos 0)
                    (buf-clear buf))
                   ((not from-buf)         ; 2
                    (buf-insert c buf))))
            ((zerop pos)                   ; 1
             (princ c out)
             (when from-buf
               (buf-pop buf)
               (buf-reset buf)))
            (t                             ; 4
             (unless from-buf
               (buf-insert c buf))
             (princ (buf-pop buf) out)
             (buf-reset buf)
             (setf pos 0))))
    (buf-flush buf out)))

(defun matching-char-p (old new wildcard)
  (or (and (eql old :digit) (digit-char-p new))
      (and (eql old :alpha) (alpha-char-p new))
      (and (eql old :alphanumeric)
           (or
             (digit-char-p new)
             (alpha-char-p new)))
      (eql old new)
      (eql old :wild)
      (eql old wildcard)))
