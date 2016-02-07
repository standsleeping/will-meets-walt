;;;;henley.lisp

(defpackage "HENLEY"
	(:use "COMMON-LISP")
	(:export "*WORDS*"
					 "MAXWORD"
					 "READ-TEXT"
					 "PUNC"
					 "SEE"
					 "GENERATE-TEXT"
					 "RANDOM-NEXT"))

(defparameter *words* (make-hash-table :size 10000))

(defconstant maxword 100)

(defun random-next (prev)
  (let* ((choices (gethash prev *words*))
         (i (random (reduce #'+ choices 
                            :key #'cdr))))
    (dolist (pair choices)
      (if (minusp (decf i (cdr pair)))
          (return (car pair))))))

;; (gethash '|for| *words*)
;; (reduce #'+ (gethash '|for| *words*) :key #'cdr)

(defun generate-text (n &optional (prev '|.|))
  (if (zerop n)
      (terpri)
      (let ((next (random-next prev)))
        (format t "~A " next)
        (generate-text (1- n) next))))

(let ((prev `|.|))
  (defun see (symb)
    (let ((pair (assoc symb (gethash prev *words*)))) ; get the assoc
      (if (null pair)
          (push (cons symb 1) (gethash prev *words*)) ; make an assoc
          (incf (cdr pair)))) ; incf the occurrence
    (setf prev symb))) ; set us up for next time

(defun punc (c)
  (case c
    (#\. '|.|) (#\, '|,|) (#\; '|;|) 
    (#\! '|!|) (#\? '|?|) ))


(defun read-text (pathname)
  (with-open-file (s pathname :direction :input)
    (let ((buffer (make-string maxword)) ; buffer = our string to load chars
          (pos 0))
      (do ((c (read-char s nil :eof) 
              (read-char s nil :eof)))
          ((eql c :eof))
        (if (or (alpha-char-p c) (char= c #\'))
            (progn  ; if a letter, add to string
              (setf (aref buffer pos) c)
              (incf pos))
            (progn  ; if not a letter, process the finished word
              (unless (zerop pos)
                (see (intern (string-downcase 
                               (subseq buffer 0 pos))))
                (setf pos 0))
              (let ((p (punc c)))
                (if p (see p)))))))))