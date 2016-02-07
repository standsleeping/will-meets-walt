;; Current working directory: (sb-posix:getcwd) 

(defpackage "MY-UTILITIES"
  (:use "COMMON-LISP")
  (:nicknames "BILL")
  (:export "IN-FILE"
           "LIST-LINES-AS-STRINGS"
           "UNIX-CAT"
           "READ-LINE-EXCLUDING-WORDS-BEGINNING-WITH-CHAR"
           "SIMPLE-READ-LINE"))

(defun list-lines-as-strings (file)
  "Returns a list of strings representing each line in the file."
  (with-open-file (str file :direction :input)
    (let ((accum nil))
      (do ((line (read-line str nil 'eof) (read-line str nil 'eof)))
          ((equal line 'eof))
        (setf accum (cons line accum)))
      (reverse accum))))

(defun unix-cat (file)
  "Like the Unix cat utility."
  (with-open-file (str file :direction :input)
    (do ((line (read-line str nil 'eof) 
               (read-line str nil 'eof)))
        ((eql line 'eof))
      (format t "~A~%" line))))

(defun read-line-excluding-words-beginning-with-char (c)
  "Enter lines of text, stop collecting by typing 'end' on it's own line.
   If a line starts with the passed-in character, ignore the line."
	   (let ((list nil))
	     (do ((line (read-line) (read-line)))
		 ((equal line "end"))
	       (if (char= (elt line 0) c)
		   nil
		   (push line list)))
	     (reverse list)))

(defun simple-read-line ()
  "Enter lines of text, stop collecting by typing 'end' on it's own line."
	   (let ((list nil))
	     (do ((word (read-line) (read-line)))
		 ((equal word "end"))
	       (push word list))
	     (reverse list)))