;;;; File: shakespeare.lisp

(defpackage "SHAKESPEARE"
  (:use "COMMON-LISP" "HENLEY")
  (:export "IN-FILE"
           "WILL-MEETS-WALT"
           "PRINT-POEM"
           "PRINT-STANZA"
           "WORD-PUNC"
           "PRINT-TXT-LST"
           "GENERATE-WORD-LST"))

;; Stanza types:
;; couplet  (2)
;; tercet   (3)
;; quatrain (4)
;; cinquain (5)
;; sestet   (6)
;; septet   (7)
;; octave   (8)

(defun print-txt-lst (lst)
  (dolist (obj lst)
    (format t "~A " obj)))

(defun generate-word-lst (n lst &optional (prev '|.|))
  (if (zerop n)
    lst
    (let ((next (random-next prev))) ;; from henley
      (setf lst (append lst (list next))) ;; add to the list
      (generate-word-lst (1- n) lst next)))) ;; repeat n more times, next = prev

(defun word-punc (word)
  (case word
    (|,| t)
    (|.| t)
    (|;| t)
    (|!| t)
    (|'| t)
    (|?| t)
    (|`| t)))

(defun print-stanza (bank lns-per)
 (if (zerop lns-per)
     nil ;; we are done
     (progn
        (dotimes (i (+ (random 6) 2)) ;; do no more than 8 times
          (let ((word (pop bank))) ;; pop a word from the bank
            (if (word-punc word) 
              nil ;; if the word is punc, do nothing
              (format t "~A " word)))) ;; otherwise, print the word
        (terpri)
        (print-stanza bank (1- lns-per))))) ;; repeat with fewer lines

(defun print-poem (banklist stzs-rem lns-per)
  (if (zerop stzs-rem)
    nil ;; if we're out of stanzas, stop
    (progn
      (print-stanza (car banklist) lns-per) ;; give the first banklist to stnza
      (terpri)
      (print-poem (cdr banklist) (1- stzs-rem) lns-per)))) ;; repeat with rest

(defun will-meets-walt (stanzas lines-per)
  (let ((wordbank nil))
    (dotimes (i stanzas)
      (setf wordbank (cons (generate-word-lst
                              (hash-table-count *words*) ()) ;; # words in list
                              wordbank))) ;; generate word lists for stanzas
    (print-poem wordbank stanzas lines-per)))

;;; end  
