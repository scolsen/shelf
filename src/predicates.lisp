(defpackage shelf.predicates
  (:use cl)
  (:export :option?))

(in-package shelf.predicates)

(defun option? (x) 
  "Is a string an option?"
  (if (eql (char x 0) #\-) 
      t 
      nil))
