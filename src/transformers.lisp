(defpackage shelf.transformers 
  (:use cl)
  (:import-from :shelf.predicates
                :option?)
  (:export :option-to-keyword
           :drop-option-dash
           :arguments-to-hash))

(in-package shelf.transformers)

(defun drop-option-dash (option) 
  "Drop dashes from option strings."
  (cond ((shelf.predicates:option? option) (drop-option-dash (subseq option 1)))
        (t option)))

(defun option-to-keyword (option) 
  "Convert an argument string to a keyword."
  (if (shelf.predicates:option? option) 
      (intern (string-upcase (drop-option-dash option)) "KEYWORD")
      option))

(defun arguments-to-hash (arguments) 
  "Convert an argument list to a hash table."
  (labels ((r (a current prev result) 
             (cond ((eql nil a) result)
                   ((shelf.predicates:option? current) 
                    (setf (gethash (option-to-keyword current) result) '())
                    (r (cdr a) (cadr a) current 
                       result))
                   (t (setf (gethash (option-to-keyword prev) result) (cons current (gethash (option-to-keyword prev) result)))
                   (r (cdr a) (cadr a) prev 
                         result))))) 
          (r arguments (car arguments) (car arguments) (make-hash-table))))

