(cl-annot:enable-annot-syntax)

(in-package #:steampunk.utilities)

@export
(defun filter (predicate list &key key)
  "Iterates over a LIST calling PREDICATE on each element and collecting a list of either elements
or results of calling KEY on elements."
  (loop for elt in list
        when (funcall predicate elt)
          collect (if (null key)
                      elt
                      (funcall key elt))))
