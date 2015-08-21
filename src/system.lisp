(in-package #:qua)

(defclass system ()
  ((dependencies
    :type cons
    :accessor dependencies)
   (entities
    :initform (make-hash-table)
    :type hash-table
    :accessor entities)))

(defmacro defsystem (name (&rest dependencies))
  `(defclass ,name (system)
     ((dependencies
       :initform ',dependencies
       :type cons
       :accessor dependencies))))

(defmethod clear-system ((system system))
  (setf (entities system) (make-hash-table)))
