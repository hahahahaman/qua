(in-package #:qua)

(defclass system ()
  ((dependencies
    :initarg :dependencies
    :type cons
    :accessor dependencies)
   (entities
    :initarg :entities
    :type hash-table
    :accessor entities))
  (:default-initargs
   :dependencies nil
   :entities (make-hash-table)))

(defmacro defsystem (name (&rest dependencies))
  `(defclass ,name (system)
     ((dependencies
       :initform ',dependencies
       :type cons
       :accessor dependencies))))

(defmethod clear-system ((system system))
  (setf (entities system) (make-hash-table)))
