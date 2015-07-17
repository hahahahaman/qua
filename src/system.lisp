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

(defmacro with-components ((&rest component-types) world system &body body)
  `(iter (for (entity-id n) in-hashtable (entities ,system))
     (let (,@(iter (for c in component-types)
               (collect `(,c (gethash ',c (components ,world entity-id))))))
      ,@body)))

(defmethod add-system ((world world) system)
  (with-slots (systems) world
    (setf (gethash (type-of system) systems) system)))

(defmethod remove-system ((world world) system)
  (with-slots (systems) world
    (remhash (type-of system) systems)))

(defmethod update-system ((world world) (system system) dt)
  (format t "~s updated.~%" (type-of system)))
