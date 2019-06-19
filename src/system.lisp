(in-package #:qua)

(defclass system ()
  ((dependencies
    :initarg :dependencies
    :type cons
    :accessor dependencies)
   (entities
    :initarg :entities
    :type hash-table
    :accessor system-entities))
  (:default-initargs
   :dependencies nil
   :entities (make-hash-table)))

(defmacro defsystem (name (&rest dependencies))
  `(defclass ,name (system) ()
     (:default-initargs
      :dependencies ',dependencies)))

(defmethod clear-system ((system system))
  (setf (system-entities system) (make-hash-table)))

(defun system-add-entity (system entity-id components)
  "If entity has correct components add into system."
  (when (components-in-system-p components system)
    (setf (gethash entity-id (system-entities system)) 1)
    ;; (warn "Entity ~a doesn't satisfy dependencies of system ~a!~%" entity-id system))
    ))

(defun system-add-entities (system &rest entities)
  "Adds (entity-id components) into SYSTEM."
  (iter (for (e ec) in entities)
    (system-add-entity system e ec)))

(defun system-remove-entity (system entity-id)
  (remhash entity-id (system-entities system)))

(defun system-remove-entities (system &rest entities)
  (iter (for e in entities)
    (system-remove-entity system e)))
