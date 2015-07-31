(in-package :qua)

(defclass world ()
  ((entity-components
    :initform (make-hash-table)
    :type hash-table)
   (entity-ids
    :initform (make-array 1 :fill-pointer 0 :adjustable t
                            :element-type 'bit
                            :initial-element 0)
    :type array)
   (systems
    :initform (make-hash-table)
    :type hash-table))
  (:documentation "Handles the entities and the systems."))

(defmethod make-entity ((world world))
  (with-slots (entity-ids entity-components) world
    (iter (for i from 0 below (length entity-ids))
      ;; empty id found - set filled
      (when (= (aref entity-ids i) 0)
        (setf (aref entity-ids i) 1
              ;; initialize the entity's components
              (components world i) (make-hash-table))
        (leave i))
      ;; if nothing found - extend array
      (finally (vector-push-extend 1 entity-ids)
               ;; initialize the entity's components
               (setf (components world i) (make-hash-table))
               (return i)))))

(defmethod remove-entity ((world world) entity-id)
  (with-slots (entity-components entity-ids systems) world
    ;; remove entity from entity-components
    (unless (remhash entity-id entity-components)
      (warn "Entity ~a not found. Nothing removed." entity-id)
      (return-from remove-entity nil))
    ;; free entity id
    (setf (aref entity-ids entity-id) 0)

    ;; remove entity from all systems
    (iter (for (st s) in-hashtable systems)
      (remhash entity-id (entities s)))))

(defmethod add-component ((world world) entity-id component)
  (with-slots (entity-components entity-ids) world
    (let ((type (type-of component)))
      ;; check if component of same type is already there
      (when (in-hash-table-p type (components world entity-id))
        (warn "Entity ~a already has component of type ~a, replacing." entity-id type))
      (setf (gethash type (components world entity-id)) component))))

(defmethod remove-component ((world world) entity-id component)
  (let ((type (type-of component)))
    ;; remove the component from components hash-table
    (remhash type (components world entity-id))

    ;;remove entity from systems which depend on the removed component
    (with-slots (systems) world
      (iter (for (st s) in-hashtable systems)
        (iter (for d in (dependencies s))
          (when (eql d type)
            ;; exit from inner loop
            (leave (remhash entity-id (entities s)))))))))

(defmethod update ((world world) dt)
  (with-slots (systems) world
    (iter (for (st s) in-hashtable systems)
      (update-system world s dt))))

(defun system-add-entity (world system entity-id)
  "Add entity directly into system."
  (when (components-in-system-p (components world entity-id) system)
    (setf (gethash entity-id (entities system)) 1)))

(defmethod add-system ((world world) system)
  (with-slots (systems) world
    (setf (gethash (type-of system) systems) system)))

(defmethod remove-system ((world world) system)
  (with-slots (systems) world
    (remhash (type-of system) systems)))

(defmethod update-system ((world world) (system system) dt)
  (format t "~s updated.~%" (type-of system)))

(defun initialize-systems (world)
  "Goes through all the entities placing them into the correct systems
based on the depedencies of the system and the current components."
  (with-slots (systems entity-components) world
    ;;clear systems of entities
    (iter (for (st s) in-hashtable systems)
      (setf (entities s) (make-hash-table)))

    ;; entity-id, component
    (iter (for (e ec) in-hashtable entity-components)
      ;; if components is nil, that means the entity
      (when (not (null ec))
        ;; system-type, system
        (iter (for (st s) in-hashtable systems)
         (when (components-in-system-p ec s)
           (setf (gethash e (entities s)) 1)))))))

(defmacro with-components ((&rest component-types) world system &body body)
  "Macro that is a short hand for use in update-system. Creates a loop that applies BODY
to all entities in the system and makes all the relevant component slots available as
variables, with the same name as their slot name, in BODY."

  ;; the entities slot of system is a hashtable with a key of the
  ;; entity-id and a value of 1.
  `(iter (for (entity-id n) in-hashtable (entities ,system))
     (let (,@(iter (for c in component-types)
               (collect `(,c (gethash ',c (components ,world entity-id))))))
       ,@body)))
