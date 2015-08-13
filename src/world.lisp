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

(defun make-world ()
  "Returns a new world instance, also sets *WORLD* to this new world."
  (let ((w (make-instance 'world)))
    (setf *world* w)
    w))

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

(defun remove-entities (world &rest entities)
  (iter (for e in entities)
    (remove-entity world e)))

(defmethod add-component ((world world) entity-id component)
  (with-slots (entity-components entity-ids) world
    (let ((type (type-of component)))
      ;; check if component of same type is already there
      (when (in-hash-table-p type (components world entity-id))
        (warn "Entity ~a already has component of type ~a, replacing." entity-id type))
      (setf (gethash type (components world entity-id)) component))))

(defun add-components (world entity-id &rest components)
  (iter (for c in components)
    (add-component world entity-id c)))

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

(defun remove-components (world entity-id &rest components)
  (iter (for c in components)
    (remove-component world entity-id c)))

(defmethod update-world ((world world) dt)
  (with-slots (systems) world
    (iter (for (st s) in-hashtable systems)
      (update-system world s dt))))

(defun system-add-entity (world system entity-id)
  "Add entity directly into system."
  (when (components-in-system-p (components world entity-id) system)
    (setf (gethash entity-id (entities system)) 1)))

(defun system-add-entities (world system &rest entities)
  (iter (for e in entities)
    (system-add-entity world system e)))

(defmethod add-system ((world world) system)
  (with-slots (systems) world
    (setf (gethash (type-of system) systems) system)))

(defun add-systems (world &rest systems)
  (iter (for s in systems)
    (add-system world s)))

(defmethod remove-system ((world world) system)
  (with-slots (systems) world
    (remhash (type-of system) systems)))

(defun remove-systesm (world &rest systems)
  (iter (for s in systems)
    (remove-system world s)))

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

(defmacro with-components (component-types world system &body body)
  " COMPONENT-TYPE takes 2 value list, (var-name type) kind of like WITH-ACCESSORS.
This loops through all entities in SYSTEM exposing the components specified in COMPONENT-TYPES.
This macro exposes the current ENTITY-ID, which can be useful."

  ;; the entities slot of system is a hashtable with a key of the
  ;; entity-id and a value of 1, so N is a throw away variable.
  `(iter (for (entity-id n) in-hashtable (entities system)) ;; loop through entities
     ;;collect components specified in component-types
     (let (,@(iter (for c in component-types)
               (collect `(,(if (symbolp c) c (car c))
                          (gethash ',(if (symbolp c) c (cadr c))
                                   (components ,world entity-id))))))
       ,@body)))
