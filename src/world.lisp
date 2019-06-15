(in-package :qua)

(defclass world ()
  ((entity-components
    :initform (make-hash-table)
    :type hash-table)
   (entity-ids
    :initform (make-array 256 :fill-pointer 0 :adjustable t
                              :element-type 'bit
                              :initial-element 0)
    :type array
    :documentation "Implemented this way so that I can loop through
    all entities.")
   (systems
    :initform (make-hash-table)
    :type hash-table
    :accessor systems))
  (:documentation "Handles the entities and the systems."))

(defun make-world ()
  "Returns a new world instance."
  (make-instance 'world))

(defmethod make-entity ((world world))
  (with-slots (entity-ids entity-components) world
    (iter (for i from 0 below (length entity-ids))
      ;; empty e found - set filled
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
    ;; free entity e
    (setf (aref entity-ids entity-id) 0)

    ;; remove entity from all systems
    (iter (for (st s) in-hashtable systems)
      (remhash entity-id (entities s)))))

(defun remove-entities (world &rest entities)
  (iter (for e in entities)
    (remove-entity world e)))

(defmethod add-component ((world world) entity-id component)
  (with-slots (entity-components entity-ids systems) world
    (let ((type (type-of component))
          (ec (components world entity-id)))
      ;; check if component of same type is already there
      (when (in-hash-table-p type ec)
        (warn "Entity ~a already has component of type ~a, replacing." entity-id type))
      (setf (gethash type ec) component)

      ;; add entity to any systems
      ;; system-type, system
      (iter (for (st s) in-hashtable systems)
        (system-add-entity s entity-id ec)))))

(defun add-components (world entity-id &rest components)
  (iter (for c in components)
    (add-component world entity-id c)))

(defmethod remove-component ((world world) entity-id component)
  (let ((type (type-of component))
        (components (components world entity-id)))
    ;; remove the component from components hash-table
    (remhash type components)

    ;;remove entity from systems for entity does not have the
    ;;relevant components
    (with-slots (systems) world
      ;; system-type, system
      (iter (for (st s) in-hashtable systems)
        (unless (components-in-system-p components s)
          (system-remove-entity s entity-id))))))

(defun remove-components (world entity-id &rest components)
  (iter (for c in components)
    (remove-component world entity-id c)))

(defmethod update-world ((world world) dt)
  (with-slots (systems) world
    (iter (for (st s) in-hashtable systems)
      (update-system world s dt))))

(defmethod add-system ((world world) system)
  (with-slots (systems entity-ids) world
    (setf (gethash (type-of system) systems) system)
    (iter (for e from 0 below (length entity-ids))
      (when (= (aref entity-ids e) 1)
        (system-add-entity system e (components world e))))))

(defun add-systems (world &rest systems)
  (iter (for s in systems)
    (add-system world s)))

(defmethod remove-system ((world world) system)
  (with-slots (systems) world
    (remhash (type-of system) systems)))

(defun remove-systems (world &rest systems)
  (iter (for s in systems)
    (remove-system world s)))

(defmethod get-system ((world world) (system-type symbol))
  (gethash system-type (systems world)))

(defmethod update-system ((world world) (system system) dt)
  (format t "~s updated.~%" (type-of system)))

;; (defun add-entities-to-systems (world)
;;   "Goes through all the entities placing them into the correct systems
;; based on the depedencies of the system and the current components."
;;   (with-slots (systems entity-components) world
;;     ;;clear systems of entities
;;     (iter (for (st s) in-hashtable systems)
;;       (iter (for key in (alexandria:hash-table-keys (entities s)))
;;         (remhash key (entities s))))

;;     ;; entity-id, component
;;     (iter (for (e ec) in-hashtable entity-components)
;;       ;; don't check systems if no components
;;       (when (not (null ec))
;;         ;; system-type, system
;;         (iter (for (st s) in-hashtable systems)
;;           ;; when all depencenies of system are satisfied
;;           (when (components-in-system-p ec s)
;;             ;; place entity into the system
;;             (setf (gethash e (entities s)) 1)))))))

(defmethod clear-entities ((world world))
  (with-slots (entity-ids) world
    (iter (for e in-vector entity-ids)
      (when (= e 1)
        (remove-entity world e)))))

(defmethod clear-world ((world world))
  (with-slots (entity-components entity-ids systems) world
    (setf entity-components (make-hash-table)
          systems (make-hash-table))
    (iter (for i in-vector entity-ids)
      (setf i 0))))

(defmacro system-do-with-components (component-types world system
                                     id-var-name
                                     &body body)
  "COMPONENT-TYPE takes 2 value list, (var-name type) kind of like WITH-ACCESSORS.
This loops through all entities in SYSTEM exposing the components
specified in COMPONENT-TYPES. This macro exposes the current
ENTITY-ID, as ID-VAR-NAME, which can be useful."

  ;; the entities slot of system is a hashtable with a key of the
  ;; entity-id and a value of 1, so N is a throw away variable.
  `(iter (for (entity-id n) in-hashtable (entities ,system)) ;; loop through entities
     ;;collect components specified in component-types
     (let (,@(iter (for c in component-types)
               (collect `(,(if (symbolp c) c (car c))
                          (gethash ',(if (symbolp c) c (cadr c))
                                   (components ,world entity-id)))))
           (,(alexandria:symbolicate id-var-name) entity-id))
       ,@body)))
