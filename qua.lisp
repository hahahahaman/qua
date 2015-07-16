;;;; qua.lisp

(in-package #:qua)

;;; "qua" goes here. Hacks and glory await!

(defun print-table (table)
  (format t "#<HASH-TABLE~%")
  (iter (for (k v) in-hashtable table) (format t "~s: ~s~%" k v))
  (format t ">~%"))

(defparameter *world* nil)

(defun build-var (var)
  (list var
        :initform nil
        :accessor (intern (string var))
        :initarg (intern (string var) :keyword)))

(defun build-varlist (varlist)
  (iter (for var in varlist)
    (collect (build-var var))))

(defmacro defcomponent (name (&rest slots))
  "Makes a basic class. The accessors are declared for the slots, with the same name."
  `(defclass ,name ()
     (,@(build-varlist slots))))

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

(defgeneric make-entity (world)
  (:documentation "Returns the unique id of the new entity."))
(defgeneric remove-entity (world entity-id)
  (:documentation "Removes the entity's components and sets the id up for reuse."))

(defgeneric add-component (world entity-id component))
(defgeneric remove-component (world entity-id component-type))

(defgeneric add-system (world system))
(defgeneric remove-system (world system-type))

(defgeneric update (world dt)
  (:documentation "Update all the systems in the world."))

(defun in-hash-table-p (key hash-table)
  (nth-value 1 (gethash key hash-table)))

(defun switch-to-world (world)
  (setf *world* world))

(defun current-world ()
  *world*)

(defun components (world entity-id)
  "Internal short-hand"
  (with-slots (entity-components) world
    (gethash entity-id entity-components)))
(defun (setf components) (value world entity-id)
  (with-slots (entity-components) world
    (setf (gethash entity-id entity-components) value)))

(defmethod make-entity ((world world))
  (with-slots (entity-ids entity-components) world
    (iter (for i from 0 below (length entity-ids))
      ;; empty id found - set filled
      (when (= (aref entity-ids i) 0)
        (setf (aref entity-ids i) 1
              (components world i)(make-hash-table))
        (leave i))
      ;; if nothing found - extend array
      (finally (vector-push-extend 1 entity-ids)
               (setf (components world i) (make-hash-table))
               (return i)))))

(defmethod remove-entity ((world world) entity-id)
  (with-slots (entity-components entity-ids systems) world
    (unless (remhash entity-id entity-components)
      (warn "Entity ~a not found. Nothing removed." entity-id)
      (return-from remove-entity nil))
    ;; free entity id
    (setf (aref entity-ids entity-id) 0)

    ;; remove entity from all systems
    (iter (for (st s) in-hashtable systems)
      (remhash entity-id (entities s)))
    t))

(defmethod add-component ((world world) entity-id component)
  (with-slots (entity-components entity-ids) world
    ;; (unless (= (aref entity-ids entity-id) 1)
    ;;   (warn "Entity ~a not found. Component not added." entity-id)
    ;;   (return-from add-component nil))
    (unless (components world entity-id)
      (setf (components world entity-id) (make-hash-table)))
    (let ((type (type-of component)))
      ;; check if component of same type is already there
      (when (in-hash-table-p type (components world entity-id))
        (warn "Entity ~a already has component of type ~a, replacing." entity-id type))
      (setf (gethash type (components world entity-id)) component))))

(defmethod remove-component ((world world) entity-id component)
  (let ((type (type-of component)))
    (remhash type (components world entity-id))

    ;;remove entity from systems which depend on the removed component
    (with-slots (systems) world
      (iter (for (st s) in-hashtable systems)
        (iter (for d in (dependencies s))
          (when (eql d type)
            (leave (remhash entity-id (entities s)))))))))

(defun entity-component (world entity-id component-type)
  (gethash component-type (components world entity-id)))

(defun (setf entity-component) (value world entity-id component-type)
  (setf (gethash component-type (components world entity-id)) value))

(defmethod update ((world world) dt)
  (with-slots (systems) world
    (iter (for (st s) in-hashtable systems)
      (update-system world s dt))))

(defclass system ()
  ((dependencies
    :type cons
    :accessor dependencies)
   (entities
    :initform (make-hash-table)
    :type hash-table
    :accessor entities)))

(defgeneric update-system (world system dt))

(defmacro defsystem (name (&rest dependencies))
  `(defclass ,name (system)
     ((dependencies
       :initform ',dependencies
       :type cons
       :accessor dependencies))))

(defun in-system-p (components system)
  "Compare the hash-table of components with the list of dependencies of the system. "
  (with-slots (dependencies) system
    (iter (for ct in dependencies)
      (unless (in-hash-table-p ct components)
        (leave nil))
      (finally (return t)))))

(defun build-let-var (var components)
  (list var (gethash var components)))

(defmacro with-components ((&rest component-types) components &body body)
  "Takes a component type list and"
  `(let (,@(iter (for c in component-types)
             (collect (build-let-var c components))))
     ,@body))

(defun setup-entity-systems (world)
  (with-slots (systems entity-components) world
    (iter (for (e ec) in-hashtable entity-components)
      (iter (for (st s) in-hashtable systems)
        (when (in-system-p ec s)
          (setf (gethash e (entities s)) 1))))))

(defmethod add-system ((world world) system)
  (with-slots (systems) world
    (setf (gethash (type-of system) systems) system)))

(defmethod remove-system ((world world) system)
  (with-slots (systems) world
    (remhash (type-of system) systems)))

(defmethod update-system ((world world) (system system) dt)
  (format t "~s updated.~%" (type-of system)))




