;;;; qua.lisp

(in-package #:qua)

;;; "qua" goes here. Hacks and glory await!

(defparameter *world* nil)

(defmacro defcomponent (name (&rest slots))
  "Makes a basic class. The accessors are declared for the slots, with the same name."
  `(defclass ,name ()
     (,@(iter (for s in slots)
          (collect `(,s :accessor ,s))))))

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

(defgeneric update (world))

(defun in-hashtable-p (key hash-table)
  (nth-value 1 (gethash key hash-table)))

(defun switch-to-world (world)
  (setf *world* world))

(defun current-world ()
  *world*)

(defun components (world entity-id)
  "Internal short-hand"
  (with-slots (entity-components) world
    (gethash entity-id entity-components)))

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
    (let ((type (type-of component)))
      ;; check if component of same type is already there
      (when (nth-value 1 (gethash type (components world entity-id)))
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
            (leave (remhash entity-id (entities s))))))))

(defun entity-component (world entity-id component-type)
  (gethash component-type (components world entity-id)))

(defun (setf entity-component) (value world entity-id component-type)
  (setf (get-hash component-type (components world entity-id)) value))

(defclass system ()
  ((dependencies
    :initarg dependencies
    :type cons
    :accessor dependencies)
   (entities
    :initform (make-hash-table)
    :type hash-table
    :accessor entities)))

(defun in-system-p (components system)
  "Compare the hash-table of components with the list of dependencies of the system. "
  (with-slots (dependencies) system
    (iter (for ct in dependencies)
      (unless (nth-value 1 (gethash ct components))
        (leave nil))
      (finally (return t)))))

(defmacro update-system (world system dt &body code)
  `(with-slots (entities dependencies) system
    (iter (for i in-vector entities)
       (let (,@(iter (for d in dependencies)
                 (collect `(,d (gethash ,d ,(components world i))))))
         ,@body))))

(defun setup-entity-systems (world)
  (with-slots (systems entity-components) world
    (iter (for (e ec) in-hashtable entity-components)
      (iter (for (st s) in-hastable systems)
        (when (in-system-p ec s)
          (setf (gethash e (entities s)) 1))))))

;; (defun test-interface ()
;;   (defcomponent point ()
;;     (x y z))

;;   (defcomponent velocity (point)
;;     (vx vy vz))

;;   (defsystem point (entity point)
;;     (format t "entity ~a at position (~a, ~a, ~a)~%" entity (x point) (y point) (z point)))

;;   (defsystem velocity (entity velocity point)
;;     (incf (x point) (vx velocity))
;;     (incf (y point) (vy velocity))
;;     (incf (z point) (vz velocity)))

;;   (make-entity nil '(point) :x 1 :y 2 :z 3)
;;   (make-entity nil '(point velocity) :x 4 :y 5 :z 6 :vx -1 :vy -2 :vz -3)

;;   (loop repeat 10 do (system-loop)))
