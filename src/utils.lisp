(in-package :qua)

(defun print-table (table)
  (format t "#<HASH-TABLE~%")
  (iter (for (k v) in-hashtable table) (format t "~s: ~s~%" k v))
  (format t ">~%"))

(defun in-hash-table-p (key hash-table)
  (nth-value 1 (gethash key hash-table)))

(defparameter *world* nil)

(defun switch-to-world (world)
  (setf *world* world))

(defun current-world ()
  *world*)

(defun components (world entity-id)
  "Returns the hash-table of components, from the ENTITY-COMPONENTS slot of WORLD."
  (with-slots (entity-components) world
    (gethash entity-id entity-components)))

(defun (setf components) (value world entity-id)
  (with-slots (entity-components) world
    (setf (gethash entity-id entity-components) value)))

(defun entity-component (world entity-id component-type)
  "Returns the component of type COMPONENT-TYPE of ENTITY-ID."
  (gethash component-type (components world entity-id)))

(defun (setf entity-component) (value world entity-id component-type)
  (setf (gethash component-type (components world entity-id)) value))

(defun components-in-system-p (components system)
  "Compare the hash-table of components with the list of dependencies of the system. "
  (with-slots (dependencies) system
    ;; component type
    (iter (for ct in dependencies)
      (unless (in-hash-table-p ct components)
        (leave nil))
      (finally (return t)))))
