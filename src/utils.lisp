(in-package :qua)

(defun print-table (table)
  (format t "#<HASH-TABLE~%")
  (iter (for (k v) in-hashtable table) (format t "~s: ~s~%" k v))
  (format t ">~%"))

(defun in-hash-table-p (key hash-table)
  ;; gethash returns 2 values, the value,
  ;; and a boolean saying whether the key is in the hash table or not
  ;; (it's zero based indexing, so nth-value 1)
  (nth-value 1 (gethash key hash-table)))

;; (defparameter *world* nil)

;; (defun switch-to-world (world)
;;   (setf *world* world))

;; (defun current-world ()
;;   *world*)

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

(defun get-component (world entity-id type)
  (entity-component world entity-id type))

(defun (setf get-components) (value world entity-id type)
  (setf (entity-component world entity-id type) value))

(defun ec (w id type)
  (entity-component w id type))

(defun (setf ec) (value w id type)
  (setf (entity-component w id type) value))

(defun has-component-p (world entity-id component-type)
  (nth-value 1 (entity-component world entity-id component-type)))

(defun components-in-system-p (components system)
  "Returns T if COMPONENTS contains all the dependencies of SYSTEM, else NIL."
  (with-slots (dependencies) system
    ;; component type
    (iter (for ct in dependencies)
      (unless (in-hash-table-p ct components)
        (leave nil))
      (finally (return t)))))
