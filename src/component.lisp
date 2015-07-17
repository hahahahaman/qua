(in-package #:qua)

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
