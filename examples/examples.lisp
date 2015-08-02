;;;; examples.lisp

(in-package :qua-examples)

;; (defcomponent pos (x y z))
(defstruct pos x y z)
(defcomponent velocity (x y z))

(defsystem pos-system (pos))

(defmethod update-system ((world qua:world) (system pos-system) dt)
  (flet ((cfloat (n) (coerce n 'single-float)))
    (with-components (pos) world system
      (format t "~5$, ~5$, ~5$~%"
              (cfloat (pos-x pos))
              (cfloat (pos-y pos))
              (cfloat (pos-z pos))))))

(defsystem velocity-system (pos velocity))

(defmethod update-system ((world qua:world) (system velocity-system) dt)
  (with-components (pos velocity) world system
    (incf (pos-x pos) (* (x velocity) dt))
    (incf (pos-y pos) (* (y velocity) dt))
    (incf (pos-z pos) (* (z velocity) dt))))

(defun example ()
  (let* ((n 10)
         (w (make-instance 'qua:world))
         (e (make-array n
                        :initial-contents
                        (iter (for i from 0 below n) (collect (make-entity w)))))
         (pos (make-array n
                          :initial-contents
                          (iter (for i from 0 below n)
                            (collect (make-pos :x 1.0 :y 1.0 :z 1.0)))))
         (vel (make-array n
                          :initial-contents
                          (iter (for i from 0 below n)
                            (collect (make-instance 'velocity :x 1.0 :y 1.0 :z 1.0)))))
         (pos-sys (make-instance 'pos-system))
         (vel-sys (make-instance 'velocity-system)))
    (iter (for i from 0 below n)
      (add-component w (aref e i) (aref pos i))
      (add-component w (aref e i) (aref vel i)))
    ;; (print-table (components w e))
    (iter (for i from 1 below n)
      (remove-entity w (aref e i)))
    (add-system w pos-sys)
    (add-system w vel-sys)
    (initialize-systems w)
    (iter (for i from 0 to 10) (update w 0.1))))
