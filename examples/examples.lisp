(in-package :qua-examples)

(defcomponent pos (x y z))
(defcomponent velocity (x y z))

(defsystem pos-system (pos))

(defmethod update-system ((world qua:world) (system pos-system) dt)
  (with-components (pos) world system
    (format t "~a, ~a, ~a~%" (x pos) (y pos) (z pos))))

(defsystem velocity-system (pos velocity))

(defmethod update-system ((world qua:world) (system velocity-system) dt)
  (with-components (pos velocity) world system
    (incf (x pos) (* (x velocity) dt))
    (incf (y pos) (* (y velocity) dt))
    (incf (z pos) (* (z velocity) dt))))

(defun example ()
  (let* ((w (make-instance 'qua:world))
         (e (make-entity w))
         (pos (make-instance 'pos :x 1.0 :y 1.0 :z 1.0))
         (vel (make-instance 'velocity :x 1.0 :y 0.0 :z 0.0))
         (pos-sys (make-instance 'pos-system))
         (vel-sys (make-instance 'velocity-system)))
    (add-component w e pos)
    (add-component w e vel)
    (print-table (components w e))
    (add-system w pos-sys)
    (add-system w vel-sys)
    (add-component w e vel)
    (initialize-systems w)
    (iter (for i from 1 to 10) (update w 0.5))))
