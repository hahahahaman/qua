(in-package :qua-examples)

(defcomponent pos (x y z))
(defcomponent velocity (x y z))

(defsystem pos-system (pos))

#|
(defmethod update-system ((world qua:world) (system pos-system) dt)
  (iter (for (entity-id n) in-hashtable (entities system))
    (let ((pos (gethash 'pos (components world entity-id))))
      (format t "x: ~a, y: ~a, z: ~a ~%" (x pos) (y pos) (z pos))
      ;; (format t "~a ~%" pos)
      )))
|#
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
    (remove-component w e vel)
    (add-component w e vel)
    (initialize-systems w)
    (iter (for i from 1 to 10) (update w 0.5))
  ))
;; (defun test-interface ()
;;   (defcomponent point ()
;;     (x y z))

;;   (defcomponent velocity (point)
;;     (vx vy vz))

;;   (defsystem point (entity point)
;;     (format t "entity ~a at pos (~a, ~a, ~a)~%" entity (x point) (y point) (z point)))

;;   (defsystem velocity (entity velocity point)
;;     (incf (x point) (vx velocity))
;;     (incf (y point) (vy velocity))
;;     (incf (z point) (vz velocity)))

;;   (make-entity nil '(point) :x 1 :y 2 :z 3)
;;   (make-entity nil '(point velocity) :x 4 :y 5 :z 6 :vx -1 :vy -2 :vz -3)

;;   (loop repeat 10 do (system-loop)))
