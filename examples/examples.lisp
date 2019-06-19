;;;; examples.lisp

(in-package :qua-examples)

;; (defcomponent pos (x y z))
(defstruct pos x y z)
(defcomponent velocity (x y z))

(defsystem pos-system (pos))

(defmethod update-system ((world world) (system pos-system) dt)
  (system-do-with-components (pos) world system id
    (format t "id:~D, x:~2$, y:~2$, z:~2$~%"
            id
            (pos-x pos)
            (pos-y pos)
            (pos-z pos))))

(defsystem velocity-system (pos velocity))

(defmethod update-system ((world world) (system velocity-system) dt)
  (system-do-with-components (pos (vel velocity)) world system id
    ;; using defstruct'ed POS and DEFSYSTEM, a class, together
    (incf (pos-x pos) (* (x vel) dt))
    (incf (pos-y pos) (* (y vel) dt))
    (incf (pos-z pos) (* (z vel) dt))))

(defun example ()
  (let* ((n 2)
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
    (add-systems w vel-sys pos-sys)
    (iter (for i from 0 below n)
      (add-components w (aref e i) (aref pos i) (aref vel i)))
    ;; (print-table (components w e))
    ;; (iter (for i from 1 below n)
    ;;   (remove-entities w (aref e i)))
    ;; (add-entities-to-systems w)
    (iter (for i from 0 to 10) (update-world w 0.1))))

(defcomponent position-component (coord))
(defcomponent newtonian-component (velocity mass force))

(defsystem newtonian-system (position-component newtonian-component))

(defmethod update-system ((world world) (system newtonian-system) dt)
  (system-do-with-components ((pos position-component) (newt newtonian-component))
      world system id
    (print id)
    (with-slots ((vel velocity) mass force) newt
      (let ((accel (if (zerop (reduce #'+ force))
                       (vector 0.0 0.0 0.0)
                       (map 'vector #'(lambda (x) (/ x mass)) force))))
        (with-slots ((p coord)) pos
          (iter (for i from 0 below 3)
            ;; semi-implicit euler integration
            (incf (aref vel i) (* (aref accel i) dt))
            (incf (aref p i) (* (aref vel i) dt)))
          (format t "id:~D, x:~2$, y:~2$, z:~2$ | vx:~2$, vy:~2$, vz:~2$~%"
                  id
                  (aref p 0) (aref p 1) (aref p 2)
                  (aref vel 0) (aref vel 1) (aref vel 2)))))))

(defun physics-example ()
  (let* ((n 1)
         (w (make-instance 'world))
         (e (make-array n
                        :initial-contents
                        (iter (for i from 0 below n) (collect (make-entity w)))))
         (pos (make-array n
                          :initial-contents
                          (iter (for i from 0 below n)
                            (collect (make-instance 'position-component
                                                    :coord (vector 0.0 0.0 0.0))))))
         (newt (make-array n
                           :initial-contents
                           (iter (for i from 0 below n)
                             (collect (make-instance 'newtonian-component
                                                     :velocity (vector 0.0 0.0 0.0)
                                                     :mass 1.0
                                                     :force (vector 10.0 0.0 0.0))))))
         (newt-sys (make-instance 'newtonian-system)))
    (add-systems w newt-sys)
    (iter (for i from 0 below n)
      (add-components w (aref e i) (aref pos i) (aref newt i)))
    (iter (for i from 0 to 10) (update-world w 0.1))
    ))
