;;;; qua-examples.asd

(asdf:defsystem #:qua-examples
  :name "Some basic qua usage"
  :serial t
  :components ((:module examples
                :components ((:file "package")
                             (:file "examples"))))
  :depends-on (:iterate :qua))
