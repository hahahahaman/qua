;;;; qua.asd

(asdf:defsystem #:qua
  :description "Describe qua here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :components ((:module src
                :components ((:file "package")
                             (:file "utils")
                             (:file "generics")
                             (:file "component")
                             (:file "system")
                             (:file "world")
                             (:file "qua"))))
  :depends-on (:iterate))

