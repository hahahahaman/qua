;;;; qua.asd

(asdf:defsystem #:qua
  :description "Describe qua here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :components ((:file "package")
               (:file "qua"))
  :depends-on (:iterate))

