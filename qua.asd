;;;; qua.asd

(asdf:defsystem #:qua
  :description "Entity Component system"
  :author "hahahahaman <hahahadude@gmail.com>"
  :license "licenceless Rider"
  :serial t
  :components ((:module src
                :components ((:file "package")
                             (:file "generics")
                             (:file "utils")
                             (:file "component")
                             (:file "system")
                             (:file "world")
                             (:file "qua"))))
  :depends-on (:iterate))

