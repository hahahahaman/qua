;;;; package.lisp

(defpackage #:qua
  (:use #:cl #:iter)
  (:export
   ;; declaration macros
   #:defcomponent
   #:defsystem

   ;; entity related
   #:make-entity
   #:remove-entities

   ;; component stuff
   #:with-components
   #:add-components
   #:remove-components

   ;; entity-component
   #:entity-component
   #:|setf entity-component|

   ;; world
   #:make-world
   #:update-world
   #:current-world

   ;; system
   #:update-system
   #:initialize-systems
   #:add-systems
   #:remove-systems
   #:system-add-entities

   ;; classes
   #:world
   #:system

   ;; #:components
   ;; #:entities

   #:print-table ;; utility for debuging hashtables
   ))

