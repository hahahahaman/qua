;;;; qua.lisp

(in-package #:qua)

;; exports all symbols in package
;; seems reckless, but convenient
(let ((pack (find-package :qua)))
  (do-all-symbols (sym pack)
    (when (eql (symbol-package sym) pack)
      (export sym))))




