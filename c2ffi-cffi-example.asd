(defpackage :c2ffi-cffi-example.asdf
  (:use #:cl #:asdf))

(in-package :c2ffi-cffi-example.asdf)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system :c2ffi-cffi))

(defsystem :c2ffi-cffi-example
  :description "Example for using c2ffi-cffi"
  :author "Ryan Pavlik"
  :license "LLGPL"
  :version "0.0"

  :depends-on (:c2ffi-cffi)
  :pathname "example"
  :serial t

  :components
  ((:file "package")
   (c2ffi-cffi:spec "example")
   (c2ffi-cffi:spec "macros" :package :c2ffi-another-package)))
