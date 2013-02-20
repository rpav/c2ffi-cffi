(defpackage :c2ffi-cffi.asdf
  (:use #:cl #:asdf))

(in-package :c2ffi-cffi.asdf)

(defsystem :c2ffi-cffi
  :description "Bridge for parsing c2ffi JSON into CFFI"
  :author "Ryan Pavlik"
  :license "LLGPL"
  :version "0.0"

  :depends-on (:alexandria :optima :cl-json)
  :pathname "src"
  :serial t

  :components
  ((:file "package")
   (:file "parse")
   (:file "asdf")))
