(in-package :c2ffi-cffi)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass spec (asdf:cl-source-file)
    ((package :initform :common-lisp-user :accessor spec-package :initarg :package)
     (file :initform nil :accessor spec-file :initarg :from)
     (exclude-sources :initform nil :accessor exclude-sources :initarg :exclude-sources)
     (exclude-definitions :initform nil :accessor exclude-definitions :initarg :exclude-definitions))
    (:documentation "Generate and load CFFI definitions from a c2ffi JSON spec."))

  (defmethod asdf:perform :before ((op asdf:compile-op) (c spec))
    (let ((in-filename (if (spec-file c)
                           (merge-pathnames (spec-file c)
                                            (asdf:component-pathname c))
                           (merge-pathnames (make-pathname
                                             :name (pathname-name (asdf:component-pathname c))
                                             :type "spec")
                                            (asdf:component-pathname c))))
          (out-filename (asdf:component-pathname c)))
      (let ((*output-package* (spec-package c))
            (*exclude-sources* (exclude-sources c))
            (*exclude-definitions* (exclude-definitions c)))
        (c2ffi-cffi:parse-file-to-file in-filename out-filename)))))
