(defpackage :c2ffi-cffi
  (:use #:cl)
  (:export #:parse #:parse-file #:parse-file-to-file
           #:spec

           #:*parse-symbol-fun* #:*anonymous-name* #:*output-package*
           #:*exclude-sources* #:*exclude-definitions*))
