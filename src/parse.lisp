(in-package :c2ffi-cffi)

(declaim (inline akey aval))
(defun akey (val alist) (car (rassoc val alist)))
(defun aval (key alist) (cdr (assoc key alist)))

(defun anonymous-p (form)
  (and (consp form)
       (or (string= "" (aval :name form))
           (and (string= ":array" (aval :tag form))
                (string= "" (aval :name (aval :type form)))))))

(defun make-anonymous-name ()
  (gensym "ANON-TYPE-"))

(defvar *anonymous-name* nil)
(defvar *output-package* nil)

(defvar *export-symbols* nil)
(defvar *array-size-p* nil)

(optima:defpattern tag (name &rest params)
  `(and (assoc :tag ,name)
        ,@(loop for i in params
                collect `(assoc ,(alexandria:make-keyword i) ,i))))

(defun default-parse-symbol (string type &optional (package *package*))
  (let ((string (if (eq #\_ (aref string 0))
                    (string-upcase string)
                    (nstring-upcase (substitute #\- #\_ string)))))
    (if (eq #\: (aref string 0))
        (alexandria:make-keyword (subseq string 1))
        (cond
          ((eq type :cconst)
           (intern (format nil "+~A+" string) package))
          ((eq type :cenumfield)
           (alexandria:make-keyword string))
          (t (intern string package))))))

(defmacro with-array-sizes (&body body)
  `(let ((*array-size-p* t))
     ,@body))

(defvar *parse-symbol-fun* 'default-parse-symbol)

(defun parse-symbol (string type &optional (package *package*))
  (if (string= "" string)
      *anonymous-name*
      (let ((symbol (funcall *parse-symbol-fun* string type package)))
        (unless (or (keywordp symbol)
                    (not (symbol-package symbol))
                    (member type '(:cfield :cparam)))
          (pushnew symbol *export-symbols*))
        symbol)))

(defun parse-type (form)
  (optima:match form
    ((tag ":signed-char") '(:char))
    ((tag ":pointer" type)
     (let ((pointee (parse-type type)))
       (if (or (equal '(:unsigned-char) pointee)
               (equal '(:char) pointee))
           (list :string)
           (list :pointer))))
    ((tag ":function-pointer")
     '(:pointer))
    ((tag ":array" type size)
     (if *array-size-p*
         `(,@(parse-type type) :count ,size)
         (list :pointer)))
    ((or (tag ":struct" name)
         (tag "struct" name))
     `(,(parse-symbol name :cstruct)))
    ((or (tag ":union" name)
         (tag "union" name))
     `(,(parse-symbol name :cunion)))
    ((or (tag ":enum" name)
         (tag "enum" name))
     `(,(parse-symbol name :cenum)))
    ((tag x) `(,(parse-symbol x :ctype)))))

(defun parse-fields (parameters type &optional (vals-are-types-p t))
  (let (fields toplevels)
    (loop for p in parameters
          as name = (aval :name p)
          as val = (if vals-are-types-p
                       (aval :type p)
                       (aval :value p))
          do (let* ((*anonymous-name* (when (anonymous-p val)
                                        (make-anonymous-name)))
                    (toplevel (if (and vals-are-types-p
                                       (string= ":array" (aval :tag val)))
                                  (parse-toplevel (aval :type val))
                                  (parse-toplevel val))))
               (when toplevel
                 (push toplevel toplevels))
               (push `(,(or (parse-symbol name type) (gensym "PARAM-"))
                       ,@(if vals-are-types-p
                             (parse-type val)
                             (list val)))
                     fields))
          finally (return (values (nreverse fields) toplevels)))))

(defun parse-toplevel (form)
  (when (consp form)
    (let* ((name (aval :name form)))
      (optima:match form
        ((tag "typedef" type)
         (if (anonymous-p type)
             (let ((*anonymous-name* (make-anonymous-name)))
               `(progn
                  ,(parse-toplevel type)
                  (cffi:defctype ,(parse-symbol name :ctype) ,*anonymous-name*)))
             `(cffi:defctype ,(parse-symbol name :ctype) ,@(parse-type type))))
        ((tag "const" value)
         (if (numberp value)
             `(defconstant ,(parse-symbol name :cconst) ,value)
             `(defvar ,(parse-symbol name :cconst) ,value)))
        ((tag "extern" type)
         `(cffi:defcvar (,(parse-symbol name :cvar) ,name)
              ,@(parse-type type)))
        ((tag "function" return-type parameters)
         `(cffi:defcfun (,(parse-symbol name :cfun) ,name)
              ,@(parse-type return-type)
            ,@(parse-fields parameters :cparam)))
        ((tag "struct" fields)
         (with-array-sizes
           (multiple-value-bind (fields toplevels)
               (parse-fields fields :cfield)
             `(progn
                ,@toplevels
                (cffi:defcstruct ,(parse-symbol name :cstruct)
                  ,@fields)))))
        ((tag "union" fields)
         (with-array-sizes
           (multiple-value-bind (fields toplevels)
               (parse-fields fields :cfield)
             `(progn
                ,@toplevels
                (cffi:defcunion ,(parse-symbol name :cstruct)
                  ,@fields)))))
        ((tag "enum" fields)
         `(cffi:defcenum ,(parse-symbol name :cenum)
            ,@(parse-fields fields :cenumfield nil)))))))

(defun parse-file-to-file (input-file output-file &optional (append-p nil))
  (with-open-file (out output-file
                       :direction :output
                       :if-exists (if append-p :append :supersede))
    (parse-file input-file out)))

(defun parse-file (input-file &optional (output *standard-output*))
  (with-open-file (in input-file)
    (let ((json (json:decode-json in)))
      (parse json output))))

(defun write-nicely (stream object)
  (write object
         :stream stream
         :case :downcase
         :circle t
         :pretty t
         :readably t)
  (format stream "~%~%"))

(defun parse (json &optional (out *standard-output*))
  (format out "~&")
  (let ((*package* (find-package (or *output-package* *package*)))
        (*export-symbols* nil))
    (when *output-package*
      (write-nicely out `(in-package ,*output-package*)))
    (loop for form in json do
      (let ((loc (aval :location form)))
        (when loc (format out "~&;; ~A~%" loc)))
      (write-nicely out (parse-toplevel form)))
    (write-nicely out `(export ',*export-symbols*))))
