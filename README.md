# DEPRECATED

This project is deprecated in favor of
[cl-autowrap](https://github.com/rpav/cl-autowrap) which does a whole
lot more with a lot less work.

## c2ffi-cffi

This is a bridge between [c2ffi](https://github.com/rpav/c2ffi) and
[CFFI](http://common-lisp.net/project/cffi/).  It reads the JSON
output produced by `c2ffi` and produces very readable input for CFFI.
Example:

```c
#define FOO (1 << 2)

#define QUUX "abc"

const int BAR = FOO + 10;

extern SomeExtern;

void blah(char *x[]);

extern char *foo;

typedef struct my_point {
  int x;
  int y;
  int odd_value[BAR + 1];
} my_point_t;

typedef struct {
  int a, b;
} anonymous_t;

union my_union {
  char c;
  int i;
  double d;
};

enum some_values {
  a_value,
  another_value,
  yet_another_value
};

void do_something(my_point_t *p, int x, int y);
```

This produces the following two files:

```lisp
;;;; example.lisp:

(in-package :common-lisp-user)

;; example.h:5:11
(defconstant +bar+ 14)

;; example.h:7:12
(cffi:defcvar (someextern "SomeExtern") :int)

;; example.h:9:6
(cffi:defcfun (blah "blah")
    :void
  (x (:pointer :string)))

;; example.h:11:14
(cffi:defcvar (foo "foo") :string)

;; example.h:13:16
(cffi:defcstruct my-point
  (x :int)
  (y :int)
  (odd-value :int :count 15))

;; example.h:17:3
(cffi:defctype my-point-t my-point)

;; example.h:21:3
(progn
 (eval-when (:compile-toplevel :load-toplevel :execute)
   (cffi:defcstruct #1=#:anon-type-1587
     (a :int)
     (b :int)))
 (cffi:defctype anonymous-t #1#))

;; example.h:23:7
(cffi:defcstruct my-union
  (c :char)
  (i :int)
  (d :double))

;; example.h:29:6
(cffi:defcenum some-values
  (:a-value 0)
  (:another-value 1)
  (:yet-another-value 2))

;; example.h:35:6
(cffi:defcfun (do-something "do_something")
    :void
  (p (:pointer my-point-t))
  (x :int)
  (y :int))

(export
 '(do-something some-values my-union anonymous-t my-point-t my-point foo blah
                someextern +bar+))

;;;; macros.lisp:
(common-lisp:in-package :c2ffi-another-package)

;; macros.h:7:13
(common-lisp:defvar +quux+ "abc")

;; macros.h:8:18
(common-lisp:defconstant +foo+ 4)

(common-lisp:export '(+foo+ +quux+))
```

## Usage

First, produce a `spec` file using `c2ffi`:

```console
$ cd example/
$ c2ffi -M macros.h -o example.spec example.h
$ c2ffi -o macros.spec macros.h
```

Now you can generate a file manually like the following:

```lisp
(c2ffi-cffi:parse-file-to-file "example.spec" "example.lisp")
(c2ffi-cffi:parse-file-to-file "macros.spec" "macros.lisp")
```

Note this probably didn't work, unless you specified the full path to
the files.  Easier, especially for projects, is to use the provided
ASDF source-file type:

```lisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system :c2ffi-cffi))

(defsystem :my-system
  :
  :components ((c2ffi-cffi:spec "example")
               (c2ffi-cffi:spec "macros")))
```

This will, at load/compile time, generate and load `example.lisp` and
`macros.lisp` from the `example.spec` and `macros.spec`.  You may also
specify a `:package PACKAGE`, which will put an `in-package` at the
top of the generated files.  Otherwise this will default to
:COMMON-LISP-USER, which is probably not what you want.

Note that the package must exist at the time the file is loaded, or
you will get an error.  This is necessary to write fully-qualified
symbols.

### Exclusions

Because some things are not supported by CFFI, like bitfields, and
some are occasionally even unsupported by c2ffi (vector literals?),
you can now specify exclusions as follows:

```lisp
(let ((c2ffi-cffi:*exclude-sources*
       '("/path/to/bad-stuff/.*" ...))
      (c2ffi-cffi:*exclude-definitions*
       '("SomeBadCFun1" "SomeBad.*Funs")))
  (c2ffi-cffi:parse...))
```

Both use `cl-ppcre` regexps to pre-exclude entire files or specific
definitions.  You will simply see the file/line and `nil` in the
output.

This is also supported with ASDF:

```lisp
  :
  :components ((c2ffi-cffi:spec "example"
                  :exclude-sources ("/path/.*" ...)
                  :exclude-definitions ("Bad.*Defs" ...)))
```
