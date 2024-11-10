# LISP Cookbook

## Optimise

```lisp
(proclaim '(optimize (speed 3) (safety 0) (space 0) (debug 0)))
```

Compile functions to achieve TCO, not needed for *SBCL*:

```lisp
(compile <function name>)
```

## Rename macro

```(setf (macro-function 'mvb) (macro-function 'multiple-value-bind))```

## Get current datetime

```lisp
(defun current-date-string ()
  "Returns current date as a string."
  (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
                       (get-decoded-time)
    (declare (ignore sec min hr dow dst-p tz))
    (format nil "~4,'0d-~2,'0d-~2,'0d" yr mon day) ))
```

## Lists to/from arrays

### Lists to 2D array

```lisp
(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))

```

### 2D array to lists

```lisp
(defun 2d-array-to-list (array)
  (loop for i below (array-dimension array 0)
        collect (loop for j below (array-dimension array 1)
                      collect (aref array i j))))

```

## Elements from strings

```lisp
(defun parse-string-to-elem (line)
  (with-input-from-string (s line)
    (loop
      for elem := (read s nil nil)
      while elem
      collect elem)))
```

## Number string to digits

```lisp
(defun chars-to-nums (chars)
  (loop for i across chars for num := (digit-char-p i) collect num))
```

## Read multiple lines from stdin

```lisp
(defvar lines ())
(loop for line = (read-line *terminal-io* nil :eof)
      until (or (eq line :eof) (zerop (length line)))
      do (push line lines))
(format t "~&~S" lines)
```

```lisp
(let ((i (read-line))
      (lines ()))
     (loop while (> (length i) 0) do
         (push i lines)
         (setf i (read-line)))
     lines)
```

```lisp
(loop
    for line := (read-line)
    while (not (zerop (length line)))
    collect line)
```

### Hackerank input

```
3
123
345
789
```

```lisp
(defun read-input ()
  (loop
    for line := (read-line *standard-input*  nil nil)
    while (and line (not (zerop (length line))))
    collect line))

```

### Read multiple elements as strings until EOF/Ctrl-D

```lisp
(defun read-elems-as-strings (stream)
  (loop
    for e := (read stream nil :eof)
    while (not (equalp e :eof))
    collect  e))
```


## Read objects from file

```lisp
(defun read-my-file (fname)
  (with-open-file
   (stream fname)
    (let ((contents
            (read-all-objects stream (list '$eof$))))
      (format t "~&Read ~S objects from the file."
              (length contents))
      contents)))
```

```lisp
(defun read-all-objects (stream &aux (eof '#:eof))
  (loop for item = (read stream nil eof)
        until (eq item eof)
        collect item))
```

## Strings

### Fill with character

```lisp
(format nil "~v@{~A~:*~}" 3 #\*) ;;Repeat '*' three times
```

### Whitespace delimited string


```lisp
(defun read-string (&optional (stream *standard-input*))
  (loop
     for c = (peek-char nil stream nil nil)              ; include whitespace
     while (and c (eql c (peek-char t stream nil nil)))  ; skip whitespace
     collect (read-char stream) into letters
     finally (return (coerce letters 'string))))
```

```lisp
(defun read-string (&optional (stream *standard-input*))
  (with-output-to-string (out)
    (loop
       for c = (peek-char nil stream nil nil)
       while (and c (eql c (peek-char t stream nil nil)))
       do (write-char (read-char stream) out))))
```

## Execute code in file

```bash
sbcl --load ./cavity-map.lisp --eval "(progn (main) (sb-ext:quit))"
```


## Create package

[Link](https://www.youtube.com/watch?v=LqBbGFMPcDI)

### 0. Create file with package description

### 1. Create ASDF file in root directory

In directory `hello-web`:

```
(asdf:defsystem #:hello-web
  :description "Web app tutor"

  :author ""
  :license  "BSD"
  :version "0.0.0"
  :serial t
  :depends-on (#:clog)
  :components ((:file "hello")))

```

### 2. Add path to local quicklisp project path

```lisp
(push #P"~/projects/lisp/hello-web"
    ql:*local-project-directories*)
```

### 3. Load with quicklisp

```lisp
(ql:quickload :hello-web)
```

## Compile standalone executable

```lisp
;; invoke with: sbcl --load ./compile.lisp
;;(push '#".")
(push (uiop:getenv "PWD") ql:*local-project-directories*)
(ql:quickload :clog)
(load (compile-file "hello.lisp"))
(save-lisp-and-die "hello.exe" :toplevel #'hello-pkg:hello-world :executable t)
```
