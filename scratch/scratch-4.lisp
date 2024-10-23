(defun read-lines ()
  (loop for line = (read-line *terminal-io* nil :eof)
        until (or (eq line :eof) (zerop (length line)))
        collect line))

(defun read-elems-as-strings (stream)
  (loop for e = (read stream nil :eof)
        until (equalp e :eof)
        collect (prin1-to-string e)))

(defun read-elems (stream)
  (loop
    for e := (read stream nil ::eof)
    while (not (equalp e :eof))
    collect e))

(defvar ccc (read-elems *terminal-io*))
(print ccc)
