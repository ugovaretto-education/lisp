;;;; https://gist.github.com/mayerrobert/913b4c26103c614f9517360a4f00286a
;;;; Matrix multiplication optimization experiments
;;;
;;; This file contains functions to multiply two matrices written in Common Lisp.
;;;
;;; DISCLAIMER: while this code has some tests it might contains bugs,
;;;             I made it to experiment with SBCL's SIMD support.
;;;
;;; All functions use the naive nested loop algorithm which is O(n^3).
;;; This file contains 5 functions with various optimizations
;;; that should be portable Common Lisp and 5 functions that use sbcl's SB-SIMD.
;;; (the last SIMD function 'matrix-multiply-ikj-simd-unrolled-blocked2' is not
;;; that useful, though, leaving 4 SIMD functions)
;;;
;;; All 10 functions should produce the same result, the multiplication of two matrices.
;;;
;;; Execution times with sbcl-2.3.3 for multiplying
;;; two single-float (1000x1000) x (1000x1000) matrices range from:
;;;
;;; - 1.6 seconds for 'matrix-multiply-ikj' through
;;; - 0.73 seconds for 'matrix-multiply-ikj-unrolled-blocked' (fastest portable version) down to
;;; - 0.125 seconds for 'matrix-multiply-ikj-simd-unrolled-blocked'.
;;;
;;; and for multiplying two single-float (5000x2000) x (2000x5000) matrices range from:
;;;
;;; - 85 seconds for 'matrix-multiply-ikj' through
;;; - 39 seconds for 'matrix-multiply-ikj-unrolled-blocked' (fastest portable version) down to
;;; - 6.4 seconds for 'matrix-multiply-ikj-simd-unrolled-blocked'.
;;;
;;; Optimizations include loop unrolling, cache blocking and SIMD instructions.
;;;
;;;
;;; a is an m x p matrix, b is an p x n matrix, result is an m x n matrix.
;;;
;;; matrix-multiplication ::= for i=1..m
;;;                             for j=1..n
;;;                               tmp = 0
;;;                               for k=1..p
;;;                                  tmp += a(i,k) * b(k,j)
;;;                               result(i,j) = tmp
;;;
;;; or with the hot loop reading consecutive memory locations:
;;;
;;; matrix-multiplication ::= for k=1..p
;;;                             for i=1..m
;;;                               for j=1..n
;;;                                 result(i,j) += a(i,k) * b(k,j)
;;;
;;; or with better cache locality:
;;;
;;; matrix-multiplication ::= for i=1..m
;;;                             for k=1..p
;;;                               for j=1..n
;;;                                 result(i,j) += a(i,k) * b(k,j)
;;;
;;;
;;; Run with
;;;     sbcl --script mmult-simd.lisp
;;;
;;; To use all provided functions you need sbcl-x64 2.2.6+
;;; and a CPU that supports the selected instruction set.
;;;
;;; Other Common Lisp implementations will only be able to use the 5 portable functions.
;;;
;;; E.g. on abcl run with:
;;;   C:\> abcl --batch --eval "(compile-file \"mmult-simd.lisp\")"
;;;   C:\> abcl --batch --load mmult-simd.abcl
;;;   Using iblock=16, jblock=1536, iblock x jblock x 4 x 3=288k
;;;   MATRIX-MULTIPLY-IKJ-UNROLLED-BLOCKED (1000x1000) by (1000x1000)
;;;   12.501 seconds real time
;;;   4000002 cons cells


(format t "Running ~a ~a~%" (lisp-implementation-type) (lisp-implementation-version))


(eval-when (:compile-toplevel :load-toplevel :execute)

  ;; comment out the next line for older sbcl versions w/o SB-SIMD
  #+(and sbcl :X86-64) (push :use-simd *features*)


  ;; comment out the next line for older CPUs to use SSE
  (push :avx256 *features*)


  ;; comment out the next line for older CPUs that don't have FMA or if you don't want to use FMA
  ;; which computes different results because of different intermediate precision
  ;(push :fma *features*)


  ;; comment out the next line to run with safety==0 and disable tests
  ;(push :mmult-test *features*)


  ;; uncomment (activate) the next line to run the profiler (unless we're running on windows)
  ;(push :mmult-profile *features*)

)


(declaim (optimize (speed 3)
                   (compilation-speed 0)
                   #-mmult-test (safety 0)
                   #-mmult-test (debug 0)))


(eval-when (:compile-toplevel :load-toplevel :execute)

  #+use-simd
  (require "sb-simd")

)


(deftype array-index ()
  `(integer 0 (,array-dimension-limit)))

(deftype array-element ()
  `(single-float))


;; Use SSE instructions
#+(and :use-simd (not :avx256))
(progn
  (defconstant +simd-size+ (coerce 4 'array-index))

  (deftype simd-vector ()
    '(sb-simd-sse:f32.4))

  (defmacro simd-vector (&rest args)
    `(sb-simd-sse:f32.4 ,@args))

  (defmacro simd-+ (vec val)
    `(sb-simd-sse:f32.4+ ,vec ,val))

  (defmacro simd-* (vec val)
    `(sb-simd-sse:f32.4* ,vec ,val))

  (defmacro simd-fmadd (mulop1 mulop2 addop)
    #+fma `(sb-simd-fma:f32.4-fmadd ,mulop1 ,mulop2 ,addop)
    #-fma `(simd-+ (simd-* ,mulop1 ,mulop2) ,addop))

  (defmacro simd-row-major-aref (vec &rest subscripts)
    `(sb-simd-sse:f32.4-row-major-aref ,vec ,@subscripts))

  (defmacro simd-aref (vec &rest subscripts)
    `(sb-simd-sse:f32.4-aref ,vec ,@subscripts))

  (format t "Using SSE~%")
)


;; Use AVX256
#+(and :use-simd :avx256)
(progn
  (defconstant +simd-size+ (coerce 8 'fixnum))

  (deftype simd-vector ()
    '(sb-simd-avx:f32.8))

  (defmacro simd-vector (&rest args)
    `(sb-simd-avx:f32.8 ,@args))

  (defmacro simd-+ (vec val)
    `(sb-simd-avx:f32.8+ ,vec ,val))

  (defmacro simd-* (vec val)
    `(sb-simd-avx:f32.8* ,vec ,val))

  (defmacro simd-fmadd (mulop1 mulop2 addop)
    #+fma `(sb-simd-fma:f32.8-fmadd ,mulop1 ,mulop2 ,addop)
    #-fma `(simd-+ (simd-* ,mulop1 ,mulop2) ,addop))

  (defmacro simd-row-major-aref (vec &rest subscripts)
    `(sb-simd-avx:f32.8-row-major-aref ,vec ,@subscripts))

  (defmacro simd-aref (vec &rest subscripts)
    `(sb-simd-avx:f32.8-aref ,vec ,@subscripts))

  (format t "Using AVX-256~%")
)

(defmacro fmadd (mulop1 mulop2 addop)
  "With scalar arguments compute 'mulop1 * mulop2 + addop' ::= '(+ (* mulop1 mulop2) addop)'."
  #+fma `(sb-simd-fma:f32-fmadd ,mulop1 ,mulop2 ,addop)
  #-fma `(+ (* ,mulop1 ,mulop2) ,addop))

(format t #+fma "Using FMA~%" #-fma "Not using FMA~%")


(setf (documentation 'simd-fmadd 'function)
      "With vector arguments compute 'mulop1 * mulop2 + addop' ::= '(+ (* mulop1 mulop2) addop)'.")



;; the blocksizes are tailored towards a 64k L1 cache,
;; and the blocksizes x 4 should probably be multiples of 64-byte cachelines
;; (4 bytes for single floats)
(defconstant +iblock-size+ 16)
(defconstant +kblock-size+ 16)
(defconstant +jblock-size+ 128)

(defconstant +unroll-sisd+ 4)
(defconstant +unroll-simd+ 4)

(declaim (fixnum +iblock-size+ +jblock-size+ +kblock-size+ +unroll-sisd+ +unroll-simd+))

(format t "Using iblock=~d, kblock=~d, jblock=~d~%" +iblock-size+ +kblock-size+ +jblock-size+)

;(defun cache (m p n)
;  (declare (array-index m p n))
;  (format t "cache for (~d x ~d) = (~d x ~d) x (~d x ~d), kij-blocked: ~,1fk~%"
;          m n m p p n (/ (* 4 (+ (* +iblock-size+ p) (* +iblock-size+ n) +jblock-size+)) 1024)))

;(cache 1000 1000 1000)
;(cache 5000 2000 5000)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; various implementations of matrix multiplication

(defmacro common-checks ()
  `(assert (and (= (array-rank a) (array-rank b) 2)
                (= (array-dimension a 1) (array-dimension b 0)))
           (a b)
           "Cannot multiply ~S by ~S." a b))

(defun matrix-multiply-ijk-gemm (a b)
  "Performs matrix multiplication of two arrays."
  (declare (type (simple-array array-element 2) a b))
  (common-checks)

  (let* ((m (array-dimension a 0))
         (n (array-dimension b 1))
         (p (array-dimension a 1))
         (result (make-array (list m n) :element-type 'array-element
                                        :adjustable nil
                                        :fill-pointer nil
                                        :initial-element 0.0)))
    (declare (array-index m n p))

    (dotimes (i m)
      (declare (array-index i))

      (dotimes (j n)
        (declare (array-index j))

        (let ((tmp 0.0))
          (declare (array-element tmp))
          (dotimes (k p)
            (declare (array-index k))
            (incf tmp (* (aref a i k) (aref b k j))))
          (setf (aref result i j) tmp))))

    result))


(defun matrix-multiply-ijk-gemm-opt (a b)
  "Performs matrix multiplication of two arrays."
  (declare (type (simple-array array-element 2) a b))
  (common-checks)

  (let* ((m (array-dimension a 0))
         (n (array-dimension b 1))
         (p (array-dimension a 1))
         (result (make-array (list m n) :element-type 'array-element
                                        :adjustable nil
                                        :fill-pointer nil
                                        :initial-element 0.0)))
    (declare (array-index m n p))

    (dotimes (i m)
      (declare (array-index i))

      (dotimes (j n)
        (declare (array-index j))

        (let ((tmp 0.0)
              (a-idx (array-row-major-index a i 0))
              (b-idx (array-row-major-index b 0 j)))
          (declare (array-element tmp) (array-index a-idx b-idx))
          (loop repeat p do
            (incf tmp (* (row-major-aref a a-idx) (row-major-aref b b-idx)))
            (incf a-idx)
            (incf b-idx n))
          (setf (aref result i j) tmp))))

    result))


(defun matrix-multiply-ikj-gemm-blocked (a b)
  "Performs matrix multiplication of two arrays."
  (declare (type (simple-array array-element 2) a b))
  (common-checks)

  (let* ((m (array-dimension a 0))
         (n (array-dimension b 1))
         (p (array-dimension a 1))
         (result (make-array (list m n) :element-type 'array-element
                                        :adjustable nil
                                        :fill-pointer nil
                                        :initial-element 0.0)))
    (declare (array-index m n p))

    (dotimes (i m)
      (declare (array-index i))

      (do ((jblock 0 (+ jblock +jblock-size+)))
          ((>= jblock n))
        (declare (array-index jblock))

        (do ((kblock 0 (+ kblock +kblock-size+)))
            ((>= kblock p))
          (declare (array-index kblock))

          (dotimes (koffset (min +kblock-size+ (- p kblock)))
            (declare (array-index koffset))

            (dotimes (joffset (min +jblock-size+ (- n jblock)))
              (declare (array-index joffset))
              (incf (aref result i (+ jblock joffset))
                    (* (aref a i (+ kblock koffset))
                       (aref b (+ kblock koffset) (+ jblock joffset)))))))))

    result))


(defun matrix-multiply-ikj-gemm-blocked-opt (a b)
  "Performs matrix multiplication of two arrays."
  (declare (type (simple-array array-element 2) a b))
  (common-checks)

  (let* ((m (array-dimension a 0))
         (n (array-dimension b 1))
         (p (array-dimension a 1))
         (result (make-array (list m n) :element-type 'array-element
                                        :adjustable nil
                                        :fill-pointer nil
                                        :initial-element 0.0)))
    (declare (array-index m n p))

    (dotimes (i m)
      (declare (array-index i))

      (do ((jblock 0 (+ jblock +jblock-size+)))
          ((>= jblock n))
        (declare (array-index jblock))

        (do ((kblock 0 (+ kblock +kblock-size+)))
            ((>= kblock p))
          (declare (array-index kblock))

          (dotimes (koffset (min +kblock-size+ (- p kblock)))
            (declare (array-index koffset))

            (let ((a-i-k (aref a i (+ kblock koffset)))
                  (b-idx (array-row-major-index b (+ kblock koffset) jblock))
                  (result-idx (array-row-major-index result i jblock)))
              (declare (array-element a-i-k) (array-index b-idx result-idx))

              (loop repeat (min +jblock-size+ (- n jblock)) do
                    (incf (row-major-aref result result-idx) (* a-i-k (row-major-aref b b-idx)))
                    (incf result-idx)
                    (incf b-idx)))))))

    result))


(defun matrix-multiply-ikj-gemm-column-blocked (a b)
  "Performs matrix multiplication of two arrays."
  (declare (type (simple-array array-element 2) a b))
  (common-checks)

  (let* ((m (array-dimension a 0))
         (n (array-dimension b 1))
         (p (array-dimension a 1))
         (result (make-array (list m n) :element-type 'array-element
                                        :adjustable nil
                                        :fill-pointer nil
                                        :initial-element 0.0)))
    (declare (array-index m n p))

    (do ((jblock 0 (+ jblock +jblock-size+)))
        ((>= jblock n))
      (declare (array-index jblock))

      (let ((jlimit (min +jblock-size+ (- n jblock))))
        (declare (array-index jlimit))

        (dotimes (i m)
          (declare (array-index i))

          (let ((result-idx-start (array-row-major-index result i jblock)))
            (declare (array-index result-idx-start))

            (do ((kblock 0 (+ kblock +kblock-size+)))
                ((>= kblock p))
              (declare (array-index kblock))

              (dotimes (koffset (min +kblock-size+ (- p kblock)))
                (declare (array-index koffset))

                (let* ((k (+ kblock koffset))
                       (a-i-k (aref a i k))
                       (b-idx (array-row-major-index b k jblock))
                       (result-idx result-idx-start))
                  (declare (array-element a-i-k) (array-index k b-idx result-idx))

                  (loop repeat jlimit do
                        (incf (row-major-aref result result-idx) (* a-i-k (row-major-aref b b-idx)))
                        (incf result-idx)
                        (incf b-idx)))))))))

    result))


(defun matrix-multiply-ijk-blocked (a b)
  "Performs matrix multiplication of two arrays."
  (declare (type (simple-array array-element 2) a b))
  (common-checks)

  (let* ((m (array-dimension a 0))
         (n (array-dimension b 1))
         (p (array-dimension a 1))
         (result (make-array (list m n) :element-type 'array-element
                                        :adjustable nil
                                        :fill-pointer nil
                                        :initial-element 0.0)))
    (declare (array-index m n p))

    (dotimes (i m)
      (declare (array-index i))

      (do ((jblock 0 (+ jblock +jblock-size+)))
          ((>= jblock n))
        (declare (array-index jblock))

        (do ((kblock 0 (+ kblock +kblock-size+)))
            ((>= kblock p))
          (declare (array-index kblock))

          (dotimes (joffset (min +jblock-size+ (- n jblock)))
            (declare (array-index joffset))

            ;(dotimes (koffset (min +kblock-size+ (- p kblock)))
            ;  (declare (array-index koffset))
            ;  (incf (aref result i (+ jblock joffset))
            ;        (* (aref a i (+ kblock koffset))
            ;           (aref b (+ kblock koffset) (+ jblock joffset)))))))))
            (loop with acc of-type array-element = (aref result i (+ jblock joffset))
                  repeat (min +kblock-size+ (- p kblock))
                  for a-idx of-type array-index from (array-row-major-index a i kblock) by 1
                  and b-idx of-type array-index from (array-row-major-index b kblock (+ jblock joffset)) by n

                  do (setq acc (fmadd (row-major-aref a a-idx) (row-major-aref b b-idx) acc))

                  finally (setf (aref result i (+ jblock joffset)) acc))))))

    result))


;; use loop order i/k/j so that the hot loop will read and write consecutive memory locations
;; and the same row of the result array will be completed before moving to the next result row.
(defun matrix-multiply-ikj (a b)
  "Performs matrix multiplication of two arrays."
  (declare (type (simple-array array-element 2) a b))
  (common-checks)

  (let* ((m (array-dimension a 0))
         (n (array-dimension b 1))
         (p (array-dimension a 1))
         (result (make-array (list m n) :element-type 'array-element
                                        :adjustable nil
                                        :fill-pointer nil
                                        :initial-element 0.0)))
    (declare (array-index m n p))

    (dotimes (i m)
      (declare (array-index i))

      (dotimes (k p)
        (declare (array-index k))

        (dotimes (j n)
          (declare (array-index j))
          (incf (aref result i j) (* (aref a i k) (aref b k j))))))

    result))


;(setq SB-C::*compile-progress* t)
;(setq SB-C::*show-transforms-p* t)
;(setq SB-ASSEM::*show-peephole-transforms-p* t)
;(setq SB-C::*compiler-trace-output* *standard-output*)

;; simple optimizations:
;; lift (aref a i k) out of the hot loop
;; calculate addresses by incrementing the indices
(defun matrix-multiply-ikj-opt (a b)
  "Performs matrix multiplication of two arrays."
  (declare (type (simple-array array-element 2) a b))
  (common-checks)

  (let* ((m (array-dimension a 0))
         (n (array-dimension b 1))
         (p (array-dimension a 1))
         (result (make-array (list m n) :element-type 'array-element
                                        :adjustable nil
                                        :fill-pointer nil
                                        :initial-element 0.0)))
    (declare (array-index m n p))

    (dotimes (i m)
      (declare (array-index i))

      (dotimes (k p)
        (declare (array-index k))

        (let ((a-i-k (aref a i k))
              (result-idx (array-row-major-index result i 0))
              (b-idx (array-row-major-index b k 0)))
          (declare (array-element a-i-k) (array-index result-idx b-idx))

          (loop repeat n do
            (incf (row-major-aref result result-idx) (* a-i-k (row-major-aref b b-idx)))
            (incf result-idx)
            (incf b-idx)))))

    result))

;(setq SB-C::*compiler-trace-output* nil)
;(setq SB-ASSEM::*show-peephole-transforms-p* nil)
;(setq SB-C::*show-transforms-p* nil)
;(setq SB-C::*compile-progress* nil)


;; similar to matrix-multiply-ikj-opt plus unroll the hot loop
(defun matrix-multiply-ikj-unrolled (a b)
  "Performs matrix multiplication of two arrays."
  (declare (type (simple-array array-element 2) a b))
  (common-checks)

  (let* ((m (array-dimension a 0))
         (n (array-dimension b 1))
         (p (array-dimension a 1))
         (result (make-array (list m n) :element-type 'array-element
                                        :adjustable nil
                                        :fill-pointer nil
                                        :initial-element 0.0)))
    (declare (array-index m n p))

    (dotimes (i m)
      (declare (array-index i))

      (dotimes (k p)
        (declare (array-index k))

        (let ((a-i-k (aref a i k))
              (result-idx (array-row-major-index result i 0))
              (b-idx (array-row-major-index b k 0)))
          (declare (array-element a-i-k) (array-index result-idx b-idx))

          (do ((j 0 (+ j +unroll-sisd+)))
              ((> j (- n +unroll-sisd+))
               (loop repeat (- n j) do
                     (setf #1=(row-major-aref result result-idx) (fmadd (row-major-aref b b-idx) a-i-k #1#))
                     (incf result-idx)
                     (incf b-idx)))

            #.`(progn ,@(loop for u below +unroll-sisd+
                              collect `(let ((tmpidx (+ result-idx ,u)))
                                         (declare (array-index tmpidx))
                                         (setf #2=(row-major-aref result tmpidx) (fmadd (row-major-aref b (+ b-idx ,u)) a-i-k #2#)))))

            (incf result-idx +unroll-sisd+)
            (incf b-idx +unroll-sisd+)))))

    result))


;; similar to matrix-multiply-ikj-opt but with cache blocking
(defun matrix-multiply-ikj-blocked (a b)
  "Performs matrix multiplication of two arrays."
  (declare (type (simple-array array-element 2) a b))
  (common-checks)

  (let* ((m (array-dimension a 0))
         (n (array-dimension b 1))
         (p (array-dimension a 1))
         (result (make-array (list m n) :element-type 'array-element
                                        :adjustable nil
                                        :fill-pointer nil
                                        :initial-element 0.0)))
    (declare (array-index m n p))

    (do ((iblock 0 (+ iblock +iblock-size+)))
        ((>= iblock m))
      (let ((ilimit (min m (+ iblock +iblock-size+))))

        (do ((jblock 0 (+ jblock +jblock-size+)))
            ((>= jblock n))
          (let ((jlimit (min n (+ jblock +jblock-size+))))

            (dotimes (k p)
              (declare (array-index k))

              (do ((i iblock (1+ i)))
                  ((>= i ilimit))
                (declare (array-index i))

                (let ((a-i-k (aref a i k))
                      (result-idx (array-row-major-index result i jblock))
                      (b-idx (array-row-major-index b k jblock)))
                  (declare (array-element a-i-k) (array-index result-idx b-idx))

                  (loop repeat (- jlimit jblock) do
                    (incf (row-major-aref result result-idx) (* a-i-k (row-major-aref b b-idx)))
                    (incf result-idx)
                    (incf b-idx)))))))))

    result))


;; similar to matrix-multiply-ikj-opt but with loop unrolling and cache blocking
(defun matrix-multiply-ikj-unrolled-blocked (a b)
  "Performs matrix multiplication of two arrays."
  (declare (type (simple-array array-element 2) a b))
  (common-checks)

  (loop with m of-type array-index = (array-dimension a 0)
        with n of-type array-index = (array-dimension b 1)
        with p of-type array-index = (array-dimension a 1)
        with result = (make-array (list m n) :element-type 'array-element
                                             :adjustable nil
                                             :fill-pointer nil
                                             :initial-element 0.0)
        finally (return result)

        for iblock of-type array-index from 0 below m by +iblock-size+

        do (loop with ilimit of-type array-index = (min m (+ iblock +iblock-size+))
                 for jblock of-type array-index from 0 below n by +jblock-size+

                 do (loop with jlimit of-type array-index = (min n (+ jblock +jblock-size+))
                          for k of-type array-index from 0 below p

                          do (loop for i of-type array-index from iblock below ilimit

                                   do (loop with a-i-k     of-type array-element = (aref a i k)
                                            for j          of-type fixnum      from jblock below (- jlimit +unroll-sisd+) by +unroll-sisd+
                                            and result-idx of-type array-index from (array-row-major-index result i jblock) by +unroll-sisd+
                                            and b-idx      of-type array-index from (array-row-major-index b k jblock) by +unroll-sisd+

                                            do #.`(progn ,@(loop for u below +unroll-sisd+
                                                                 collect `(incf (row-major-aref result (+ result-idx ,u))
                                                                                (* a-i-k (row-major-aref b (+ b-idx ,u))))))

                                            finally (loop repeat (- jlimit j)
                                                          do
                                                          (incf (row-major-aref result result-idx) (* a-i-k (row-major-aref b b-idx)))
                                                          (incf result-idx)
                                                          (incf b-idx))))))))


(defun matrix-multiply-ikj-vector (a b)
  "Performs matrix multiplication of two arrays."
  (declare (type (simple-array array-element 2) a b))
  (common-checks)

  (let* ((m (array-dimension a 0))
         (n (array-dimension b 1))
         (p (array-dimension a 1))
         (%a (sb-vm::%array-data a))
         (a-idx (array-row-major-index a 0 0))
         (%b (sb-vm::%array-data b))
         (result (make-array (list m n) :element-type 'array-element
                                        :adjustable nil
                                        :fill-pointer nil
                                        :initial-element 0.0))
         (%result (sb-vm::%array-data result)))
    (declare (array-index m n p a-idx))

    (dotimes (i m)
      (declare (array-index i))

      (dotimes (k p)
        (declare (array-index k))

        (let ((a-i-k (aref %a (+ a-idx k)))
              (result-idx (* i n))
              (b-idx (* k n)))
          (declare (array-element a-i-k) (array-index result-idx b-idx))

          (loop repeat n
                do
                (incf (aref %result result-idx) (* a-i-k (aref %b b-idx)))
                (incf result-idx)
                (incf b-idx))))
      (incf a-idx p))

    result))


(defun matrix-multiply-ikj-vector-unrolled (a b)
  "Performs matrix multiplication of two arrays."
  (declare (type (simple-array array-element 2) a b))
  (common-checks)

  (let* ((m (array-dimension a 0))
         (n (array-dimension b 1))
         (p (array-dimension a 1))
         (%b (sb-vm::%array-data b))
         (result (make-array (list m n) :element-type 'array-element
                                        :adjustable nil
                                        :fill-pointer nil
                                        :initial-element 0.0))
         (%result (sb-vm::%array-data result)))
    (declare (array-index m n p))

    (dotimes (i m)
      (declare (array-index i))

      (dotimes (k p)
        (declare (array-index k))

        (let ((a-i-k (aref a i k))
              (result-idx (* i n))
              (b-idx (* k n)))
          (declare (array-element a-i-k) (array-index result-idx b-idx))

          (do ((j 0 (+ j +unroll-sisd+)))
              ((> j (- n +unroll-sisd+))
               (loop repeat (- n j) do
                     (setf #1=(aref %result result-idx) (fmadd (aref %b b-idx) a-i-k #1#))
                     (incf result-idx)
                     (incf b-idx)))
            (declare (array-index j))

            #.`(progn ,@(loop for u  below +unroll-sisd+
                              collect `(let ((tmpidx (+ result-idx ,u)))
                                         (declare (array-index tmpidx))
                                         (setf #2=(aref %result tmpidx) (fmadd (aref %b (+ b-idx ,u)) a-i-k #2#)))))

            (incf result-idx +unroll-sisd+)
            (incf b-idx +unroll-sisd+)))))

    result))


;; similar to matrix-multiply-ikj-opt except: use SIMD
#+use-simd
(defun matrix-multiply-ikj-simd (a b)
  "Performs matrix multiplication of two arrays."
  (declare (type (simple-array array-element 2) a b))
  (common-checks)

  (let* ((m (array-dimension a 0))
         (n (array-dimension b 1))
         (p (array-dimension a 1))
         (result (make-array (list m n) :element-type 'array-element
                                        :adjustable nil
                                        :fill-pointer nil
                                        :initial-element 0.0)))
    (declare (array-index m n p))

    (dotimes (i m)
      (declare (array-index i))

      (dotimes (k p)
        (declare (array-index k))

        (let ((va-i-k (simd-vector (aref a i k)))
              (a-i-k (aref a i k))
              (result-idx (array-row-major-index result i 0))
              (b-idx (array-row-major-index b k 0)))
          (declare (simd-vector va-i-k) (array-element a-i-k) (array-index result-idx b-idx))

          (do ((j 0 (+ j +simd-size+)))
              ((> j (- n +simd-size+))
               (loop repeat (- n j)
                     do (setf #2=(row-major-aref result result-idx) (fmadd (row-major-aref b b-idx) a-i-k #2#))

                 (incf result-idx)
                 (incf b-idx)))
            (declare (array-index j))

            (setf #1=(simd-row-major-aref result result-idx)
                  (simd-fmadd (simd-row-major-aref b b-idx) va-i-k #1#))

            (incf result-idx +simd-size+)
            (incf b-idx +simd-size+)))))

    result))


;; similar to matrix-multiply-ikj-simd but with cache blocking
#+use-simd
(defun matrix-multiply-ikj-simd-blocked (a b)
  "Performs matrix multiplication of two arrays."
  (declare (type (simple-array array-element 2) a b))
  (common-checks)

  (let* ((m (array-dimension a 0))
         (n (array-dimension b 1))
         (p (array-dimension a 1))
         (result (make-array (list m n) :element-type 'array-element
                                        :adjustable nil
                                        :fill-pointer nil
                                        :initial-element 0.0)))
    (declare (array-index m n p))

    (do ((iblock 0 (+ iblock +iblock-size+)))
        ((>= iblock m))
      (let ((ilimit (min m (+ iblock +iblock-size+))))
        (do ((jblock 0 (+ jblock +jblock-size+)))
            ((>= jblock n))
          (let ((jlimit (min n (+ jblock +jblock-size+))))

            (dotimes (k p)
              (declare (array-index k))

              (do ((i iblock (1+ i)))
                  ((>= i ilimit))
                (declare (array-index i))

                (let* ((a-i-k (aref a i k))
                       (va-i-k (simd-vector a-i-k))
                       (result-idx (array-row-major-index result i jblock))
                       (b-idx (array-row-major-index b k jblock)))
                  (declare (simd-vector va-i-k) (array-element a-i-k) (array-index result-idx b-idx))

                  (do ((j jblock (+ j +simd-size+)))
                      ((> j (- jlimit +simd-size+))
                       (loop repeat (- jlimit j) do
                         (incf (row-major-aref result result-idx) (* a-i-k (row-major-aref b b-idx)))
                         (incf result-idx)
                         (incf b-idx)))

                    (setf #1=(simd-row-major-aref result result-idx)
                          (simd-+ #1# (simd-* (simd-row-major-aref b b-idx) va-i-k)))

                    (incf result-idx +simd-size+)
                    (incf b-idx +simd-size+)))))))))

    result))


;; similar to matrix-multiply-ikj-unrolled except: use SIMD
#+use-simd
(defun matrix-multiply-ikj-simd-unrolled (a b)
  "Performs matrix multiplication of two arrays."
  (declare (type (simple-array array-element 2) a b))
  (common-checks)

  (let* ((m (array-dimension a 0))
         (n (array-dimension b 1))
         (p (array-dimension a 1))
         (result (make-array (list m n) :element-type 'array-element
                                        :adjustable nil
                                        :fill-pointer nil
                                        :initial-element 0.0)))
    (declare (array-index m n p))

    (dotimes (i m)
      (declare (array-index i))
      (let ((result-idx-start (array-row-major-index result i 0)))

      (dotimes (k p)
        (declare (array-index k))

        (let* ((a-i-k (aref a i k))
               (va-i-k (simd-vector a-i-k))
               (result-idx result-idx-start)
               (b-idx (array-row-major-index b k 0)))
          (declare (array-element a-i-k) (simd-vector va-i-k) (array-index result-idx b-idx))

          (do ((j 0 (+ j (* +unroll-simd+ +simd-size+))))
              ((> j (- n (* +unroll-simd+ +simd-size+)))
               (do ((jj j (1+ jj)))
                   ((>= jj n))
                 (setf #2=(row-major-aref result result-idx) (fmadd (row-major-aref b b-idx) a-i-k #2#))
                 (incf result-idx)
                 (incf b-idx)))

            #.`(progn ,@(loop for u below +unroll-simd+
                              collect `(let ((tmpidx (+ (* ,u +simd-size+) result-idx)))
                                         (declare (array-index tmpidx))
                                         (setf #1=(simd-row-major-aref result tmpidx)
                                               (simd-fmadd (simd-row-major-aref b (+ (* ,u +simd-size+) b-idx)) va-i-k #1#)))))

            (incf result-idx (* +unroll-simd+ +simd-size+))
            (incf b-idx (* +unroll-simd+ +simd-size+)))))))

    result))


#+use-simd
(defun matrix-multiply-ikj-simd-vector-unrolled (a b)
  "Performs matrix multiplication of two arrays."
  (declare (type (simple-array array-element 2) a b))
  (common-checks)

  (let* ((m (array-dimension a 0))
         (n (array-dimension b 1))
         (p (array-dimension a 1))
         (result (make-array (list m n) :element-type 'array-element
                                        :adjustable nil
                                        :fill-pointer nil
                                        :initial-element 0.0))
         (a-idx 0)
         (%a (sb-vm::%array-data a))
         (%b (sb-vm::%array-data b))
         (%result (sb-vm::%array-data result)))
    (declare (array-index m n p a-idx))

    (dotimes (i m)
      (declare (array-index i))
      (let ((result-idx-start (* i n)))

        (dotimes (k p)
          (declare (array-index k))

          (let* ((a-i-k (aref %a (+ a-idx k)))
                 (va-i-k (simd-vector a-i-k))
                 (result-idx result-idx-start)
                 (b-idx (* k n)))
            (declare (array-element a-i-k) (simd-vector va-i-k) (array-index result-idx b-idx))

            (do ((j 0 (+ j (* +unroll-simd+ +simd-size+))))
                ((> j (- n (* +unroll-simd+ +simd-size+)))
                 (do ((jj j (1+ jj)))
                     ((>= jj n))
                   (setf #2=(aref %result result-idx) (fmadd (aref %b b-idx) a-i-k #2#))
                   (incf result-idx)
                   (incf b-idx)))

              #.`(progn ,@(loop for u below +unroll-simd+
                                collect `(let ((tmpidx (+ (* ,u +simd-size+) result-idx)))
                                           (declare (array-index tmpidx))
                                           (setf #1=(simd-aref %result tmpidx)
                                                 (simd-fmadd (simd-aref %b (+ (* ,u +simd-size+) b-idx)) va-i-k #1#)))))

              (incf result-idx (* +unroll-simd+ +simd-size+))
              (incf b-idx (* +unroll-simd+ +simd-size+))))))
      (incf a-idx p))

    result))


;; similar to matrix-multiply-ikj-unrolled-blocked except: use SIMD
#+use-simd
(defun matrix-multiply-ikj-simd-unrolled-blocked (a b)
  "Performs matrix multiplication of two arrays."
  (declare (type (simple-array array-element 2) a b))
  (common-checks)

  (let* ((m (array-dimension a 0))
         (n (array-dimension b 1))
         (p (array-dimension a 1))
         (result (make-array (list m n) :element-type 'array-element
                                        :adjustable nil
                                        :fill-pointer nil
                                        :initial-element 0.0)))
    (declare (array-index m n p))

    (do ((iblock 0 (+ iblock +iblock-size+)))
        ((>= iblock m))
      (let ((ilimit (min m (+ iblock +iblock-size+))))
        (do ((jblock 0 (+ jblock +jblock-size+)))
            ((>= jblock n))
          (let ((jlimit (min n (+ jblock +jblock-size+))))

            (dotimes (k p)
              (declare (array-index k))

              (do ((i iblock (1+ i)))
                  ((>= i ilimit))
                (declare (array-index i))

                (let* ((a-i-k (aref a i k))
                       (va-i-k (simd-vector a-i-k))
                       (result-idx (array-row-major-index result i jblock))
                       (b-idx (array-row-major-index b k jblock)))
                  (declare (simd-vector va-i-k)
                           (array-element a-i-k) (array-index result-idx b-idx jlimit))

                  (do ((j jblock (+ j (* +unroll-simd+ +simd-size+))))
                      ((> j (- jlimit (* +unroll-simd+ +simd-size+)))
                       (do ((jj j (+ jj +simd-size+)))
                           ((> jj (- jlimit +simd-size+))
                            (loop repeat (- jlimit jj) do
                                  (setf #2=(row-major-aref result result-idx) (fmadd (row-major-aref b b-idx) a-i-k #2#))
                                  (incf result-idx)
                                  (incf b-idx)))

                         (setf #10=(simd-row-major-aref result result-idx)
                               (simd-fmadd (simd-row-major-aref b b-idx) va-i-k #10#))
                         (incf result-idx +simd-size+)
                         (incf b-idx +simd-size+)))

                    #.`(progn ,@(loop for u below +unroll-simd+
                                      collect `(let ((tmpidx (+ (* ,u +simd-size+) result-idx)))
                                                 (declare (array-index tmpidx))

                                                 (setf #1=(simd-row-major-aref result tmpidx)
                                                       (simd-fmadd (simd-row-major-aref b (+ (* ,u +simd-size+) b-idx)) va-i-k #1#)))))

                    (incf result-idx (* +unroll-simd+ +simd-size+))
                    (incf b-idx (* +unroll-simd+ +simd-size+))))))))))

    result))


#+use-simd
(defun matrix-multiply-ikj-gemm-simd-unrolled-column-blocked (a b)
  "Performs matrix multiplication of two arrays."
  (declare (type (simple-array array-element 2) a b))
  (common-checks)

  (let* ((m (array-dimension a 0))
         (n (array-dimension b 1))
         (p (array-dimension a 1))
         (result (make-array (list m n) :element-type 'array-element
                                        ;:adjustable nil
                                        :fill-pointer nil
                                        :initial-element 0.0)))
    (declare (array-index m n p))

    (do ((jblock 0 (+ jblock +jblock-size+)))
        ((>= jblock n))
      (declare (array-index jblock))

      (let ((jlimit (min +jblock-size+ (- n jblock))))
        (declare (array-index jlimit))

        (dotimes (i m)
          (declare (array-index i))

          (let ((result-idx-start (array-row-major-index result i jblock)))
            (declare (array-index result-idx-start))

            (do ((kblock 0 (+ kblock +kblock-size+)))
                ((>= kblock p))
              (declare (array-index kblock))

              (dotimes (koffset (min +kblock-size+ (- p kblock)))
                (declare (array-index koffset))

                (let* ((k (+ kblock koffset))
                       (a-i-k (aref a i k))
                       (va-i-k (simd-vector a-i-k))
                       (b-idx (array-row-major-index b k jblock))
                       (result-idx result-idx-start))
                  (declare (array-element a-i-k) (array-index k b-idx result-idx))

                  (do ((j 0 (+ j (* +unroll-simd+ +simd-size+))))
                      ((> j (- jlimit (* +unroll-simd+ +simd-size+)))
                       (do ((jj j (+ jj +simd-size+)))
                           ((> jj (- jlimit +simd-size+))
                            ;(do ((jjj jj (1+ jjj)))
                            ;    ((>= jjj jlimit))
                            ;  (setf #20=(row-major-aref result result-idx) (fmadd (row-major-aref b b-idx) a-i-k #20#))
                            ;  (incf result-idx)
                            ;  (incf b-idx)))
                            (loop repeat (- jlimit jj) do
                              (setf #2=(row-major-aref result result-idx) (fmadd (row-major-aref b b-idx) a-i-k #2#))
                              (incf result-idx)
                              (incf b-idx)))
                         (declare (array-index jj))

                         (setf #10=(simd-row-major-aref result result-idx)
                               (simd-fmadd (simd-row-major-aref b b-idx) va-i-k #10#))
                         (incf result-idx +simd-size+)
                         (incf b-idx +simd-size+)))
                    (declare (array-index j))

                    #.`(progn ,@(loop for u below +unroll-simd+
                                      collect `(setf #1=(simd-row-major-aref result (+ (* ,u +simd-size+) result-idx))
                                                     (simd-fmadd (simd-row-major-aref b (+ (* ,u +simd-size+) b-idx)) va-i-k #1#))))

                                      ;collect `(let ((tmpidx (+ (* ,u +simd-size+) result-idx)))
                                      ;           (declare (array-index tmpidx))
                                      ;           (setf #1=(simd-row-major-aref result tmpidx)
                                      ;                 (simd-fmadd (simd-row-major-aref b (+ (* ,u +simd-size+) b-idx)) va-i-k #1#)))))

                    (incf result-idx (* +unroll-simd+ +simd-size+))
                    (incf b-idx (* +unroll-simd+ +simd-size+))))))))))

    result))


;(setq SB-C::*compile-progress* t)
;(setq SB-C::*show-transforms-p* t)
;(setq SB-ASSEM::*show-peephole-transforms-p* t)

#+use-simd
(defun matrix-multiply-ikj-gemm-simd-vector-unrolled-column-blocked (a b)
  "Performs matrix multiplication of two arrays."
  (declare (type (simple-array array-element 2) a b))
  (common-checks)

  (let* ((m (array-dimension a 0))
         (n (array-dimension b 1))
         (p (array-dimension a 1))
         (result (make-array (list m n) :element-type 'array-element
                                        ;:adjustable nil
                                        :fill-pointer nil
                                        :initial-element 0.0))
         (%a (sb-vm::%array-data a))
         (%b (sb-vm::%array-data b))
         (%result (sb-vm::%array-data result)))
    (declare (array-index m n p))

    (do ((jblock 0 (+ jblock +jblock-size+)))
        ((>= jblock n))
      (declare (array-index jblock))

      (let ((a-idx 0)
            (jlimit (min +jblock-size+ (- n jblock))))
        (declare (array-index a-idx jlimit))

        (dotimes (i m)
          (declare (array-index i))

          (let ((result-idx-start (+ (the array-index (* i n)) jblock)))
            (declare (array-index result-idx-start))

            (do ((kblock 0 (+ kblock +kblock-size+)))
                ((>= kblock p))
              (declare (array-index kblock))
              #+(or)
              (format t "i=~3d, k=~3d..~3d, j=~3d..~3d~%"
                      i
                      kblock (+ kblock (min +kblock-size+ (- p kblock)) -1)
                      jblock (+ jblock (min +jblock-size+ (- n jblock)) -1))

              (dotimes (koffset (min +kblock-size+ (- p kblock)))
                (declare (array-index koffset))

                (let* ((k (+ kblock koffset))
                       (a-i-k (aref %a (+ a-idx k)))
                       (va-i-k (simd-vector a-i-k))
                       (b-idx (+ (the array-index (* k n)) jblock))
                       (result-idx result-idx-start))
                  (declare (array-element a-i-k) (array-index k b-idx result-idx))

                  #+(or)
                  (format t "j=~3d..~3d~%" jblock jlimit)
                  (do ((j 0 (+ j (* +unroll-simd+ +simd-size+))))
                      ((> j (- jlimit (* +unroll-simd+ +simd-size+)))
                       (do ((jj j (+ jj +simd-size+)))
                           ((> jj (- jlimit +simd-size+))
                            ;(do ((jjj jj (1+ jjj)))
                            ;    ((>= jjj jlimit))
                            ;  (setf #20=(aref %result result-idx) (fmadd (aref %b b-idx) a-i-k #20#))
                            ;  (incf result-idx)
                            ;  (incf b-idx)))
                            (loop repeat (- jlimit jj) do
                              #+(or)
                              (format t "cleanup loop: result-idx=~d~%" result-idx)
                              (setf #2=(aref %result result-idx) (fmadd (aref %b b-idx) a-i-k #2#))
                              (incf result-idx)
                              (incf b-idx)))

                         #+(or)
                         (format t "cleanup simd loop: jj=~d~%" jj)
                         (setf #10=(simd-aref %result result-idx)
                               (simd-fmadd (simd-aref %b b-idx) va-i-k #10#))
                         (incf result-idx +simd-size+)
                         (incf b-idx +simd-size+)))
                    (declare (array-index j))

                    #+(or)
                    (format t "unrolled simd loop: j=~d~%" j)
                    #.`(progn ,@(loop for u below +unroll-simd+
                                      ;collect `(setf #1=(simd-aref %result (+ (* ,u +simd-size+) result-idx))
                                      ;               (simd-fmadd (simd-aref %b (+ (* ,u +simd-size+) b-idx)) va-i-k #1#))))

                                      collect `(let ((tmpidx (+ (* ,u +simd-size+) result-idx)))
                                                 (declare (array-index tmpidx))
                                                 (setf #1=(simd-aref %result tmpidx)
                                                       (simd-fmadd (simd-aref %b (+ (* ,u +simd-size+) b-idx)) va-i-k #1#)))))

                    (incf result-idx (* +unroll-simd+ +simd-size+))
                    (incf b-idx (* +unroll-simd+ +simd-size+)))))))

          (incf a-idx p))))

    result))

;(setq SB-ASSEM::*show-peephole-transforms-p* nil)
;(setq SB-C::*show-transforms-p* nil)
;(setq SB-C::*compile-progress* nil)


;; try a third level of blocking which is not really needed and makes things slower
#+use-simd
(defun matrix-multiply-ikj-simd-unrolled-blocked2 (a b)
  "Performs matrix multiplication of two arrays."
  (declare (type (simple-array array-element 2) a b))
  (common-checks)

  (let* ((m (array-dimension a 0))
         (n (array-dimension b 1))
         (p (array-dimension a 1))
         (result (make-array (list m n) :element-type 'array-element
                                        :adjustable nil
                                        :fill-pointer nil
                                        :initial-element 0.0)))
    (declare (array-index m n p))

    (do ((kblock 0 (+ kblock +kblock-size+)))
        ((>= kblock p))
      (do ((iblock 0 (+ iblock +iblock-size+)))
          ((>= iblock m))
        (do ((jblock 0 (+ jblock +jblock-size+)))
            ((>= jblock n))

          (do ((k kblock (1+ k)))
              ((>= k (min p (+ kblock +kblock-size+))))
            (declare (array-index k))

            (do ((i iblock (1+ i)))
                ((>= i (min m (+ iblock +iblock-size+))))
              (declare (array-index i))

              (let ((a-i-k (aref a i k))
                    (result-idx (array-row-major-index result i 0))
                    (b-idx (array-row-major-index b k 0)))
                (declare (array-element a-i-k) (array-index result-idx b-idx))

                (do ((j jblock (+ j (* 4 +simd-size+))))
                    ((> j (min (- n (* 4 +simd-size+)) (+ jblock +jblock-size+)))
                     (do ((jj j (1+ jj)))
                         ((>= jj (min (- n +simd-size+) (+ jblock +jblock-size+)))
                          (do ((jjj jj (1+ jjj)))
                              ((>= jjj (min n (+ jblock +jblock-size+))))
                            (incf (row-major-aref result result-idx) (* a-i-k (row-major-aref b b-idx)))
                            (incf result-idx)
                            (incf b-idx)))

                       (setf #10=(simd-row-major-aref result result-idx)
                             (simd-+ #10# (simd-* (simd-row-major-aref b b-idx) a-i-k)))
                       (incf result-idx +simd-size+)
                       (incf b-idx +simd-size+)))

                  (setf #1=(simd-row-major-aref result result-idx)
                        (simd-+ #1# (simd-* (simd-row-major-aref b b-idx) a-i-k)))

                  (setf #2=(simd-row-major-aref result (+ +simd-size+ result-idx))
                        (simd-+ #2# (simd-* (simd-row-major-aref b (+ +simd-size+ b-idx)) a-i-k)))

                  (setf #3=(simd-row-major-aref result (+ (* 2 +simd-size+) result-idx))
                        (simd-+ #3# (simd-* (simd-row-major-aref b (+ (* 2 +simd-size+) b-idx)) a-i-k)))

                  (setf #4=(simd-row-major-aref result (+ (* 3 +simd-size+) result-idx))
                        (simd-+ #4# (simd-* (simd-row-major-aref b (+ (* 3 +simd-size+) b-idx)) a-i-k)))

                  (incf result-idx (* 4 +simd-size+))
                  (incf b-idx (* 4 +simd-size+)))))))))

    result))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

;; comment the next line to run a basic test
#+mmult-test
(let ((x (matrix-multiply-ikj (make-array '(2 3) :element-type 'array-element
                                                 :initial-contents '((1.0 2.0 3.0) (4.0 5.0 6.0)))
                              (make-array '(3 2) :element-type 'array-element
                                                 :initial-contents '((5.0 6.0) (7.0 8.0) (9.0 10.0))))))
  ;(print x)
  (unless (equalp x #2A((46.0 52.0) (109.0 124.0)))
    (error "d'oh")))

(defun random-array (x y)
  (declare (array-index x y))
  (let ((a (make-array (list x y) :element-type 'array-element :adjustable nil :fill-pointer nil)))
    (dotimes (i x)
      (dotimes (j y)
        (setf (aref a i j) (- 10.0 (random 20.0)))))
    a))

(defun equal-ish (a b)
  (declare (type (simple-array array-element 2) a b))
  (let ((x (array-dimension a 0))
        (y (array-dimension a 1)))
    (dotimes (i x)
      (dotimes (j y)
        ;(format t "a(~d, ~d): ~f, b(~d, ~d): ~f~%" i j (aref a i j) i j (aref b i j))
        (unless #-fma (= (aref a i j) (aref b i j))
                #+fma (< (abs (- (aref a i j) (aref b i j))) 0.01)
          (return-from equal-ish nil)))))
  t)


;; comment the next line to run tests
#+mmult-test
(dolist (f '(matrix-multiply-ijk-gemm
             matrix-multiply-ijk-gemm-opt
             matrix-multiply-ikj-gemm-blocked
             matrix-multiply-ikj-gemm-blocked-opt
             matrix-multiply-ikj-gemm-column-blocked
             matrix-multiply-ijk-blocked

             matrix-multiply-ikj
             matrix-multiply-ikj-opt
             matrix-multiply-ikj-unrolled
             matrix-multiply-ikj-blocked
             matrix-multiply-ikj-unrolled-blocked

             matrix-multiply-ikj-vector
             matrix-multiply-ikj-vector-unrolled

             #+use-simd matrix-multiply-ikj-simd
             #+use-simd matrix-multiply-ikj-simd-unrolled
             #+use-simd matrix-multiply-ikj-simd-vector-unrolled
             #+use-simd matrix-multiply-ikj-simd-blocked
             #+use-simd matrix-multiply-ikj-simd-unrolled-blocked
             #+use-simd matrix-multiply-ikj-gemm-simd-unrolled-column-blocked
             #+use-simd matrix-multiply-ikj-gemm-simd-vector-unrolled-column-blocked))
  (dolist (x '(15 16 17 #|1535 1536 1537|#))
    (dolist (y '(1535 1536 1537))
      ;(format t "testing ~a (~dx~d) x (~dx~d)~%" f x y y x)
      #1=(let* ((a (random-array x y))
                (b (random-array y x))
                (ref (matrix-multiply-ikj a b))
                (tst (funcall f a b)))
           (unless (equal-ish ref tst)
             (format t "~%!!! ERROR: ~a (~dx~d) x (~dx~d) produces wrong result~%~%" f x y y x)))))

  (dotimes (xx 33)
    (dotimes (yy 65)
      (let ((x (1+ xx))
            (y (1+ yy)))
        #1#))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; timed runs


(defun arry (dimspec init)
  "Create a simple-array and set its contents to 'init'."
  (make-array dimspec
              :element-type 'array-element
              :initial-element init))


(defun mmult (func m p &optional (n m))
  (format t "~a (~dx~d) by (~dx~d)~%" func m p p n)
  (disassemble func)

  (let ((a (arry (list m p) 1.0))
        (b (arry (list p n) 2.0))
        (f (symbol-function func)))
    #-mmult-test (sleep 5)
    (time (funcall f a b))

    #-mmult-test (sleep 5)
    #-mmult-test (time (funcall f a b))

    #-mmult-test (sleep 5)
    #-mmult-test (time (funcall f a b))))




#+(or)
(defun mmult10 (func m p &optional (n m) (sleep-p t))
  (format t "~a (~dx~d) by (~dx~d)~%" func m p p n)
  #-mmult-test (when sleep-p (sleep 5))
  (let ((a (arry (list m p) 1.0))
        (b (arry (list p n) 2.0))
        (f (symbol-function func)))
    (time (loop repeat 10 do (funcall f a b)))))


(defun mmult10 (func m p &optional (n m) (sleep-p t))
  (format t "~a (~dx~d) by (~dx~d)~%" func m p p n)
  #-mmult-test (when sleep-p (sleep 5))
  (let ((a (arry (list m p) 1.0))
        (b (arry (list p n) 2.0))
        (f (symbol-function func))
        (tstart (get-internal-real-time)))
    (loop repeat 10 do (funcall f a b))
    (format t "~g seconds~%" (/ (- (get-internal-real-time) tstart) INTERNAL-TIME-UNITS-PER-SECOND))))


;                                                              ;  sbcl-2.2.9/ sbcl-2.3.4
;(mmult 'matrix-multiply-ikj 1000 1000)                        ;  1.84s/ 1.552s
;(mmult 'matrix-multiply-ikj-opt 1000 1000)                    ;  0.775s/ 0.763s
;(mmult 'matrix-multiply-ikj-unrolled 1000 1000)               ;  0.729s/ 0.690s
;(mmult 'matrix-multiply-ikj-blocked 1000 1000)                ;  0.827s/ 0.846s
;(mmult 'matrix-multiply-ikj-unrolled-blocked 1000 1000)       ;  0.903s/ 0.777s

;(mmult 'matrix-multiply-ijk-gemm 1000 1000)                    ;  1.182s/ 1.257s
;(mmult 'matrix-multiply-ijk-gemm-opt 1000 1000)                ;  1.026s/ 1.044s
;(mmult 'matrix-multiply-ikj-gemm-blocked 1000 1000)            ;  2.347s/ 2.298s
;(mmult 'matrix-multiply-ikj-gemm-blocked-opt 1000 1000)        ;  0.925s/ 0.874s
;(mmult 'matrix-multiply-ikj-gemm-column-blocked 1000 1000)     ;  0.807s/ 0.804s
;(mmult 'matrix-multiply-ijk-blocked 1000 1000)                 ;  0.999s/ 0.988s

;(mmult 'matrix-multiply-ikj-vector 1000 1000)                 ;  0.521s/ 0.534s
;(mmult 'matrix-multiply-ikj-vector-unrolled 1000 1000)        ;  0.447s/ 0.448s


#+use-simd
(progn
;(mmult 'matrix-multiply-ikj-simd 1000 1000)                   ;  0.167s/ 0.170s
;(mmult 'matrix-multiply-ikj-simd-unrolled 1000 1000)          ;  0.160s/ 0.148s
;(mmult 'matrix-multiply-ikj-simd-vector-unrolled 1000 1000)   ;  0.109s/ 0.113s
;(mmult 'matrix-multiply-ikj-simd-blocked 1000 1000)           ;  0.158s/ 0.156s
;(mmult 'matrix-multiply-ikj-simd-unrolled-blocked 1000 1000)  ;  0.154s/ 0.133s
;(mmult 'matrix-multiply-ikj-gemm-simd-unrolled-column-blocked 1000 1000) ; 0.171s/ 0.177s
(mmult 'matrix-multiply-ikj-gemm-simd-vector-unrolled-column-blocked 1000 1000) ; 0.120s/ 0.121s
;(mmult 'matrix-multiply-ikj-gemm-simd-vector-unrolled-column-blocked 20 20)
)


;(mmult 'matrix-multiply-ikj 5000 2000)                        ; 84.902s
;(mmult 'matrix-multiply-ikj-opt 5000 2000)                    ; 46.415s
;(mmult 'matrix-multiply-ikj-unrolled 5000 2000)               ; 40.123s
;(mmult 'matrix-multiply-ikj-blocked 5000 2000)                ; 42.022s
;(mmult 'matrix-multiply-ikj-unrolled-blocked 5000 2000)       ; 38.968s
#+use-simd
(progn
;(mmult 'matrix-multiply-ikj-simd 5000 2000)                   ; 16.019s
;(mmult 'matrix-multiply-ikj-simd-unrolled 5000 2000)          ; 15.519s
;(mmult 'matrix-multiply-ikj-simd-vector-unrolled 5000 2000)   ; 10.821s
;(mmult 'matrix-multiply-ikj-simd-blocked 5000 2000)           ;  9.618s
;(mmult 'matrix-multiply-ikj-simd-unrolled-blocked 5000 2000)  ;  9.804s
;(mmult 'matrix-multiply-ikj-gemm-simd-vector-unrolled-column-blocked 5000 2000) ; 10.057s
)


;(mmult 'matrix-multiply-ikj 2000 5000)                         ; 36.055
;(mmult 'matrix-multiply-ikj-opt 2000 5000)                     ; 17.820s
;(mmult 'matrix-multiply-ikj-unrolled 2000 5000)                ; 16.162s
;(mmult 'matrix-multiply-ikj-blocked 2000 5000)                 ; 17.853s
;(mmult 'matrix-multiply-ikj-unrolled-blocked 2000 5000)        ; 15.578s
;(mmult 'matrix-multiply-ikj-gemm-blocked-opt 2000 5000)        ; 34s/ 34s
;(mmult 'matrix-multiply-ijk-blocked 2000 5000)
#+use-simd
(progn
;(mmult 'matrix-multiply-ikj-simd 2000 5000)                    ;  5.2s/ 5.234s
;(mmult 'matrix-multiply-ikj-simd-unrolled 2000 5000)           ;  5.107s/ 4.951s
;(mmult 'matrix-multiply-ikj-simd-blocked 2000 5000)            ;  4.474s/ 4.513s
;(mmult 'matrix-multiply-ikj-simd-unrolled-blocked 2000 5000)   ;  4.328s/ 3.952s
;(mmult 'matrix-multiply-ikj-gemm-simd-vector-unrolled-column-blocked 2000 5000) ; 7.774s/ 7.783s
)



#|
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "magicl/core" :silent t))

(defun magicl-mmult (m p &optional (n m))
  (format t "magicl (~dx~d) by (~dx~d)~%" m p p n)
  (let ((a (magicl:const 1.0 (list m p)))
        (b (magicl:const 2.0 (list p n)))
        (c (magicl:const 0.0 (list m n))))

    (sleep 5)
    (time (magicl:@ a b))

    (sleep 5)
    (time (magicl:mult a b))

    (sleep 5)
    (time (magicl:mult a b :target c))
    ))


(magicl-mmult 1000 1000) ; 1.773s
|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; timed series runs
;;
;; (m x p) x (p x n) -> (m x n)

(defun mmult2 (func m p &optional (n m))
  (declare (array-index m p n))
  (let* ((start (get-internal-real-time))
         (end (+ start INTERNAL-TIME-UNITS-PER-SECOND))
         (a (arry (list m p) 1.0))
         (b (arry (list p n) 2.0))
         (rep (ceiling 50000 m))
         (array-elements (+ (the array-index (* m p)) (the array-index (* p n)) (the array-index (* m n))))
         (kbytes (truncate array-elements 256)))
    (declare (array-index m p n) (array-index array-elements kbytes))
    (loop :for now = (get-internal-real-time)
          :while (< now end)
          :do (loop repeat rep do (funcall (symbol-function func) a b))
          :sum rep :into count of-type fixnum
          :finally (format t "~a (~dx~d) = (~dx~d) x (~dx~d), using ~dkB: ~,3fms~%"
                           func m n m p p n kbytes (/ (- now start) (/ INTERNAL-TIME-UNITS-PER-SECOND 1000) count)))))

;; doubling m, p and n with each step -> time will increase 8-fold
;(format t "time should increase with N^3~%")
;(dolist (x '(100 200 400 800)) (mmult2 'matrix-multiply-ikj-simd-unrolled-blocked x x))

;; doubling m with each step -> time will increase 4-fold
;(format t "time should increase with N^2~%")
;(dolist (x '(100 200 400 800)) (mmult2 'matrix-multiply-ikj-simd-unrolled-blocked x 100))

;; doubling p with each step -> time will double
;(format t "time should increase with N~%")
;(dolist (x '(100 200 400 800)) (mmult2 'matrix-multiply-ikj-simd-unrolled-blocked 100 x))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; profiling stuff


#+(and sbcl :X86-64 mmult-profile)
(require :sb-sprof)

#+(and sbcl :X86-64 mmult-profile (not win32))
(let ((a (arry '(3000 1000) 1.0))
      (b (arry '(1000 3000) 2.0)))

  (sb-sprof:with-profiling (:mode :cpu :loop t :max-samples 1000 :report :flat :sample-interval 0.1)
    (loop repeat 10
          do (matrix-multiply-ikj-gemm-simd-vector-unrolled-column-blocked a b))))


;(disassemble 'matrix-multiply-ijk-gemm)
;(disassemble 'matrix-multiply-ijk-gemm-opt)
;(disassemble 'matrix-multiply-ikj-gemm-blocked)
;(disassemble 'matrix-multiply-ikj-gemm-blocked-opt)
;(disassemble 'matrix-multiply-ikj-gemm-column-blocked)
;(disassemble 'matrix-multiply-ikj-gemm-simd-unrolled-column-blocked)
;(disassemble 'matrix-multiply-ikj-gemm-simd-vector-unrolled-column-blocked)

;(disassemble 'matrix-multiply-ijk-blocked)

;(disassemble 'matrix-multiply-ikj-opt)
;(disassemble 'matrix-multiply-ikj-unrolled)
;(disassemble 'matrix-multiply-ikj-blocked)
;(disassemble 'matrix-multiply-ikj-unrolled-blocked)

;(disassemble 'matrix-multiply-ikj-vector)
;(disassemble 'matrix-multiply-ikj-vector-unrolled)

;(disassemble 'matrix-multiply-ikj-simd)
;(disassemble 'matrix-multiply-ikj-simd-unrolled)
;(disassemble 'matrix-multiply-ikj-simd-vector-unrolled)
;(disassemble 'matrix-multiply-ikj-simd-unrolled-blocked)


;; entry point for
;; sbcl --load mmult-simd.lisp --eval "(save-lisp-and-die (format nil \"mmult-sbcl-~d-~d\.exe" +kblock-size+ +jblock-size+) :executable t :toplevel 'main)"
(defun main ()
  (mmult10 'matrix-multiply-ikj-gemm-simd-vector-unrolled-column-blocked 1000 1000 1000 nil))
;  (mmult10 'matrix-multiply-ikj 1000 1000 1000 nil))
