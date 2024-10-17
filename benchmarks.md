# LISP Compiler Benchmarks

## Use

```lisp
(proclaim '(optimize (speed 3) (safety 0) (space 0) (debug 0)))
```

Compile function to achieve TCO, not needed for *SBCL*:

```lisp
(compile 'harmonic-tr)
```


## Code

Untyped:

```lisp
 (defun harmonic-tr (n accum)
  "tail-recursive harmonic function"
  (if (<= n 1) (+ accum 1)
      (harmonic-tr (- n 1) (+ accum (/ 1.0 n)))))
 (time (loop for i from 1 to 10 collect (harmonic-tr 1000000 0.0)))
```

Typed:

```lisp
(defun harmonic-tr (n accum)
  "tail-recursive harmonic function"
  (declare (type (signed-byte 32) n)
           (type single-float accum)
           (optimize (speed 3)))
  (if (<= n 1) (+ accum 1)
      (harmonic-tr (- n 1) (+ accum (/ 1.0 n)))))
(time (loop for i from 1 to 10 collect (harmonic-tr (+ 1000000 i) 0.0)))
```

In both the C++ version and the LISP version the function is called
each time with different parameters to avoid optimisations.


## Architecture

MacOS/M1

## C++

`clang++ -O3`

```
0.018 seconds
```

## ECL 24.5.10


Untyped:

```
real time : 1.033 secs
run time  : 1.229 secs
gc count  : 30 times
consed    : 320004000 bytes
```

Typed:

```
real time : 0.337 secs
run time  : 0.336 secs
gc count  : 1 times
consed    : 256 bytes
```

## SBCL 2.4.9


Untyped:

```
0.134 seconds of real time
0.134515 seconds of total run time (0.134223 user, 0.000292 system)
100.75% CPU
0 bytes consed
```

Typed:

```
0.017 seconds of real time
0.017529 seconds of total run time (0.017481 user, 0.000048 system)```
105.88% CPU
0 bytes consed
```

## Allegro CL 11.0

Untyped:

```
cpu time (non-gc) 0.304227 sec user, 0.000689 sec system
cpu time (gc)     0.663654 sec user, 0.001580 sec system
cpu time (total)  0.967881 sec user, 0.002269 sec system
real time  0.970568 sec (99.96%)
space allocation:
 10 cons cells, 959,999,360 other bytes, 0 static bytes
Page Faults: major: 0 (gc: 4), minor: 4 (gc: 4)

```

When typed and safety is zero passing 0 instead of 0.0 as
the initial accumulation value results in a segfault.

Typed:

```
cpu time (non-gc) 0.029728 sec user, 0.000125 sec system
cpu time (gc)     0.000000 sec user, 0.000000 sec system
cpu time (total)  0.029728 sec user, 0.000125 sec system
real time  0.029968 sec (99.62%)
space allocation:
 10 cons cells, 320 other bytes, 0 static bytes
Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)

```
