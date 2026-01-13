# cl-stringr

Common Lisp port of R's `stringr`. A consistent, simple, and easy-to-use string manipulation library wrapper around `cl-ppcre`.

## Philosophy
- **Prefix**: All core functions start with `str-`.
- **Vectorized**: Functions take vectors and return vectors (vector-in, vector-out).
- **NA Handling**: `nil` is treated as NA. Operations on `nil` return `nil`.

## Installation
Currently in development as part of `cl-tidyverse`.

## Core API

### Basics
- `str-length (s)`
- `str-c (&rest strings)`
- `str-sub (s start &optional end)`
- `str-trim (s)`

### Pattern Matching
- `str-detect (s pattern)`
- `str-subset (s pattern)`
- `str-count (s pattern)`
- `str-extract (s pattern)`
- `str-extract-all (s pattern)`

### Mutation
- `str-replace (s pattern replacement)`
- `str-replace-all (s pattern replacement)`

### Splitting/Joining
- `str-split (s pattern)`
- `str-join (s &optional collapse)`

### Interpolation
- `str-glue (template &rest data)`

## Lisp DSL
- `with-str-context`: A macro to bind local variables for interpolation.
- `map-str`: Functional mapping over string vectors with NA handling.

## Example
```lisp
(use-package :cl-stringr)

(str-length #("hello" nil "world"))
;; => #(5 nil 5)

(str-c #("f" nil) #("oo" "bar"))
;; => #("foo" nil)

(str-glue "Hello {name}!" :name "Alice")
;; => #("Hello Alice!")
```
