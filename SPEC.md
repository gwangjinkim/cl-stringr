# SPEC.md: cl-stringr Specification

## Overview
`cl-stringr` is a Common Lisp port of R's `stringr` package. It provides a consistent, simple, and easy-to-use string manipulation library, primarily wrapping `cl-ppcre`.

## Philosophy
1.  **Consistency**: All functions start with the `str-` prefix.
2.  **Vectorization**: Functions follow a "vector-in, vector-out" pattern.
3.  **Pipe-friendly**: The primary string (or vector of strings) is always the first argument.
4.  **NA Handling**: Operations on `nil` (NA) return `nil`.
5.  **Regex Engine**: `cl-ppcre` is used for all pattern matching.

## Data Types
- **String Vector**: A Common Lisp `vector` (or `array`) of strings.
- **NA**: Represented as `nil`.

## Core API

### Basic Operations
- `str-length (string-vector)` -> `vector-of-integers`
  - Returns the number of characters in each string.
- `str-sub (string-vector, start, end)` -> `vector-of-strings`
  - Extracts substrings from each string.
- `str-c (&rest string-vectors)` -> `vector-of-strings`
  - Concatenates multiple string vectors element-wise.
- `str-trim (string-vector)` -> `vector-of-strings`
  - Removes leading and trailing whitespace.

### Pattern Matching
- `str-detect (string-vector, pattern)` -> `vector-of-booleans`
  - Detects the presence of a pattern.
- `str-subset (string-vector, pattern)` -> `vector-of-strings`
  - Returns strings that match the pattern.
- `str-count (string-vector, pattern)` -> `vector-of-integers`
  - Counts the number of times a pattern appears.
- `str-extract (string-vector, pattern)` -> `vector-of-strings`
  - Extracts the first match.
- `str-extract-all (string-vector, pattern)` -> `vector-of-vectors`
  - Extracts all matches.

### Mutation
- `str-replace (string-vector, pattern, replacement)` -> `vector-of-strings`
  - Replaces the first match.
- `str-replace-all (string-vector, pattern, replacement)` -> `vector-of-strings`
  - Replaces all matches.

### Splitting/Joining
- `str-split (string-vector, pattern)` -> `vector-of-vectors`
  - Splits each string into pieces.
- `str-join (string-vector, collapse)` -> `string`
  - Joins elements of a vector into a single string.

### Templates
- `str-glue (template-string-vector, &rest data)` -> `vector-of-strings`
  - Interpolates variables into strings. Uses `{}` syntax similar to Python/stringr.

## NA Handling Policy
If any input in a vectorized operation is `nil`, the corresponding output element must be `nil`.
For example:
- `(str-length #("a" nil "bc"))` -> `#(1 nil 2)`
- `(str-c #("f" nil) #("oo" "bar"))` -> `#("foo" nil)`

## Lisp DSL Layer
To make it feel more "Lispy", we will provide:
- `with-str-context`: A macro to bind local variables for `str-glue`.
- Reader macros (optional): e.g., `#s"template {var}"`.
- Functional helpers: `map-str`, `filter-str`.

## Dependencies
- `cl-ppcre`: For regex matching and replacement.
- `alexandria`: For general utility functions.
- `cl-vctrs-lite`: For vector operations/NA handling if applicable.
