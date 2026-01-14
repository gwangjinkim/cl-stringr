# cl-stringr

`cl-stringr` is a Common Lisp port of R's iconic `stringr` package. It provides a consistent, simple, and powerful API for string manipulation, designed to be the backbone of the `cl-tidyverse` ecosystem.

---

## For Common Lisp Developers: Why use this?

If you are a seasoned Lisp developer, you might ask: *"Common Lisp already has `cl-ppcre` and string functions. Why do I need this?"*

**The Problem:** Traditional string manipulation often involves manual loops, mapcar calls, and defensive `if (null ...)` checks to avoid errors on missing data.

**The Solution:** `cl-stringr` abstracts away the boilerplate. It introduces **Vectorization** and **NA-Awareness**.

### 1. Vectorization: Stop Thinking in Loops
Instead of mapping over a list of strings, you pass the whole vector to a `str-` function. It handles the iteration for you.

```lisp
;; Traditional Lisp
(map 'vector (lambda (s) (when s (string-trim " " s))) #("  a " nil " b  "))

;; With cl-stringr
(str-trim #("  a " nil " b  "))
;; => #("a" nil "b")
```

### 2. NA-Awareness: The Power of `nil`
In data science, missing data is common. `cl-stringr` treats `nil` as "NA" (Not Available). Any operation on `nil` returns `nil`, rather than crashing or requiring an extra check. This allows you to chain operations safely.

---

## Quick Reference for R / Tidyverse Users

If you know `stringr`, you already know `cl-stringr`. The mapping is direct:

| stringr (R) | cl-stringr (CL) |
| :--- | :--- |
| `str_length(x)` | `(str-length x)` |
| `str_c(x, y)` | `(str-c x y)` |
| `str_detect(x, pat)` | `(str-detect x pat)` |
| `str_replace_all(x, pat, rep)` | `(str-replace-all x pat rep)` |
| `str_to_upper(x)` | `(str-to-upper x)` |
| `str_to_title(x)` | `(str-to-title x)` |
| `str_glue("hi {var}")` | `(str-glue "hi {var}")` |

*Note: All functions take the subject string (or vector) as the **first** argument, making them pipe-friendly.*

---

## Deep Dive & Tutorial

### 1. Advanced String Interpolation with `str-glue`
`str-glue` is a modern alternative to `format`. It uses `{}` to interpolate variables directly.

#### Simple usage:
```lisp
(str-glue "Hello {name}, you have {count} messages." :name "Alice" :count 5)
;; => #("Hello Alice, you have 5 messages.")
```

#### The `with-str-context` Power-Up
When you have many variables, providing them as keywords to `str-glue` can be tedious. `with-str-context` creates an environment where a local `glue` function automatically "sees" your variables.

```lisp
(defvar *user-name* "Josephus")
(defvar *status* "Online")

(with-str-context (name *user-name* status *status*)
  ;; The local 'glue' function automatically knows about 'name' and 'status'
  (glue "[{status}] User: {name}"))
;; => #("[Online] User: Josephus")
```
It can also take a plist-style list:
```lisp
(with-str-context (:x 10 :y 20)
  (glue "Point: ({x}, {y})"))
;; => #("Point: (10, 20)")
```

---

### 2. Pattern Matching & Extraction
`cl-stringr` wraps the high-performance `cl-ppcre` engine.

- **`str-detect`**: Returns a boolean vector. Great for filtering.
- **`str-extract`**: Pulls out the first matching substring.
- **`str-extract-all`**: Returns a vector of vectors containing all matches.

```lisp
(defvar *files* #("data_2023.csv" "report_v1.pdf" "summary_2024.csv"))

;; Find all files from 2023 or 2024
(str-subset *files* "202[34]")
;; => #("data_2023.csv" "summary_2024.csv")

;; Extract the year as a number
(map-str #'parse-integer (str-extract *files* "\\d{4}"))
;; => #(2023 nil 2024)  ; report_v1 has no year, returns nil safely!
```

---

### 3. Mass Replacement
Clean up strings across a whole dataset with one call.

```lisp
(str-replace-all #("apple" "banana") "[aeiou]" "_")
;; => #("_ppl_" "b_n_n_")
```

---

### 4. Case Conversion
`cl-stringr` provides several functions to change the case of strings, mapping directly to R's `str_to_*` family.

```lisp
(defvar *names* #("albert einstein" "MARIE CURIE" nil))

(str-to-title *names*)
;; => #("Albert Einstein" "Marie Curie" nil)

(str-to-upper "hello")
;; => #("HELLO")

(str-to-sentence "THIS IS A TEST")
;; => #("This is a test")
```

---

### 5. Splitting and Joining
Easily break strings apart or sew them back together.

```lisp
(str-split #("a,b,c" "d,e") ",")
;; => #(#("a" "b" "c") #("d" "e"))

(str-join #("apple" "banana" "cherry") ", ")
;; => "apple, banana, cherry"

;; Handles NA (nil) gracefully
(str-join #("apple" nil "cherry") ", ")
;; => nil  ; Following stringr: if any element is NA, the join is NA
```

---

### 6. Integration with `cl-dplyr`
`cl-stringr` is designed to be used inside `cl-dplyr` verbs for seamless data cleaning.

```lisp
(-> df
  (mutate :clean-name (str-trim (str-to-title :raw-name)))
  (filter (str-detect :clean-name "John")))
```

---

## Full API Reference

### Basics
- `str-length (s)`: Returns length for each string.
- `str-c (&rest strings)`: Concatenates vectors element-wise.
- `str-sub (s start &optional end)`: Vectorized `subseq`.
- `str-trim (s)`: Vectorized whitespace trim.
- `str-to-upper (s)`: All uppercase.
- `str-to-lower (s)`: All lowercase.
- `str-to-title (s)`: Title case (Capitalize Each Word).
- `str-to-sentence (s)`: Sentence case (Capitalize first letter, rest lower).

### Pattern Matching
- `str-detect (s pattern)`: Boolean match.
- `str-subset (s pattern)`: Filter to matching elements.
- `str-count (s pattern)`: Count occurrences.
- `str-extract (s pattern)`: Extract first match.
- `str-extract-all (s pattern)`: Extract all matches.

### Mutation
- `str-replace (s pattern replacement)`: Replace first match.
- `str-replace-all (s pattern replacement)`: Replace all matches.

### Splitting/Joining
- `str-split (s pattern)`: Split into vectors of pieces.
- `str-join (s &optional (collapse ""))`: Collapse vector into one string.

### Interpolation & DSL
- `str-glue (template &rest data)`: Variable interpolation.
- `with-str-context (bindings &body body)`: Macro for cleaner templates.
- `map-str (fn vector)`: Mapping utility that handles `nil`.
- `filter-str (pattern vector)`: Alias for `str-subset`.

---

## 🚶 Implementation Walkthrough
`cl-stringr` is engineered for performance and consistency. It uses a core `vmap` utility that ensures near-native speed for vectorized operations while maintaining strict R-compatible behavior for missing values.

It is a member of the **[cl-tidyverse](https://github.com/cl-tidyverse)** collection, intended to provide Common Lisp with the same expressive power for data manipulation as the R ecosystem.

---

## License
MIT
