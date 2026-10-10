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

### 2. Missing values
Use `cl-vctrs-lite:*na*` for missing values in shared typed columns.
Legacy character inputs also accept NIL as a missing marker. In an
unambiguous logical column NIL means FALSE and stringifies as "FALSE";
the SR2 section below describes those compatibility boundaries.

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
`cl-stringr` uses the pinned native ICU engine for pattern matching.

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
;; => #(2023 cl-vctrs-lite:*na* 2024)  ; missing matches retain NA
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

## Native pattern engine (SR1)

Pattern operations use an independent ICU **71.1** engine, matching the pinned
R stringi build's Unicode **14.0** data. They do not load R or stringi at runtime.
Install the native dependency once, in the foreground:

```sh
scripts/bootstrap-icu.sh
```

This downloads the official Unicode ICU 71.1 source, verifies SHA-256
`67a7e6e51f61faf1306b6935333e13b2c48abd8da6d2f46ce6adca24b1e21ebf`,
and builds into ignored `.native/icu-71.1/`. The script accepts an alternative
installation prefix as its first argument. `CL_STRINGR_ICU_JOBS` controls
build concurrency; `CL_STRINGR_ICU_TARBALL` selects a cached source archive.
The original dependency license is in `docs/ICU-LICENSE.txt`.

The runtime supports POSIX `dlopen`/`dlsym` on macOS and Linux. It prefers the
local installation; `CL_STRINGR_ICU_LIBRARY` can select an independent ICU
71.1 internationalization library elsewhere. Missing libraries or incompatible
versions produce an actionable `pattern-error`. Symbol resolution is scoped to
the selected library: unrelated loaded ICU versions cannot affect matching.
Windows native loading is not implemented. Newer ICU versions can recognize
characters that pinned R treats as unassigned, so they are rejected rather
than silently changing results.

| Function | Behavior |
|---|---|
| `regex` | ICU regex objects; case, multiline, comments, dotall, Unicode word rules, literal mode, limits and native option aliases |
| `fixed` | Literal objects; ICU simple uppercase comparison when ignoring case, distinct from regex full folding |
| `coll` | ICU Unicode collation objects, canonical equivalence, locale, strength and collator options |
| `boundary` | ICU character, word, line-break and sentence segments, including skip options |
| `str-escape` | Escape regex metacharacters, retaining names, NA and empty string prototypes |

All existing pattern consumers accept modifier vectors and strict scalar
recycling. Empty and all-NA outputs retain `:bool`, `:int`, `:string` or `:list`
prototypes. ICU UTF-16 positions are converted to Lisp character positions,
including astral characters and combining sequences. Explicit empty modifier
patterns warn and return missing results; bare empty text denotes character
boundaries. Detection, subset and replacement reject boundary patterns.
`fixed`/`coll` replacements are literal; regex capture references use backslash
digits. Pattern errors/warnings subclass the shared tidy condition contract.

Numeric/logical inputs use R-style character coercion; factors use shared
column labels. Date/datetime owner columns format UTC values, retaining
fractional seconds and the pinned R per-cell midnight abbreviation. HMS time
columns retain aligned hours and common fractional precision; nonfinite
numeric values coerce to `Inf`, `-Inf` and `NaN` text. Constructor
coercion warns, including typed empty non-character inputs. Pattern values,
options and optional names are available through the exported pattern protocol;
`pattern-matches-p` supplies native scalar matching to tidyselect integrations.
`pattern-scanner` is only a legacy CL-PPCRE adapter and cannot express all native
ICU semantics.

`str-equal` provides Unicode canonical equality and collation options as required
by the SR1 plan text; its complete later milestone certification remains open.
SR2 now completes detection, location and capture matching as described below.
Replacement callback/named mapping forms and split :n/:simplify remain later
milestone work, with explicit partial status in the umbrella inventory.

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

## Implementation Walkthrough
`cl-stringr` is engineered for performance and consistency. It uses a core `vmap` utility that ensures near-native speed for vectorized operations while maintaining strict R-compatible behavior for missing values.

It is a member of the **[cl-tidyverse](https://github.com/cl-tidyverse)** collection, intended to provide Common Lisp with the same expressive power for data manipulation as the R ecosystem.

---

## License
MIT

## Detection, locations and captures (SR2)

| R | Common Lisp |
|---|---|
| `str_detect`, `str_subset`, `str_which` | `str-detect`, `str-subset`, `str-which` (`:negate`) |
| `str_starts`, `str_ends` | `str-starts`, `str-ends` (`:negate`) |
| `str_count` | `str-count` (default Unicode graphemes) |
| `str_extract`, `str_extract_all` | `str-extract` (`:group`), `str-extract-all` (`:simplify`) |
| `str_locate`, `str_locate_all` | `str-locate`, `str-locate-all` |
| `str_match`, `str_match_all` | `str-match`, `str-match-all` |
| `invert_match` | `invert-match` |
| `str_like`, `str_ilike` | `str-like`, `str-ilike` |

Detection, counting and extraction preserve the input names where R does;
subset preserves source column owners. NA uses `cl-vctrs-lite:*na*`.
Unambiguous logical columns stringify NIL as FALSE; legacy mixed character /
NIL inputs retain NIL as the established missing marker. Plain patterns must
be character vectors; regex/fixed/coll modifiers keep their native options.
Capture matching requires regex patterns. `str-which` returns zero-based
indices. SQL-like operators implement `%` and `_`; `str-like :ignore-case`
is deprecated and warns, while `str-ilike` always ignores case.

Matrices are rank-two arrays accessed with `array-dimension` and `aref`.
Additional return values carry column labels, row/outer names, and the
`:int` or `:string` prototype, including empty/all-NA results. Private weak
metadata retains those attributes for `invert-match`. `str-locate-all` and
`str-match-all` return ordinary proper lists or string-keyed alists; only
missing names and explicit named-empty collections use typed `:list` columns.
Their second value holds per-matrix labels, third outer names and fourth
prototype. Existing `str-extract-all` defaults to a vector of match vectors;
`:simplify t` returns a matrix padded with empty strings.

Capture selectors follow the same index translation: `:group 0` selects R's
first capture (group 1), and `:group -1` selects its full match (group 0).
Numeric vectors select several groups; their negative values translate before
R's column-exclusion rule (`-2` encodes R group -1, selecting zero columns;
`-3` encodes R group -2, excluding the full-match column). Fractional selectors
truncate after translation. Logical selectors retain R arithmetic: `t` selects
the first capture, and `#(nil)` selects the full match; bare NIL keeps the
no-group default. Missing selectors produce NA cells. R's dimension-dropping
rule determines whether selected captures return a vector or a matrix; matrix
results expose labels, row names and `:string` as additional values.

Location endpoints are zero-based and **inclusive**. Zero-width matches have
end=start-1. `invert-match` subtracts one from R's sentinel coordinates too:
leading 0 becomes -1 and trailing -1 becomes -2. These are encoded boundaries;
the existing `str-sub` consumer has an exclusive end and does not yet support
the full R location-matrix consumer path (SR3).

```lisp
(multiple-value-bind (locations labels names prototype)
    (str-locate #("😀a" "x") "a")
  (declare (ignore names prototype))
  (list labels (aref locations 0 0) (aref locations 0 1)))
;; => (#("start" "end") 1 1)
```

SR2 validation: 205 native checks and 421 pinned R 4.6.1 / stringr 1.6.0
reference cases pass, including 202 new detection/location/capture cases.
Independent review found and protected the one-row capture-name edge cases.
