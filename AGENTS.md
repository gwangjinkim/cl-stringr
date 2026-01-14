# AGENTS.md: cl-stringr Implementation Plan

## Milestones

### Milestone 0: Project Setup
- [ ] Initialize ASDF system definition (`cl-stringr.asd`).
- [ ] Setup package structure (`package.lisp`, `utils.lisp`).
- [ ] Define core `vmap` or similar utility for "vector-in, vector-out" operations with NA handling.

### Milestone 1: Basic String Operations
Group: **Basic**
- [ ] `str-length`
- [ ] `str-sub`
- [ ] `str-c`
- [ ] `str-trim`
- [ ] `str-to-upper`
- [ ] `str-to-lower`
- [ ] `str-to-title`
- [ ] `str-to-sentence`
- [ ] Unit tests for basic operations.

### Milestone 2: Pattern Matching (Detection & Counting)
Group: **Pattern**
- [ ] `str-detect`
- [ ] `str-subset`
- [ ] `str-count`
- [ ] Integration with `cl-ppcre`.
- [ ] Unit tests for pattern detection.

### Milestone 3: Pattern Extraction
Group: **Pattern**
- [ ] `str-extract`
- [ ] `str-extract-all`
- [ ] Handle regex capture groups.
- [ ] Unit tests for extraction.

### Milestone 4: Replacement and Mutation
Group: **Replacement**
- [ ] `str-replace`
- [ ] `str-replace-all`
- [ ] Unit tests for replacements.

### Milestone 5: Splitting and Joining
Group: **Structure**
- [ ] `str-split`
- [ ] `str-join`
- [ ] Unit tests for splitting/joining.

### Milestone 6: String Interpolation (Glue)
Group: **Templates**
- [ ] `str-glue` (Interpolation engine).
- [ ] Support for dynamic variable lookups.
- [ ] Unit tests for glue.

### Milestone 7: Lisp DSL & Tidyverse Integration
- [ ] `with-str-context` macro.
- [ ] Functional helpers (`map-str`, etc.).
- [ ] Integration with `cl-tibble` (ensuring functions work seamlessly on tibble columns).
- [ ] README and documentation.
