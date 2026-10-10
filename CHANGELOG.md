# Changelog

All notable changes to this project are documented here. Format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/). See AGENTS.md
("Change Discipline") for how entries and commits are written.

## [Unreleased]

### Added
- `parity-SR2`: Add start/end/which, location/capture matrices, inverse matches
  and SQL-like detection; complete negate, capture selection, simplify and
  names for existing consumers. New list outputs follow the approved names
  protocol. Repair logical FALSE coercion, NA modifier captures and dropped
  capture-vector names with genuine regressions. All 205 native checks and
  421 pinned R cases pass (125 and 219 before); SR3 substring consumers remain
  outside the location-matrix implementation.
- SR1 pattern objects `regex`, `fixed`, `coll` and `boundary` retain vector
  values, options and names. `str-escape` and initial `str-equal` provide
  Unicode escaping/equality. Pattern consumers support those objects,
  scalar recycling and typed empty/all-NA output (topic: SR1 modifiers).
- Independent pinned ICU 71.1 bootstrap with a verified source checksum,
  dependency license, configurable native path and version diagnostics;
  runtime matching never loads R/stringi (topic: SR1 modifiers).

### Changed
- Pattern consumers use ICU Unicode semantics and locale collation; fixed
  matching uses simple Unicode uppercase comparisons. Regex replacement
  keeps capture references while fixed/coll replacements are literal.
  Native offsets retain the original Lisp strings (topic: SR1 modifiers).
- AGENTS.md: "Parity work" section listing this repository's parity
  milestones (SR1, SR2, SR3, SR4) and where the plan and backlog live
  (topic: parity plan).
- Removed emojis from README.md and PROGRESS.md (maintainer preference: no emojis in
  documentation). Meaning is kept in words where an emoji carried it
  (topic: docs style).

### Fixed
- HMS coercion removes a negative sign when fractional seconds round to
  zero and includes infinite hours when aligning finite labels. This
  prevents modifier/escaping mismatches against R on tiny negatives and
  mixed nonfinite columns (topic: hms coercion edge cases).
- Pattern operations retain C2 prototypes and owner-label coercion for
  numeric/logical/factor/UTC temporal columns, including typed empties and
  fractional timestamps. Native initialization errors release the library
  and clear all state before retry (topic: SR1 modifiers).
- **Every function signalled a type error on `cl-vctrs-lite:*na*`**
  (`NA is not a string designator`, `The value NA is not of type ...`).
  cl-readr now represents missing text cells as `*na*`, so any CSV column
  with a blank cell broke stringr. `vmap` (behind all `define-str-op`
  functions) and `str-join` now treat both `NIL` and `*na*` as missing:
  a position with an `*na*` input yields `*na*`; `NIL` inputs still yield
  `NIL` (unchanged behaviour) (topic: NA support).
- `str-subset` kept missing elements (the NA object is non-NIL, so it
  counted as a match). It now keeps only definite matches, like R's
  `str_subset`.

### Tests
- HMS rounding/alignment regressions add two unit checks and two pinned R
  cases. Stringr passes 125/125 unit checks and 219/219 conformance cases;
  the two new R cases both failed before repair (topic: hms coercion edge cases).
- SR1 adds constructor, matching, Unicode segmentation, recycling, typed
  owner/coercion and native failure-recovery unit regressions. Umbrella
  stringr suite: 123/123 checks, previously 37/37. R conformance: 209 new
  cases, 217 total stringr cases, all passing (topic: SR1 modifiers).
- `(asdf:test-system :cl-stringr)` now runs the Parachute suite and
  signals an error if it fails; previously the main system had no
  `:in-order-to` and the test system no `:perform`, so nothing ran
  (topic: test wiring). Suite: 27/27 pass.
- New "Missing values" test (10 checks). Suite: 37/37 pass.
