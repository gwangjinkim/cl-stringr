# Changelog

All notable changes to this project are documented here. Format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/). See AGENTS.md
("Change Discipline") for how entries and commits are written.

## [Unreleased]

### Fixed
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
- `(asdf:test-system :cl-stringr)` now runs the Parachute suite and
  signals an error if it fails; previously the main system had no
  `:in-order-to` and the test system no `:perform`, so nothing ran
  (topic: test wiring). Suite: 27/27 pass.
- New "Missing values" test (10 checks). Suite: 37/37 pass.
