# Changelog

All notable changes to this project are documented here. Format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/). See AGENTS.md
("Change Discipline") for how entries and commits are written.

## [Unreleased]

### Tests
- `(asdf:test-system :cl-stringr)` now runs the Parachute suite and
  signals an error if it fails; previously the main system had no
  `:in-order-to` and the test system no `:perform`, so nothing ran
  (topic: test wiring). Suite: 27/27 pass.
