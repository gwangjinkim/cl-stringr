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

## Change Discipline (git + documentation)

Every change to this repository must be reconstructible later: what changed,
why, and what it means for users. Agents and humans follow these rules.

1. **Branch, never commit straight to `main`.** Use a topic branch such as
   `fix/<topic>` or `feat/<topic>` (cross-package work in the tidystat effort
   uses `fix/tidystat-phase0`, `feat/tidystat-phase1`, ...).
2. **One logical change per commit.** Test-suite repairs, bug fixes, new
   features and documentation go in separate commits, so each can be
   reviewed, bisected or reverted on its own.
3. **Commit messages explain the why.** Format:
   ```
   <area>: <imperative summary, <= 72 chars>

   Problem:  what was wrong / missing, with the observable symptom.
   Cause:    the root cause (file:function), if it is a fix.
   Change:   what this commit does.
   Impact:   behaviour change for users; mark BREAKING if any.
   Tests:    which tests were added/changed, and the suite result.
   ```
4. **Every commit updates `CHANGELOG.md`** under `## [Unreleased]`, in the
   sections *Fixed*, *Added*, *Changed*, *Breaking*, *Tests*. Each entry
   says what users will notice and names the commit topic.
5. **Run the full test suite before committing** (`make test`, or from the
   umbrella repo `../cl-tidystat/scripts/run-tests.sh <this-package>`) and put
   the result in the commit message. Never commit with a red suite unless
   the commit message says so and why.
6. **Cross-package changes** (e.g. an API in `cl-vctrs-lite` used by
   `cl-dplyr`) are recorded in both repos' changelogs and in
   `../cl-tidystat/docs/CHANGES-phase*.md`, which links the commits.
7. **Push only after review**; do not rewrite history that has been pushed.

## Parity work (R tidyverse equivalence)

This repository is part of the plan to reach full parity with the current
R tidyverse. Milestones owned by this repository: **SR1, SR2, SR3, SR4**.

- Plan, conventions (R -> Lisp names, arguments, types, 0-based indices)
  and the step-by-step workflow: `docs/PARITY-PLAN.md` in
  `../cl-tidystat` (GitHub: gwangjinkim/cl-tidystat, private; ask the
  maintainer for access).
- Function lists with status and target names:
  `../cl-tidystat/parity/backlog/<ID>.md`; overview in `INDEX.md`.
- Work on a branch `feat/parity-<ID>`, write conformance cases first,
  and regenerate the parity status in cl-tidystat after each step.
