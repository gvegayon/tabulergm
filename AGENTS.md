# Agent Instructions for tabulergm

## Committing Changes

Agents should commit completed changes in most cases unless the user asks them
not to. When an agent commits changes, the commit message must include a
`Co-authored-by:` trailer identifying that agent.

## Versioning

Use semantic versioning (`MAJOR.MINOR.PATCH`) for every package release:

- Increment `MAJOR` for backwards-incompatible public API changes.
- Increment `MINOR` for backwards-compatible functionality additions.
- Increment `PATCH` for backwards-compatible bug fixes.

Between releases, `DESCRIPTION` carries a development version (`X.Y.9000`).
Every PR appends its bullets to that *same* section at the top of
`NEWS.md` instead of bumping `Version` or opening a new section; only bump
`Version` (and start a fresh `NEWS.md` section) when actually cutting a
release.

Keep NEWS entries short — one sentence per bullet in most cases. Link the
PR or issue (`(#NN)`) for anything that needs more context rather than
spelling it out inline. Group bullets under `## User-facing changes` and
`## Internal changes`; omit a subsection that has no entries.

## Testing

Write end-to-end tests, not unit tests, and keep the suite lean. Drive the
exported API (`tabulergm_table()`, `with_style_*()`, `parse_ergm_model()`,
`parse_ergm_formula()`, `tabulergm_save()`) the way a user would — ideally
from a fitted model or a formula — and assert on what a user can observe:
the returned table, the emitted markup, the files written to disk. Do not
add tests that call internal helpers with `:::`. One test that reproduces
the real scenario is worth more than several that pin an internal's
contract, and it keeps refactoring cheap.

Lean means: cover a new feature with one or two end-to-end tests that
exercise it together, not one test per function, argument, or file. For
example, a batch of new terms goes into a single fitted-model (or formula)
table and is checked in one pass, not term by term.

Platform-specific behavior can be left to CI. The `R CMD check` matrix in
`.github/workflows/R-CMD-check.yaml` covers Windows, macOS, and Ubuntu
(release and devel), so a Windows-only code path does not need a synthetic
local stand-in.

Tests live in `inst/tinytest/` and run through `tests/tinytest.R`.

## Adding or Editing ERGM Term Definitions

Before touching the YAML term database (`inst/terms/`), read the standards
documented in `R/notation.R` (help topic `?"tabulergm-notation"`). In short:

- **Math**: `y_ij` for ties (sum over `i<j` undirected, `i != j` directed),
  `x_i` for vertex attributes, `\mathbf{1}(...)` for indicators, `B_1`/`B_2`
  for bipartite modes, and the Hunter (2007) parameterization for
  geometrically weighted terms. Verify formulas against the `ergm` manual
  and source literature; when ambiguous, compare numerically against
  `summary(nw ~ term)` on a small test network.
- **Drawings**: black = focal structure, gray = non-focal context (both
  attribute-irrelevant nodes and structurally non-focal nodes/edges,
  e.g., shared partners in gwesp/gwdsp), orange = attribute-bearing
  nodes (matched pairs share orange), orange vs. teal (`"#008080"`) =
  mixing between two attribute values; squares mark
  first-mode (B1) nodes and circles second-mode (B2) nodes, with the first
  mode drawn on the left; one-mode drawings use circles only; vertex size
  1.0 for focal nodes, .5 for context; dashed lines for match/covariate
  annotations. These conventions drive the explanatory notes that
  `tabulergm_table()` appends below rendered tables.
- **Text**: give every term a short `title` (heading case, no trailing
  period) and a one-to-three-sentence `description` written as a folded
  block scalar (`>-`). Without them the table falls back to the `ergm`
  database, whose text is often too long or contains raw LaTeX.
- **Citations**: add a `citation:` entry when a term has an identifiable
  source, with a `key` (`lastnameYEAR`) plus a `doi`, `arxiv`, `pmid`, or
  `url`. **Resolve every identifier before committing it** (e.g.
  `curl -sLH "Accept: application/x-bibtex" https://doi.org/<id>`); use the
  free-text `text:` field when no identifier can be verified rather than
  guessing one.
- **Wiring**: no parser changes needed — files are looked up by term name
  as `inst/terms/<term>.<directed|undirected>.yml`.
- **Aliases**: when two term names share an implementation in `ergm`
  (e.g. `dgwesp`/`gwesp`), write `alias: <term>` instead of copying the
  file; any other entry in the alias file overrides the target's (`plot`
  merges field by field). Confirm the shared implementation in the
  `ergm` source first.
- **Coverage**: add the new terms to an end-to-end test in
  `inst/tinytest/test_term_db.R` (see [Testing](#testing)), and
  add the term to the dictionary tables in `README.qmd` and
  `vignettes/ergm-with-tabulergm.Rmd` (hidden coverage-check chunks fail
  the render if a term is missing).

## Before Pushing Changes

Always run the following commands before pushing changes to ensure
the package builds and passes checks:

1. **Regenerate documentation** (updates `NAMESPACE` and `man/` files):

   ```r
   devtools::document()
   ```

2. **Run full package checks** (builds, tests, and validates the package):

   ```r
   devtools::check()
   ```

3. **Render the README** (regenerates `README.md` from `README.qmd`):

   ```bash
   quarto render README.qmd
   ```

These steps ensure that exported functions are registered in `NAMESPACE`,
man pages are up to date, all tests pass, and `README.md` reflects the
current state of the package.
