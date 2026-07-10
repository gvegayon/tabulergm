# Agent Instructions for tabulergm

## Adding or Editing ERGM Term Definitions

Before touching the YAML term database (`inst/terms/`), read the standards
documented in `R/notation.R` (help topic `?"tabulergm-notation"`). In short:

- **Math**: `y_ij` for ties (sum over `i<j` undirected, `i != j` directed),
  `x_i` for vertex attributes, `\mathbf{1}(...)` for indicators, `B_1`/`B_2`
  for bipartite modes, and the Hunter (2007) parameterization for
  geometrically weighted terms. Verify formulas against the `ergm` manual
  and source literature; when ambiguous, compare numerically against
  `summary(nw ~ term)` on a small test network.
- **Drawings**: orange = attribute-bearing nodes (matched pairs share
  orange), gray = attribute-irrelevant, black = structural; squares mark
  first-mode (B1) nodes and circles second-mode (B2) nodes, with the first
  mode drawn on the left; one-mode drawings use circles only; vertex size
  1.0 for focal nodes, .5 for context; dashed lines for match/covariate
  annotations.
- **Wiring**: no parser changes needed — files are looked up by term name
  as `inst/terms/<term>.<directed|undirected>.yml`.
- **Coverage**: add tinytest cases in `inst/tinytest/test_term_db.R`, and
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
