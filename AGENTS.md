# Agent Instructions for tabulergm

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
