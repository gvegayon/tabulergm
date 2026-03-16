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

These steps ensure that exported functions are registered in `NAMESPACE`,
man pages are up to date, and all tests pass.
