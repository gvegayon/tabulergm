## Test environments

* local macOS 15.0.1 (aarch64), R 4.5.1
* GitHub Actions: ubuntu-latest (devel, release, oldrel-1),
  macOS-latest (release), windows-latest (release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Notes

The package draws its term figures into PNG files under `tempdir()` and
caches them there for the duration of the session. Nothing is written
outside `tempdir()` unless the user explicitly asks for it via
`tabulergm_save(path = )`.
