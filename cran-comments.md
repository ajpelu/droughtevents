## R CMD check results

0 errors | 0 warnings | 2 notes

* This is a new release.
* The "unable to verify current time" note (when present) is caused by an
  external time-verification service used internally by `R CMD check`
  being unavailable; it is unrelated to the package.

## Test environments

* Local: macOS (R 4.x), via `devtools::check(remote = TRUE, manual = TRUE)`
  -- 0 errors | 0 warnings | 2 notes (see above; both expected/unrelated
  to the package).
* macOS builder (r-release-macosx-arm64, R 4.6.0, via mac.r-project.org)
  -- Status: OK (0 errors | 0 warnings | 0 notes).
* win-builder (R-devel), via `devtools::check_win_devel()`
  -- Status: 1 NOTE (New submission; possibly misspelled words flagged are
  correct: a scientific term, an index acronym, and standard Latin
  citation abbreviations).

## Downstream dependencies

This is a new package with no downstream dependencies.
