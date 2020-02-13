This is an emergency release to fix a regression introduced with v1.3.0

## Test environments

* local OS X install (10.15.13): R 3.6.1

* ubuntu 14.04 (on travis-ci): R devel, R 3.6, R 3.5, R 3.4, R 3.2

* win-builder: R devel, R 3.6.1

## R CMD check results

0 ERRORS | 0 WARNINGS | 0 NOTES

## Downstream Dependencies

I also ran R CMD check on all downstream dependencies of styler using the
revdepcheck package. The downstream dependencies are: exampletestr,
languageserver, crunch, drake, knitr, nph, reprex, shinydashboardPlus,
tradestatistics, usethis.

All of them finished R CMD CHECK with the same number of ERRORS, WARNINGS and
NOTES as with the current CRAN version of styler, which means the new submission
of styler does not introduce any ERRORS, WARNINGS and NOTES in downstream
dependencies.

