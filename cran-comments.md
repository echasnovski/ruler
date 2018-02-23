## Submission details

This submission is reaction to breaking changes in 'rlang' 0.2.0.

## Test environments
* Ubuntu 16.04 LTS (local install), R 3.4.3
* macOS 10.11 El Capitan (64-bit) (on R-hub), R 3.4.3
* win-builder, R Under development (unstable) (2018-02-21 r74285)
* Debian Linux (on R-hub), R-devel (2017-12-30 r73992), GCC

## R CMD check results

0 errors | 0 warnings | 0 notes

---

On some platforms on R-hub there was WARNING:

* checking top-level files ... WARNING
Conversion of ‘README.md’ failed:
pandoc: Could not fetch https://travis-ci.org/echasnovski/ruler.svg?branch=master

  This seems like pandoc issue on particular platforms. Local Ubuntu check doesn't have that.

## Reverse dependencies

There are no reverse dependencies.
