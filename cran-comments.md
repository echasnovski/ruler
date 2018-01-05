## Submission details

This submission is reaction to update of its implicit dependency package 'tibble'. The current version has some unnecessarily strict tests for printing objects, which result in ERRORs.

In this submission __no change in functionality is made__, only tests are updated.

## Test environments
* Ubuntu 16.04 LTS (local install), R 3.4.3
* macOS 10.11 El Capitan (64-bit) (on R-hub), R 3.4.1
* win-builder, R Under development (unstable) (2018-01-04 r74054)
* Debian Linux (on R-hub), R-devel (2017-12-30 r73992), GCC

## R CMD check results

0 errors | 0 warnings | 0 notes

---

On some Linux platforms on R-hub (Ubuntu Linux 16.04 LTS, R-release, GCC; Fedora Linux, R-devel, clang, gfortran) there was WARNING:

* checking top-level files ... WARNING
Conversion of ‘README.md’ failed:
pandoc: Could not fetch https://travis-ci.org/echasnovski/ruler.svg?branch=master

  This seems like pandoc issue on particular platforms. Local Ubuntu check doesn't have that.

## Reverse dependencies

There are no reverse dependencies.
