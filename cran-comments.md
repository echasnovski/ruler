## Test environments
* Ubuntu 16.04 LTS (local install), R 3.4.2
* macOS 10.11 El Capitan (64-bit) (on R-hub), R 3.4.1
* Windows Server 2008 R2 SP1, 32/64 bit (64-bit) (on R-hub), R 3.4.2
* win-builder, R Under development (unstable) (2017-09-12 r73242)
* Debian Linux (on R-hub), R-devel (2017-11-19 r73752), GCC

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Evgeni Chasnovski <evgeni.chasnovski@gmail.com>’

New submission

* This is a first release of the package.

---

On some Linux platforms on R-hub (Ubuntu Linux 16.04 LTS, R-release, GCC; Fedora Linux, R-devel, clang, gfortran) there was WARNING:

* checking top-level files ... WARNING
Conversion of ‘README.md’ failed:
pandoc: Could not fetch https://travis-ci.org/echasnovski/ruler.svg?branch=master

  This seems like pandoc issue on particular platforms. Local Ubuntu check doesn't have that.

## Reverse dependencies

This is a new release, so there are no reverse dependencies.
