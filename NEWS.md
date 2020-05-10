# ruler (development version)

# ruler 0.2.3

* Reaction to `dplyr` 1.0.0.

# ruler 0.2.2

* Reaction to `tibble` 3.0.0.

# ruler 0.2.1

* Update logic of `rules()`: it now only converts bare expressions with `.` as input into formulas.

# ruler 0.2.0

This version is reaction to changes in `dplyr` 0.8.0.

* Breaking changes:
    * Name repair in `rules()` now uses `__` instead of `..` as separator for
    function position in input `...`. This is done because of new `dplyr` name
    repair rules which assume that `..{[0-9]}` in the end of the name can be
    removed.
    * `rules()` behaviour now depends on version of `dplyr`. For version less
    than 0.8.0 it is a direct wrapper for `dplyr::funs()` which does custom
    name repair. For newer versions it quotes elements in `...` (except explicit
    formulas) and repairs names of the output.

# ruler 0.1.4

* Reaction to `tibble` 2.0.0: ease some tests and adjust to new functionality.

# ruler 0.1.3

* Update for `dplyr` 0.7.5.

# ruler 0.1.2

* Update for `rlang` 0.2.0.

# ruler 0.1.1

* Update some unnecessarily strict tests (for CRAN).

# ruler 0.1.0

* Initial release.
