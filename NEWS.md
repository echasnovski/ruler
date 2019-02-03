# ruler 0.1.4.9000

* Reaction to `dplyr` 0.8.0:
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
