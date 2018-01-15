#' ruler: Rule Your Data
#'
#' `ruler` offers a set of tools for creating tidy data validation reports using
#' [dplyr](http://dplyr.tidyverse.org) grammar of data manipulation. It
#' is designed to be flexible and extendable in terms of creating rules and
#' using their output.
#'
#' The common workflow is:
#' - Define dplyr-style [packs][rule-packs] of rules for basic data units (data,
#'   group, column, row, cell) to obey.
#' - [Expose][expose] some data to those rules. The result is the same data with
#'   possibly created [exposure][exposure] attribute. Exposure contains
#'   information [about applied packs][packs_info] and [tidy data validation
#'   report][ruler-report].
#' - Use data and exposure to perform some [actions][act_after_exposure]:
#'   [assert about rule breakers][assert_any_breaker], impute data, remove
#'   outliers and so on.
#'
#' To learn more about `ruler` browse vignettes with `browseVignettes(package =
#' "ruler")`. The preferred order is:
#'
#' 1. Design process and exposure format.
#' 2. Rule packs.
#' 3. Validation
#'
#' @import keyholder
#' @import dplyr
#' @importFrom rlang .data !! !!!
"_PACKAGE"
