# General act -------------------------------------------------------------
#' Act after exposure
#'
#' A wrapper for consistent application of some actions based on the data after
#' exposure.
#' @param .tbl Result of [exposure][expose], i.e. data frame with [exposure]
#'   attribute.
#' @param .trigger Function which takes `.tbl` as argument and returns `TRUE` if
#'   some action needs to be performed.
#' @param .actor Function which takes `.tbl` as argument and performs the
#'   action.
#'
#' @details Basically `act_after_exposure()` is doing the following:
#' - Check that `.tbl` has a proper [exposure] attribute.
#' - Compute whether to perform intended action by computing `.trigger(.tbl)`.
#' - If trigger results in `TRUE` then `.actor(.tbl)` __is returned__. In other
#' case `.tbl` is returned.
#'
#' It is a good idea that `.actor` should be doing one of two things:
#' - Making side effects. For example throwing an error (if condition in
#' `.trigger` is not met), printing some information and so on. In this case it
#' should return `.tbl` to be used properly inside a [pipe][magrittr::pipe].
#' - Changing `.tbl` based on exposure information. In this case it should
#' return the imputed version of `.tbl`.
#'
#' @seealso [any_breaker] for trigger which returns `TRUE` in case any rule
#' breaker is found in exposure.
#'
#' [assert_any_breaker] for usage of `act_after_exposure()` in building data
#' validation pipelines.
#'
#' @examples
#' exposure_printer <- function(.tbl) {print(get_exposure(.tbl)); .tbl}
#' mtcars_exposed <- mtcars %>%
#'   expose(data_packs(. %>% dplyr::summarise(nrow_low = nrow(.) > 50))) %>%
#'   act_after_exposure(any_breaker, exposure_printer)
#'
#' @export
act_after_exposure <- function(.tbl, .trigger, .actor) {
  tbl_exposure <- get_exposure(.tbl)

  if (identical(tbl_exposure, NULL)) {
    stop("act_after_exposure: Input object does not have exposure.")
  }

  if (!is_exposure(tbl_exposure)) {
    stop("act_after_exposure: Extracted 'exposure' object is not a ",
         "proper exposure.")
  }

  if (isTRUE(.trigger(.tbl))) {
    res <- .actor(.tbl)
  } else {
    res <- .tbl
  }

  res
}


# Assertions --------------------------------------------------------------
#' Assert presence of rule breaker
#'
#' Function to assert if [exposure][expose] resulted in [detecting][any_breaker]
#' some rule breakers.
#'
#' @inheritParams act_after_exposure
#' @param .type The type of assertion. Can be only one of "error", "warning" or
#'   "message".
#' @param .silent If `TRUE` no printing of rule breaker information is done.
#' @param ... Arguments for printing rule breaker information.
#'
#' @details In case breaker presence this function does the following:
#' - In case `.silent` is `FALSE` print rows from exposure
#' [report][ruler-report] corresponding to rule breakers.
#' - Make assertion of the chosen `.type` about breaker presence in exposure.
#' - Return `.tbl` (for using inside a [pipe][magrittr::pipe]).
#'
#' If there are no breakers only `.tbl` is returned.
#'
#' @seealso [any_breaker] for checking of breaker presence in exposure result.
#'
#' [act_after_exposure] for making general actions based in exposure result.
#'
#' @examples
#' \dontrun{
#'   mtcars %>%
#'     expose(data_packs(. %>% dplyr::summarise(nrow_low = nrow(.) > 50))) %>%
#'     assert_any_breaker()
#' }
#' @export
assert_any_breaker <- function(.tbl, .type = "error", .silent = FALSE, ...) {
  informer_fun <- switch(
    .type,
    message = message,
    warning = function(.msg) {warning(.msg, call. = FALSE)},
    function(.msg) {stop(.msg, call. = FALSE)}
  )
  breakers_informer <- generate_breakers_informer(
    informer_fun,
    "assert_any_breaker: Some breakers found in exposure.",
    .silent,
    ...
  )

  act_after_exposure(.tbl, any_breaker, breakers_informer)
}


# Triggers ----------------------------------------------------------------
#' Is there any breaker in exposure?
#'
#' Function designed to be used as trigger in [act_after_exposure()]. Returns
#' `TRUE` if [exposure] attribute of `.tbl` has any information about data units
#' not obeying the rules, i.e. rule breakers.
#'
#' @inheritParams act_after_exposure
#'
#' @seealso [assert_any_breaker] for implicit usage of `any_breaker()`.
#'
#' @examples
#' mtcars %>%
#'   expose(data_packs(. %>% dplyr::summarise(nrow_low = nrow(.) > 50))) %>%
#'   any_breaker()
#'
#' @export
any_breaker <- function(.tbl) {
  input_exposure <- get_exposure(.tbl)
  if (!is_exposure(input_exposure)) {
    stop("any_breaker: Input object has not a proper exposure.")
  }

  report <- get_report(input_exposure)

  !all(is_obeyer(report[["value"]]))
}


# Actors ---------------------------------------------------------------
generate_breakers_informer <-
  function(.fun = stop, .message = "Some breakers found in exposure.",
           .silent, ...) {
    force(.fun)
    force(.message)
    force(.silent)

    function(.tbl) {
      report_breakers <- get_report(.tbl) %>% remove_obeyers(TRUE)

      if (!(.silent)) {
        print(report_breakers, ...)
      }
      .fun(.message)

      .tbl
    }
  }
