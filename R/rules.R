#' Create a list of rules
#'
#' `rules()` is a wrapper for `dplyr`'s [funs][dplyr::funs] which provides a
#' different naming scheme.
#'
#' @param ... A list of functions (as in [funs][dplyr::funs]).
#' @param .args A named list of additional arguments to be added to all function
#'   calls (as in [funs][dplyr::funs]).
#' @param .prefix Prefix to be added to function names.
#'
#' @details `rules()` behaves exactly as `funs()` with only difference being the
#' names of the output. The following naming scheme is applied:
#' - Absent names are replaced with the 'rule..\\{ind\\}' where \\{ind\\} is the
#' index of function position in the `...` .
#' - `.prefix` is added at the beginning of all names. The default is `._.` . It
#'   is picked for its symbolism (it is the Morse code of letter 'R') and rare
#'   occurrence in names. In those rare cases it can be manually changed but
#'   this will not be tracked further.
#'
#' @examples
#' rules_1 <- rules(mean, sd, .args = list(na.rm = TRUE))
#' rules_1_ref <- dplyr::funs('._.rule..1' = mean, '._.rule..2' = sd,
#'                            .args = list(na.rm = TRUE))
#' identical(rules_1, rules_1_ref)
#'
#' rules_2 <- rules(mean, sd = sd, "var")
#' rules_2_ref <- dplyr::funs(
#'   '._.rule..1' = mean,
#'   '._.sd' = sd,
#'   '._.rule..3' = "var"
#' )
#' identical(rules_2, rules_2_ref)
#'
#' rules_3 <- rules(mean, .prefix = "__")
#' rules_3_ref <- dplyr::funs('__rule..1' = mean)
#' identical(rules_3, rules_3_ref)
#'
#' @export
rules <- function(..., .args = list(), .prefix = "._.") {
  dots <- quos(...)
  names(dots) <- enhance_names(name = rlang::names2(dots), prefix = .prefix,
                               root = "rule")

  do.call(what = dplyr::funs, args = c(rlang::UQS(dots), list(.args = .args)),
          envir = rlang::caller_env())
}
