#' Create a list of rules
#'
#' `rules()` is a function designed to create input for `.funs` argument of
#' scoped `dplyr` "mutating" verbs (such as
#' [summarise_all()][dplyr::summarise_all()] and
#' [transmute_all()][dplyr::transmute_all()]). It converts bare expressions
#' with `.` as input into formulas and repairs names of the output.
#'
#' @param ... Bare expression(s) with `.` as input.
#' @param .prefix Prefix to be added to function names.
#'
#' @details `rules()` repairs names by the following algorithm:
#' - Absent names are replaced with the `'rule__{ind}'` where `{ind}` is the
#'   index of function position in the `...` .
#' - `.prefix` is added at the beginning of all names. The default is `._.` . It
#'   is picked for its symbolism (it is the Morse code of letter 'R') and rare
#'   occurrence in names. In those rare cases it can be manually changed but
#'   this will not be tracked further. **Note** that it is a good idea for
#'   `.prefix` to be [syntactic][make.names()], as `dplyr` will force tibble
#'   names to be syntactic. To check if string is "good", use it as input to
#'   `make.names()`: if output equals that string than it is a "good" choice.
#'
#' @examples
#' # `rules()` accepts bare expression calls with `.` as input, which is not
#' # possible with advised `list()` approach of `dplyr`
#' dplyr::summarise_all(mtcars[, 1:2], rules(sd, "sd", sd(.), ~ sd(.)))
#'
#' dplyr::summarise_all(mtcars[, 1:2], rules(sd, .prefix = "a_a_"))
#'
#' # Use `...` in `summarise_all()` to supply extra arguments
#' dplyr::summarise_all(data.frame(x = c(1:2, NA)), rules(sd), na.rm = TRUE)
#' @export
rules <- function(..., .prefix = "._.") {
  dots <- quos(...)
  names(dots) <- enhance_names(
    .name = rlang::names2(dots),
    .prefix = .prefix,
    .root = "rule"
  )

  lapply(dots, extract_funs_input)
}

extract_funs_input <- function(obj) {
  expr <- rlang::quo_get_expr(obj)
  obj_function <- quo_get_function(obj)

  if (!is.null(obj_function)) {
    obj_function
  } else if (rlang::is_formula(expr)) {
    # This seems to actually recreate the formula, meaning attaching different
    # environment. However, this shouldn't be a problem because of
    # "explicitness" of input formula.
    eval(expr)
  } else if (has_dot_symbol(expr)) {
    stats::as.formula(
      object = paste0("~", rlang::expr_text(expr)),
      env = rlang::quo_get_env(obj)
    )
  } else if (is.character(expr) && (length(expr) == 1)) {
    expr
  } else {
    stop(
      "Wrong input `",
      rlang::expr_text(expr),
      "` to `rules()`.",
      call. = FALSE
    )
  }
}

has_dot_symbol <- function(x) {
  x_parts <- vapply(squash_expr(x), rlang::expr_text, character(1))

  any(x_parts == ".")
}

squash_expr <- function(x) {
  if (rlang::is_syntactic_literal(x) || rlang::is_symbol(x)) {
    return(x)
  }

  unlist(lapply(as.list(x), squash_expr))
}

quo_get_function <- function(x) {
  get0(
    x = rlang::expr_text(rlang::quo_get_expr(x)),
    envir = rlang::quo_get_env(x),
    mode = "function"
  )
}
