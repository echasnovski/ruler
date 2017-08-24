#' Spread grouping columns
#'
#' Function to be used in packs which validate groups as a whole. It converts
#' grouped [summary][dplyr::summarise] into [column pack][column-pack] format.
#' It is recommended to be used at the end of the grouped summarising
#' [functional sequence][magrittr::pipe].
#'
#' @param .tbl Data frame with result of grouped summary.
#' @param ... A selection of grouping columns (as in [tidyr::unite()]).
#' @param .group_sep A string to be used as separator of grouping levels.
#' @param .col_sep A string to be used as separator in column pack.
#'
#' @details Multiple grouping variables are converted to one with
#' [tidyr::unite()] and separator `.group_sep`. New values are then treated as
#' variable names which should be validated and which represent the group data
#' as a whole.
#'
#' @return A data frame in [column pack][column-pack] format.
#'
#' @examples
#' mtcars_grouped_summary <- mtcars %>%
#'   dplyr::group_by(vs, am) %>%
#'   dplyr::summarise(n_low = n() > 6, n_high = n() < 10)
#'
#' spread_groups(mtcars_grouped_summary, vs, am)
#' spread_groups(mtcars_grouped_summary, vs, am, .group_sep = "__")
#' spread_groups(mtcars_grouped_summary, vs, am, .col_sep = "__")
#'
#' @export
spread_groups <- function(.tbl, ..., .group_sep = "_", .col_sep = "._.") {
  tbl_ungrouped <- ungroup(.tbl)

  if (rlang::dots_n(...) == 0) {
    stop("No grouping columns are supplied.")
  }

  rule_cols <- negate_select_cols(tbl_ungrouped, ...)
  rule_syms <- rlang::syms(rule_cols)
  if (length(rule_cols) == 0) {
    stop("No rule columns are supplied.")
  }

  is_all_rules_lgl <- tbl_ungrouped %>%
    select(rlang::UQS(rule_syms)) %>%
    vapply(is.logical, TRUE) %>%
    all()
  if (!is_all_rules_lgl) {
    stop("Some rule columns are not logical.")
  }

  group_id_sym <- rlang::sym(keyholder::compute_id_name(rule_cols))

  tbl_ungrouped %>%
    tidyr::unite(rlang::UQ(group_id_sym), ...,
                 sep = .group_sep, remove = TRUE) %>%
    tidyr::gather(key = "rule_name", value = "value",
                  rlang::UQS(rule_syms)) %>%
    tidyr::unite(col = "var_rule",
                 rlang::UQ(group_id_sym), .data[["rule_name"]],
                 sep = .col_sep, remove = TRUE) %>%
    # For preserving ordering by rule and then by variable
    mutate(var_rule = factor(.data$var_rule,
                             levels = unique(.data$var_rule))) %>%
    tidyr::spread(key = "var_rule", value = "value")
}
