#' Spread grouping columns
#'
#' Function that is used during interpretation of [group pack][group-pack]
#' output. It converts grouped [summary][dplyr::summarise] into [column
#' pack][column-pack] format.
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
#'   dplyr::summarise(n_low = dplyr::n() > 6, n_high = dplyr::n() < 10)
#'
#' spread_groups(mtcars_grouped_summary, vs, am)
#'
#' spread_groups(mtcars_grouped_summary, vs, am, .group_sep = "__")
#'
#' spread_groups(mtcars_grouped_summary, vs, am, .col_sep = "__")
#' @export
spread_groups <- function(.tbl, ..., .group_sep = ".", .col_sep = "._.") {
  tbl_ungrouped <- ungroup(.tbl)
  tbl_group_cols <- select(tbl_ungrouped, ...)

  # Check for presence of suppied group columns
  if (ncol(tbl_group_cols) == 0) {
    stop("spread_groups: No grouping columns are supplied.")
  }

  # Check if grouping columns has unique combined levels
  if (nrow(tbl_group_cols) != nrow(distinct(tbl_group_cols))) {
    stop("spread_groups: Grouping columns define non-unique levels.")
  }

  # Check for presence of rule columns
  rule_cols <- negate_select_cols(tbl_ungrouped, ...)
  rule_syms <- rlang::syms(rule_cols)
  if (length(rule_cols) == 0) {
    stop("spread_groups: No rule columns are supplied.")
  }

  # Check if all rule columns are logical
  is_all_rules_lgl <- tbl_ungrouped %>%
    select(!!!rule_syms) %>%
    vapply(is.logical, TRUE) %>%
    all()
  if (!is_all_rules_lgl) {
    stop("spread_groups: Some rule columns are not logical.")
  }

  group_id_sym <- rlang::sym(keyholder::compute_id_name(rule_cols))

  tbl_ungrouped %>%
    tidyr::unite(!!group_id_sym, ..., sep = .group_sep, remove = TRUE) %>%
    tidyr::gather(key = "rule_name", value = "value", !!!rule_syms) %>%
    tidyr::unite(
      col = "var_rule",
      !!group_id_sym,
      .data[["rule_name"]],
      sep = .col_sep,
      remove = TRUE
    ) %>%
    # For preserving ordering by rule and then by variable
    mutate(
      var_rule = factor(.data$var_rule, levels = unique(.data$var_rule))
    ) %>%
    tidyr::spread(key = "var_rule", value = "value")
}
