# Create rule packs -------------------------------------------------------
#' Create rule packs
#'
#' Functions for creating different kinds of rule packs. __Rule__ is a function
#' which converts object of interest (data, column, row, cell) to logical value
#' indicating whether this object satisfies certain condition. __Rule pack__ is
#' a function which combines several rules into one functional block. It takes a
#' data frame of interest and returns a data frame with only logical variables
#' and certain column naming scheme. Types of packs differ in interpretation of
#' their output.
#'
#' @param ... Rule functions which define pack. They can be in pure form or
#'   inside a list (at any depth).
#'
#' @details These functions convert `...` to list, apply `rlang`'s
#' [squash()][rlang::squash] and add appropriate classes. They are only
#' constructors and do not check for validity of certain pack. __Note__ that
#' it is allowed for elements of `...` to not have names: they will be computed
#' during exposure. However it is a good idea to manually name packs.
#'
#' @return `data_packs` returns a list of what should be [data rule
#'   packs][data-pack], `col_packs` - [column rule packs][column-pack],
#'   `row_packs` - [row rule packs][row-pack], `cell_packs` - [cell rule
#'   packs][cell-pack].
#'
#' @aliases packs
#'
#' @name rule-packs
NULL

#' @rdname rule-packs
#' @export
data_packs <- function(...) {
  squash_dots_rule_pack(..., .extra_class = "data_pack")
}

#' @rdname rule-packs
#' @export
col_packs <- function(...) {
  squash_dots_rule_pack(..., .extra_class = "col_pack")
}

#' @rdname rule-packs
#' @export
row_packs <- function(...) {
  squash_dots_rule_pack(..., .extra_class = "row_pack")
}

#' @rdname rule-packs
#' @export
cell_packs <- function(...) {
  squash_dots_rule_pack(..., .extra_class = "cell_pack")
}

squash_dots_rule_pack <- function(..., .extra_class) {
  rlang::dots_list(...) %>%
    rlang::squash() %>%
    lapply(function(x) {
      x %>% add_class_cond("rule_pack") %>% add_class(.extra_class)
    })
}


# Print rule packs --------------------------------------------------------
#' @export
print.data_pack <- function(x, ...) {
  cat("A Data rule pack:\n")
  NextMethod()
}

#' @export
print.col_pack <- function(x, ...) {
  cat("A Column rule pack:\n")
  NextMethod()
}

#' @export
print.row_pack <- function(x, ...) {
  cat("A Row rule pack:\n")
  NextMethod()
}

#' @export
print.cell_pack <- function(x, ...) {
  cat("A Cell rule pack:\n")
  NextMethod()
}


# Data rule pack ----------------------------------------------------------
#' Data rule pack
#'
#' Data rule pack is a [rule pack][rule-packs] which defines a set of rules for
#' data as a whole, i.e. functions which convert data to logical values. It
#' should return a data frame with the following properties:
#' - Number of rows equals to __one__.
#' - Column names should be treated as __rule names__.
#' - Values indicate whether the __data as a whole__ follows the rule.
#'
#' This format is inspired by `dplyr`'s [summarise()][dplyr::summarise] applied
#' to non-grouped data.
#'
#' The most common way to define data pack is by creating a [functional
#' sequence][magrittr::pipe] with no grouping and ending with
#' \code{summarise(...)}.
#'
#' @examples
#' data_dims_rules <- . %>%
#'   dplyr::summarise(nrow_low = nrow(.) > 10,
#'                    nrow_up = nrow(.) < 20,
#'                    ncol_low = ncol(.) > 5,
#'                    ncol_up = ncol(.) < 10)
#' data_na_rules <- . %>%
#'   dplyr::summarise(all_not_na = Negate(anyNA)(.))
#'
#' data_packs(
#'   data_nrow = data_dims_rules,
#'   data_na = data_na_rules
#' )
#'
#' @seealso [Column pack][column-pack], [row pack][row-pack], [cell
#'   pack][cell-pack].
#'
#' @name data-pack
NULL


# Column rule pack --------------------------------------------------------
#' Column rule pack
#'
#' Column rule pack is a [rule pack][rule-packs] which defines a set of rules
#' for columns as a whole, i.e. functions which convert columns of interest to
#' logical values. It should return a data frame with the following properties:
#' - Number of rows equals to __one__.
#' - Column names should be treated as concatenation of
#' \bold{'check column name' + 'separator' + 'rule name'}.
#' - Values indicate whether the __column as a whole__ follows the rule.
#'
#' This format is inspired by `dplyr`'s
#' [scoped variants of summarise()][dplyr::summarise_all] applied to non-grouped
#' data.
#'
#' There two common ways to define column pack:
#' - __For validating present columns__: by creating a [functional
#'   sequence][magrittr::pipe] with no grouping and ending with one of:
#'     - \code{summarise_all(.funs = rules(...))}.
#'     - \code{summarise_if(.predicate, .funs = rules(...))}.
#'     - \code{summarise_at(.vars, .funs = rules(...))}.
#' - __For validating groups as a whole__: by creating a grouped
#' [summarising][dplyr::summarise] [functional sequence][magrittr::pipe] ending
#' with [spread_groups()].
#'
#' @section Using rules():
#' Using [rules()] instead of [funs()][dplyr::funs] is recommended because:
#' - It is a convenient way to ensure consistent naming of rules without manual
#' name.
#' - It adds a common prefix to all rule names. This helps in defining
#' separator as prefix surrounded by any number of non-alphanumeric values.
#'
#' @examples
#' # Validating present columns
#' numeric_column_rules <- . %>% dplyr::summarise_if(
#'   is.numeric,
#'   rules(mean(.) > 5, sd(.) < 10)
#' )
#' character_column_rules <- . %>% dplyr::summarise_if(
#'   is.character,
#'   rules(. %in% letters[1:4])
#' )
#'
#' col_packs(
#'   num_col = numeric_column_rules,
#'   chr_col = character_column_rules
#' )
#'
#' # Validating groups as a whole
#' vs_am_n <- . %>%
#'   dplyr::group_by(vs, am) %>%
#'   dplyr::summarise(n_low = n() > 6, n_high = n() < 10) %>%
#'   spread_groups(vs, am)
#'
#' col_packs(vs_am_n = vs_am_n)
#'
#' @seealso [Data pack][data-pack], [row pack][row-pack], [cell
#'   pack][cell-pack].
#'
#' [spread_groups()] for validating groups as a whole.
#'
#' @name column-pack
NULL


# Row rule pack -----------------------------------------------------------
#' Row rule pack
#'
#' Row rule pack is a [rule pack][rule-packs] which defines a set of rules for
#' rows as a whole, i.e. functions which convert rows of interest to logical
#' values. It should return a data frame with the following properties:
#' - Number of rows equals to __number of checked rows__.
#' - Column names should be treated as __rule names__.
#' - Values indicate whether the __row as a whole__ follows the rule.
#'
#' This format is inspired by `dplyr`'s [transmute()][dplyr::transmute].
#'
#' The most common way to define row pack is by creating a [functional
#' sequence][magrittr::pipe] containing \code{transmute(...)}.
#'
#' @section Note about rearranging rows:
#' __Note__ that during exposure packs are applied to [keyed
#' object][keyholder::keys-set] with [id key][keyholder::keyholder-id]. So they
#' can rearrange rows as long as it is done with [functions supported by
#' keyholder][keyholder::keyholder-supported-funs]. Rows will be tracked and
#' recognized as in the original data frame of interest.
#'
#' @examples
#' some_row_mean_rules <- . %>% dplyr::slice(1:3) %>%
#'   dplyr::mutate(row_mean = rowMeans(.)) %>%
#'   dplyr::transmute(
#'     row_mean_low = row_mean > 10,
#'     row_mean_up = row_mean < 20
#'   )
#' all_row_sum_rules <- . %>% dplyr::mutate(row_sum = rowSums(.)) %>%
#'   dplyr::transmute(row_sum_low = row_sum > 30)
#'
#' row_packs(
#'   some_row_mean_rules,
#'   all_row_sum_rules
#' )
#'
#' @seealso [Data pack][data-pack], [column pack][column-pack], [cell
#'   pack][cell-pack].
#'
#' @name row-pack
NULL


# Cell rule pack -----------------------------------------------------------
#' Cell rule pack
#'
#' Cell rule pack is a [rule pack][rule-packs] which defines a set of rules for
#' cells, i.e. functions which convert cells of interest to logical values. It
#' should return a data frame with the following properties:
#' - Number of rows equals to __number of rows for checked cells__.
#' - Column names should be treated as concatenation of
#' \bold{'column name of check cell' + 'separator' + 'rule name'}
#' - Values indicate whether the __cell__ follows the rule.
#'
#' This format is inspired by [scoped variants of
#' transmute()][dplyr::transmute_all].
#'
#' The most common way to define cell pack is by creating a [functional
#' sequence][magrittr::pipe] containing one of:
#' - \code{transmute_all(.funs = rules(...))}.
#' - \code{transmute_if(.predicate, .funs = rules(...))}.
#' - \code{transmute_at(.vars, .funs = rules(...))}.
#'
#' @inheritSection column-pack Using rules()
#' @inheritSection row-pack Note about rearranging rows
#'
#' @examples
#' cell_outlier_rules <- . %>% dplyr::transmute_at(
#'   c("disp", "qsec"),
#'   rules(z_score = abs(. - mean(.)) / sd(.) > 1)
#' )
#'
#' cell_packs(outlier = cell_outlier_rules)
#'
#' @seealso [Data pack][data-pack], [column pack][column-pack], [row
#'   pack][row-pack].
#'
#' @name cell-pack
NULL
