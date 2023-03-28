# Create rule packs -------------------------------------------------------
#' Create rule packs
#'
#' Functions for creating different kinds of rule packs. __Rule__ is a function
#' which converts data unit of interest (data, group, column, row, cell) to
#' logical value indicating whether this object satisfies certain condition.
#' __Rule pack__ is a function which combines several rules into one functional
#' block. It takes a data frame of interest and returns a data frame with
#' certain structure and column naming scheme. Types of packs differ in
#' interpretation of their output.
#'
#' @param ... Functions which define packs. They can be in pure form or inside a
#'   list (at any depth).
#' @param .group_vars Character vector of names of grouping variables.
#' @param .group_sep String to be used as separator when uniting grouping
#'   levels for `var` column in [exposure report][ruler-report].
#'
#' @details These functions convert `...` to list, apply `rlang`'s
#' [squash()][rlang::squash] and add appropriate classes (`group_packs()` also
#' adds necessary attributes). Also they are only constructors and do not check
#' for validity of certain pack. __Note__ that it is allowed for elements of
#' `...` to not have names: they will be computed during exposure. However it is
#' a good idea to manually name packs.
#'
#' @return `data_packs()` returns a list of what should be [data rule
#'   packs][data-pack], `group_packs()` - [group rule packs][group-pack],
#'   `col_packs()` - [column rule packs][column-pack], `row_packs()` - [row rule
#'   packs][row-pack], `cell_packs()` - [cell rule packs][cell-pack].
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
group_packs <- function(..., .group_vars, .group_sep = ".") {
  assert_character(.group_vars, ".group_vars")
  assert_positive_length(.group_vars, ".group_vars")
  assert_character(.group_sep, ".group_sep")
  assert_length(.group_sep, 1L, ".group_sep")

  squash_dots_rule_pack(..., .extra_class = "group_pack") %>%
    lapply(`attr<-`, which = "group_vars", value = .group_vars) %>%
    lapply(`attr<-`, which = "group_sep", value = .group_sep)
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
    squash() %>%
    lapply(function(x) {
      x %>%
        add_class_cond("rule_pack") %>%
        add_class(.extra_class)
    })
}


# Print rule packs --------------------------------------------------------
#' @export
print.data_pack <- function(x, ...) {
  cat("A Data rule pack:\n")
  NextMethod()
}

#' @export
print.group_pack <- function(x, ...) {
  cat("A Group rule pack:\n")
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
#' The most common way to define data pack is by creating a
#' \link[magrittr:pipe]{functional sequence} with no grouping and ending with
#' \code{summarise(...)}.
#'
#' @examples
#' data_dims_rules <- . %>%
#'   dplyr::summarise(
#'     nrow_low = nrow(.) > 10,
#'     nrow_up = nrow(.) < 20,
#'     ncol_low = ncol(.) > 5,
#'     ncol_up = ncol(.) < 10
#'   )
#' data_na_rules <- . %>%
#'   dplyr::summarise(all_not_na = Negate(anyNA)(.))
#'
#' data_packs(
#'   data_nrow = data_dims_rules,
#'   data_na = data_na_rules
#' )
#' @seealso [Group pack][group-pack], [Column pack][column-pack], [row
#'   pack][row-pack], [cell pack][cell-pack].
#'
#' @name data-pack
NULL


# Group rule pack ---------------------------------------------------------
#' Group rule pack
#'
#' Group rule pack is a [rule pack][rule-packs] which defines a set of rules
#' for groups of rows as a whole, i.e. functions which convert groups of
#' interest to logical values. It should return a data frame with the following
#' properties:
#' - There should be present some columns which combined values __uniquely__
#'   describe group. They should be defined during creation with
#'   [group_packs()][rule-packs].
#' - Number of rows equals to __number of checked groups__.
#' - Names of non-grouping columns should be treated as __rule names__.
#' - Values indicate whether the __group as a whole__ follows the rule.
#'
#' This format is inspired by `dplyr`'s [summarise()][dplyr::summarise] applied
#' to grouped data.
#'
#' The most common way to define data pack is by creating a
#' \link[magrittr:pipe]{functional sequence} with grouping and ending with
#' \code{summarise(...)}.
#'
#' @section Interpretation:
#' Group pack output is interpreted in the following way:
#' - All grouping columns are [united][tidyr::unite] with delimiter `.group_sep`
#' (which is an argument of `group_packs()`).
#' - Levels of the resulting column are treated as names of some new variables
#' which should be exposed as a whole. Names of non-grouping columns are treated
#' as rule names. They are transformed in [column pack][column-pack] format and
#' interpreted accordingly.
#'
#' Exposure result of group pack is different from others in a way that column
#' `var` in [exposure report][ruler-report] doesn't represent the actual column
#' in data.
#'
#' @examples
#' vs_am_rules <- . %>%
#'   dplyr::group_by(vs, am) %>%
#'   dplyr::summarise(
#'     nrow_low = n(.) > 10,
#'     nrow_up = n(.) < 20,
#'     rowmeans_low = rowMeans(.) > 19
#'   )
#'
#' group_packs(vs_am = vs_am_rules, .group_vars = c("vs", "am"))
#' @seealso [Data pack][data-pack], [Column pack][column-pack], [row
#'   pack][row-pack], [cell pack][cell-pack].
#'
#' @name group-pack
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
#' The most common way to define column pack is by creating a
#' \link[magrittr:pipe]{functional sequence} with no grouping and ending with
#' one of:
#'   - \code{summarise_all(.funs = rules(...))}.
#'   - \code{summarise_if(.predicate, .funs = rules(...))}.
#'   - \code{summarise_at(.vars, .funs = rules(...))}.
#'
#' __Note__ that (as of `dplyr` version 0.7.4) when only one column is
#' summarised, names of the output don't have a necessary structure. The 'check
#' column name' is missing which results (after [exposure][expose()]) into empty
#' string in `var` column of [validation report][ruler-report]. The current way
#' of dealing with this is to name the input column (see examples).
#'
#' @section Using rules():
#' Using [rules()] to create list of functions for scoped `dplyr` "mutating"
#' verbs (such as [summarise_all()][dplyr::summarise_all()] and
#' [transmute_all()][dplyr::transmute_all()]) is recommended because:
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
#' # Dealing with one column edge case
#' improper_pack <- . %>% dplyr::summarise_at(
#'   dplyr::vars(vs),
#'   rules(improper_is_chr = is.character)
#' )
#'
#' proper_pack <- . %>% dplyr::summarise_at(
#'   dplyr::vars(vs = vs),
#'   rules(proper_is_chr = is.character)
#' )
#'
#' mtcars %>%
#'   expose(col_packs(improper_pack, proper_pack)) %>%
#'   get_report()
#' @seealso [Data pack][data-pack], [group pack][group-pack], [row
#'   pack][row-pack], [cell pack][cell-pack].
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
#' The most common way to define row pack is by creating a
#' \link[magrittr:pipe]{functional sequence} containing \code{transmute(...)}.
#'
#' @section Note about rearranging rows:
#' __Note__ that during exposure packs are applied to [keyed
#' object][keyholder::keys-set] with [id key][keyholder::keyholder-id]. So they
#' can rearrange rows as long as it is done with [functions supported by
#' keyholder][keyholder::keyholder-supported-funs]. Rows will be tracked and
#' recognized as in the original data frame of interest.
#'
#' @examples
#' some_row_mean_rules <- . %>%
#'   dplyr::slice(1:3) %>%
#'   dplyr::mutate(row_mean = rowMeans(.)) %>%
#'   dplyr::transmute(
#'     row_mean_low = row_mean > 10,
#'     row_mean_up = row_mean < 20
#'   )
#' all_row_sum_rules <- . %>%
#'   dplyr::mutate(row_sum = rowSums(.)) %>%
#'   dplyr::transmute(row_sum_low = row_sum > 30)
#'
#' row_packs(
#'   some_row_mean_rules,
#'   all_row_sum_rules
#' )
#' @seealso [Data pack][data-pack], [group pack][group-pack], [column
#'   pack][column-pack], [cell pack][cell-pack].
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
#' The most common way to define cell pack is by creating a
#' \link[magrittr:pipe]{functional sequence} containing one of:
#' - \code{transmute_all(.funs = rules(...))}.
#' - \code{transmute_if(.predicate, .funs = rules(...))}.
#' - \code{transmute_at(.vars, .funs = rules(...))}.
#'
#' __Note__ that (as of `dplyr` version 0.7.4) when only one column is
#' transmuted, names of the output don't have a necessary structure. The 'column
#' name of check cell' is missing which results (after [exposure][expose()])
#' into empty string in `var` column of [validation report][ruler-report]. The
#' current way of dealing with this is to name the input column (see examples).
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
#' # Dealing with one column edge case
#' improper_pack <- . %>% dplyr::transmute_at(
#'   dplyr::vars(vs),
#'   rules(improper_is_neg = . < 0)
#' )
#'
#' proper_pack <- . %>% dplyr::transmute_at(
#'   dplyr::vars(vs = vs),
#'   rules(proper_is_neg = . < 0)
#' )
#'
#' mtcars[1:2, ] %>%
#'   expose(cell_packs(improper_pack, proper_pack)) %>%
#'   get_report()
#' @seealso [Data pack][data-pack], [group pack][group-pack], [column
#'   pack][column-pack], [row pack][row-pack].
#'
#' @name cell-pack
NULL
