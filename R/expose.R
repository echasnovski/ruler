# Expose ------------------------------------------------------------------
#' Expose data to rule packs
#'
#' Function for applying rule packs to data.
#'
#' @param .tbl Data frame of interest.
#' @param ... Rule packs. They can be in pure form or inside a list
#'   (at any depth).
#' @param .rule_sep Regular expression used as separator between column and
#'   rule names in [col packs][column-pack] and [cell packs][cell-pack].
#' @param .remove_obeyers Whether to remove elements which obey rules from
#'   report.
#' @param .guess Whether to guess type of unsupported rule pack type (see
#'   Details).
#'
#' @details `expose()` applies all supplied rule packs to data, creates an
#'   [exposure] object based on results and stores it to attribute 'exposure'.
#'   It is guaranteed that `.tbl` is not modified in any other way in order to
#'   use `expose()` inside a \code{\link[magrittr]{pipe}}.
#'
#'   To work properly in some edge cases (see Examples) one should specify pack
#'   types with [appropriate function][rule-packs]. However with `.guess` equals
#'   to `TRUE` `expose` will guess the pack type based on its output after
#'   applying to `.tbl`. It uses number of rows and presence of `.rule_sep` in
#'   all column names as features.
#'
#'   It is a good idea to name all rule packs: explicitly in `...` or inside
#'   rule pack function. In case of missing name it is imputed based on possibly
#'   existing exposure attribute in `.tbl` and supplied rule packs. Imputation
#'   is similar to one in [rules()] but applied to every pack type separately.
#'
#'   Default value for `.rule_sep` is the regular expression `characters
#'   ._. surrounded by non alphanumeric characters`. It is picked to be used
#'   smoothly with `dplyr`'s [scoped verbs][dplyr::scoped] and [rules()] instead
#'   of [funs()][dplyr::funs()]. In most cases it shouldn't be changed but if
#'   needed it should align with `.prefix` in [rules()].
#'
#' @return A `.tbl` with possibly added 'exposure' attribute containing the
#'   resulting [exposure]. If `.tbl` already contains 'exposure' attribute then
#'   the result is binded with it.
#'
#' @examples
#' my_rule_pack <- . %>% dplyr::summarise(nrow_neg = nrow(.) < 0)
#' my_data_packs <- data_packs(my_data_pack_1 = my_rule_pack)
#'
#' # These pipes give identical results
#' mtcars %>% expose(my_data_packs) %>% get_report()
#' mtcars %>% expose(my_data_pack_1 = my_rule_pack) %>% get_report()
#'
#' # This throws an error because no pack type is specified for my_rule_pack
#' \dontrun{
#'   mtcars %>% expose(my_data_pack_1 = my_rule_pack, guess = FALSE) %>%
#'   get_report()
#'}
#'
#' # Edge cases against using 'guess = TRUE' for robust code
#' # If input data frame has one row 'guess' is unable to distinguish row pack
#' # from data pack and cell pack from column pack
#' row_rule_pack <- . %>% dplyr::transmute(neg_row_sum = rowSums(.) < 0)
#' cell_rule_pack <- . %>% dplyr::transmute_all(rules(neg_value = . < 0))
#'
#' # Results should have in column 'id' value 1 and not 0.
#' mtcars %>% dplyr::slice(1) %>% expose(row_rule_pack) %>% get_report()
#' mtcars %>% dplyr::slice(1) %>% expose(cell_rule_pack) %>% get_report()
#'
#' @export
expose <- function(.tbl, ..., .rule_sep = inside_punct("\\._\\."),
                   .remove_obeyers = TRUE, .guess = TRUE) {
  present_exposure <- get_exposure(.tbl)

  tbl_keyed <- .tbl %>% unkey() %>% use_id()
  packs <- rlang::dots_list(...) %>%
    rlang::squash()

  res_exposure <- lapply(
    packs, expose_single, .tbl = tbl_keyed, .rule_sep = .rule_sep,
    .remove_obeyers = .remove_obeyers, .guess = .guess
  ) %>%
    impute_exposure_pack_names(.exposure_ref = present_exposure) %>%
    add_pack_names() %>%
    bind_exposures()

  output_exposure <- bind_exposures(present_exposure, res_exposure,
                                    .validate_output = TRUE)

  set_exposure(.tbl, output_exposure)
}

#' Expose data to single rule pack
#'
#' The workhorse generic function for doing exposure. The result is
#' [single_exposure].
#'
#' @param .pack Rule pack function.
#' @param ... Further arguments passed to or from other methods.
#' @inheritParams expose
#'
#' @keywords internal
expose_single <- function(.tbl, .pack, .rule_sep,
                          .remove_obeyers, ...) {
  UseMethod("expose_single", .pack)
}

#' @export
expose_single.default <- function(.tbl, .pack, .rule_sep,
                                  .remove_obeyers, .guess, ...) {
  if (!.guess) {
    stop("There is unsupported class of rule pack.")
  }

  pack_out <- .pack(.tbl)
  pack_out_type <- guess_pack_type(.pack_out = pack_out, .rule_sep = .rule_sep)

  pack_report <- switch(
    pack_out_type,
    data_pack = interp_data_pack_out(pack_out),
    col_pack = interp_col_pack_out(pack_out, .rule_sep),
    row_pack = interp_row_pack_out(pack_out),
    cell_pack = interp_cell_pack_out(pack_out, .rule_sep)
  ) %>%
    remove_obeyers(.do_remove = .remove_obeyers)

  pack_with_class <- .pack %>%
    add_class_cond("rule_pack") %>%
    add_class(pack_out_type)

  new_single_exposure(pack_with_class, .remove_obeyers, pack_report)
}

#' @export
expose_single.data_pack <- function(.tbl, .pack, .rule_sep,
                                    .remove_obeyers, ...) {
  pack_report <- .tbl %>% .pack() %>% interp_data_pack_out() %>%
    remove_obeyers(.do_remove = .remove_obeyers)

  new_single_exposure(.pack = .pack, .remove_obeyers, pack_report)
}

#' @export
expose_single.col_pack <- function(.tbl, .pack, .rule_sep,
                                   .remove_obeyers, ...) {
  pack_report <- .tbl %>% .pack() %>%
    interp_col_pack_out(.rule_sep = .rule_sep) %>%
    remove_obeyers(.do_remove = .remove_obeyers)

  new_single_exposure(.pack = .pack, .remove_obeyers, pack_report)
}

#' @export
expose_single.row_pack <- function(.tbl, .pack, .rule_sep,
                                   .remove_obeyers, ...) {
  pack_report <- .tbl %>% .pack() %>% interp_row_pack_out() %>%
    remove_obeyers(.do_remove = .remove_obeyers)

  new_single_exposure(.pack = .pack, .remove_obeyers, pack_report)
}

#' @export
expose_single.cell_pack <- function(.tbl, .pack, .rule_sep,
                                    .remove_obeyers, ...) {
  pack_report <- .tbl %>% .pack() %>%
    interp_cell_pack_out(.rule_sep = .rule_sep) %>%
    remove_obeyers(.do_remove = .remove_obeyers)

  new_single_exposure(.pack = .pack, .remove_obeyers, pack_report)
}


# Interpretation ----------------------------------------------------------
interp_data_pack_out <- function(.pack_out) {
  assert_pack_out_all_logical(.pack_out, "data pack")
  assert_pack_out_one_row(.pack_out, "data pack")

  .pack_out %>%
    as_tibble() %>%
    tidyr::gather(key = "rule", value = "value",
                  rlang::UQS(rlang::syms(colnames(.pack_out)))) %>%
    mutate(var = ".all", id = 0L) %>%
    select(.data$rule, .data$var, .data$id, .data$value)
}

interp_col_pack_out <- function(.pack_out, .rule_sep) {
  assert_pack_out_all_logical(.pack_out, "column pack")
  assert_pack_out_one_row(.pack_out, "column pack")
  assert_pack_out_all_have_separator(.pack_out, "column pack", .rule_sep)

  .pack_out %>%
    as_tibble() %>%
    tidyr::gather(key = "var_rule", value = "value",
                  rlang::UQS(rlang::syms(colnames(.pack_out)))) %>%
    tidyr::separate(col = "var_rule", into = c("var", "rule"),
                    sep = .rule_sep) %>%
    mutate(id = 0L) %>%
    select(.data$rule, .data$var, .data$id, .data$value)
}

interp_row_pack_out <- function(.pack_out) {
  assert_pack_out_all_logical(.pack_out, "row pack")

  .pack_out %>%
    as_tibble() %>%
    restore_keys_at(.vars = ".id", .funs = funs(~ "id"),
                    .remove = TRUE, .unkey = TRUE) %>%
    tidyr::gather(key = "rule", value = "value",
                  rlang::UQS(rlang::syms(colnames(.pack_out)))) %>%
    mutate(var = ".all") %>%
    select(.data$rule, .data$var, .data$id, .data$value)
}

interp_cell_pack_out <- function(.pack_out, .rule_sep) {
  assert_pack_out_all_logical(.pack_out, "row pack")
  assert_pack_out_all_have_separator(.pack_out, "column pack", .rule_sep)

  .pack_out %>%
    as_tibble() %>%
    restore_keys_at(.vars = ".id", .funs = funs(~ "id"),
                    .remove = TRUE, .unkey = TRUE) %>%
    tidyr::gather(key = "var_rule", value = "value",
                  rlang::UQS(rlang::syms(colnames(.pack_out)))) %>%
    tidyr::separate(col = "var_rule", into = c("var", "rule"),
                    sep = .rule_sep) %>%
    select(.data$rule, .data$var, .data$id, .data$value)
}
