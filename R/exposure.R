# Documentation -----------------------------------------------------------
#' Exposure
#'
#' Exposure is a result of [exposing][ruler::expose()] data to rules. It is
#' implemented with S3 class `exposure` which is a list of the following
#' structure: `packs_info` - a [packs_info] object; `report` -
#' [tidy data validation report][ruler-report].
#'
#' @param .x Object to test.
#' @param .object Object to get or remove `exposure` attribute from.
#'
#' @return `get_exposure()` returns `object` if it is exposure and its attribute
#' 'exposure' otherwise.
#'
#' `remove_exposure()` returns `object` with removed attributed 'exposure'.
#'
#' @examples
#' my_col_packs <- col_packs(
#'   col_sum_props = . %>% dplyr::summarise_all(
#'     rules(
#'       col_sum_low = sum(.) > 100,
#'       col_sum_high = sum(.) < 1000
#'     )
#'   )
#' )
#' mtcars_exposed <- mtcars %>% expose(my_col_packs)
#' mtcars_exposure <- mtcars_exposed %>% get_exposure()
#'
#' is_exposure(mtcars_exposure)
#'
#' identical(remove_exposure(mtcars_exposed), mtcars)
#'
#' identical(get_exposure(mtcars_exposure), mtcars_exposure)
#'
#' @name exposure
NULL

#' Single exposure
#'
#' An S3 class `single_exposure` to represent exposure of data to __one__ rule
#' pack. It is a list of the following structure: `pack_info` - single
#' [pack_info] object; `report` - [tidy data validation report][ruler-report]
#' without column `pack`.
#'
#' @details Single exposure is implemented in order to encapsulate preliminary
#' exposure data from single rule pack. It is needed to impute possibly missing
#' pack names during [exposure][expose()]. That is why `single_exposure` doesn't
#' contain pack name in any form.
#'
#' @keywords internal
#'
#' @name single_exposure
NULL

#' Pack info
#'
#' An S3 class `pack_info` to represent information about pack in [single
#' exposure][single_exposure]. Its content is as in [packs_info] but without
#' column 'name'.
#'
#' @param .pack [Rule pack][rule-packs].
#' @param .remove_obeyers Value of `.remove_obeyers` argument of [expose()] with
#'   which `.pack` was applied.
#'
#' @keywords internal
#'
#' @name pack_info
NULL

#' Packs info
#'
#' An S3 class `packs_info` to represent information about packs in [exposure].
#' It is a tibble with the following structure:
#' - __name__ <chr> : Name of the pack.
#' - __type__ <chr> : [Pack type][rule-packs].
#' - __fun__ <list> : List (preferably unnamed) of rule pack functions.
#' - __remove_obeyers__ <lgl> : value of `.remove_obeyers` argument of
#' [expose()] with which pack was applied.
#'
#' @param .x Object to test.
#' @param .object Object to get `packs_info` value from `exposure` attribute.
#' @param .skip_class Whether to skip checking inheritance from `packs_info`.
#'
#' @details To avoid possible confusion it is preferred (but not required) that
#' list-column `fun` doesn't have names. Names of packs are stored in `name`
#' column. During [exposure][expose] `fun` is always created without names.
#'
#' @return `get_packs_info()` returns `packs_info` attribute of `object` if it
#'   is exposure and of its 'exposure' attribute otherwise.
#'
#' @examples
#' my_row_packs <- row_packs(
#'   row_mean_props = . %>% dplyr::transmute(row_mean = rowMeans(.)) %>%
#'     dplyr::transmute(
#'       row_mean_low = row_mean > 20,
#'       row_mean_high = row_mean < 60
#'     ),
#'   row_outlier = . %>% dplyr::transmute(row_sum = rowSums(.)) %>%
#'     dplyr::transmute(
#'       not_row_outlier = abs(row_sum - mean(row_sum)) / sd(row_sum) < 1.5
#'     )
#' )
#' my_data_packs <- data_packs(
#'   data_dims = . %>% dplyr::summarise(nrow = nrow(.) == 32,
#'                                      ncol = ncol(.) == 5)
#' )
#'
#' mtcars_exposed <- mtcars %>%
#'   expose(my_data_packs, .remove_obeyers = FALSE) %>%
#'   expose(my_row_packs)
#'
#' mtcars_exposed %>% get_packs_info()
#'
#' mtcars_exposed %>% get_packs_info() %>% is_packs_info()
#'
#' @name packs_info
NULL

#' Tidy data validation report
#'
#' A tibble representing the data validation result of certain data units in
#' tidy way:
#' - __pack__ <chr> : Name of rule pack from column 'name' of corresponding
#' [packs_info] object.
#' - __rule__ <chr> : Name of the rule defined in rule pack.
#' - __var__ <chr> : Name of the variable which validation result is reported.
#' Value '.all' is reserved and interpreted as 'all columns as a whole'.
#' __Note__ that `var` doesn't always represent the actual column in data frame
#' (see [group packs][group-pack]).
#' - __id__ <int> : Index of the row in tested data frame which validation
#' result is reported. Value 0 is reserved and interpreted as 'all rows as a
#' whole'.
#' - __value__ <lgl> : Whether the described data unit obeys the rule.
#'
#' @param .x Object to test.
#' @param .object Object to get `report` value from `exposure` attribute.
#' @param .skip_class Whether to skip checking inheritance from `ruler_report`.
#'
#' @details There are four basic combinations of `var` and `id` values which
#' define five basic data units:
#' - `var == '.all'` and `id == 0`: Data as a whole.
#' - `var != '.all'` and `id == 0`: Group (`var` shouldn't be an actual column
#'   name) or column (`var` should be an actual column name) as a whole.
#' - `var == '.all'` and `id != 0`: Row as a whole.
#' - `var != '.all'` and `id != 0`: Described cell.
#'
#' @return `get_report()` returns `report` element of `object` if it is
#' exposure and of its 'exposure' attribute otherwise.
#'
#' @examples
#' my_row_packs <- row_packs(
#'   row_mean_props = . %>% dplyr::transmute(row_mean = rowMeans(.)) %>%
#'     dplyr::transmute(
#'       row_mean_low = row_mean > 20,
#'       row_mean_high = row_mean < 60
#'     ),
#'   row_outlier = . %>% dplyr::transmute(row_sum = rowSums(.)) %>%
#'     dplyr::transmute(
#'       not_row_outlier = abs(row_sum - mean(row_sum)) / sd(row_sum) < 1.5
#'     )
#' )
#' my_data_packs <- data_packs(
#'   data_dims = . %>% dplyr::summarise(nrow = nrow(.) == 32,
#'                                      ncol = ncol(.) == 5)
#' )
#'
#' mtcars_exposed <- mtcars %>%
#'   expose(my_data_packs, .remove_obeyers = FALSE) %>%
#'   expose(my_row_packs)
#'
#' mtcars_exposed %>% get_report()
#'
#' mtcars_exposed %>% get_report() %>% is_report()
#'
#' @name ruler-report
NULL


# Constructors and converters ---------------------------------------------
new_exposure <- function(.packs_info, .report, .validate = TRUE) {
  if (.validate && !(is_packs_info(.packs_info, .skip_class = FALSE) &&
                     is_report(.report, .skip_class = FALSE))) {
    stop("Invalid input for `new_exposure`.")
  } else {
    structure(
      list(packs_info = .packs_info, report = .report),
      class = "exposure"
    )
  }
}

new_single_exposure <- function(.pack, .remove_obeyers, .report) {
  structure(
    list(pack_info = new_pack_info(.pack, .remove_obeyers),
         report = .report),
    class = "single_exposure"
  )
}

#' @rdname pack_info
new_pack_info <- function(.pack, .remove_obeyers) {
  pack_type <- class(.pack)[1]

  tibble(type = pack_type,
         fun = list(.pack),
         remove_obeyers = .remove_obeyers) %>%
    add_class("pack_info")
}

new_packs_info <- function(.names, .packs, .remove_obeyers) {
  packs_type <- vapply(.packs, function(x) {class(x)[1]}, "chr")
  # List-column 'fun' shouldn't have names because
  # pack name is stored in 'name' column.
  names(.packs) <- NULL

  tibble(name = .names,
         type = packs_type,
         fun = .packs,
         remove_obeyers = .remove_obeyers) %>%
    add_class("packs_info")
}

as_packs_info <- function(.x, .validate = TRUE) {
  if (.validate && !(is_packs_info(.x, .skip_class = TRUE))) {
    stop("as_packs_info: Invalid input.")
  } else {
    add_class_cond(.x, "packs_info")
  }
}

as_report <- function(.x, .validate = TRUE) {
  if (.validate && !(is_report(.x, .skip_class = TRUE))) {
    stop("as_report: Invalid input.")
  } else {
    add_class_cond(.x, "ruler_report")
  }
}


# Predicates --------------------------------------------------------------
#' @rdname exposure
#' @export
is_exposure <- function(.x) {
  inherits(.x, "exposure") &&
    is.list(.x) &&
    identical(names(.x), c("packs_info", "report")) &&
    is_packs_info(.x[["packs_info"]]) &&
    is_report(.x[["report"]])
}

#' @rdname packs_info
#' @export
is_packs_info <- function(.x, .skip_class = FALSE) {
  (.skip_class || inherits(.x, "packs_info")) &&
    is.data.frame(.x) &&
    ("tbl_df" %in% class(.x)) &&
    identical(names(.x), c("name", "type", "fun", "remove_obeyers")) &&
    is.character(.x[["name"]]) && is.character(.x[["type"]]) &&
    all(vapply(.x[["fun"]], rlang::is_function, TRUE)) &&
    is.logical(.x[["remove_obeyers"]])
}

#' @rdname ruler-report
#' @export
is_report <- function(.x, .skip_class = FALSE) {
  (.skip_class || inherits(.x, "ruler_report")) &&
    is.data.frame(.x) &&
    ("tbl_df" %in% class(.x)) &&
    identical(names(.x), c("pack", "rule", "var", "id", "value")) &&
    is.character(.x[["pack"]]) &&
    is.character(.x[["rule"]]) && is.character(.x[["var"]]) &&
    is.integer(.x[["id"]]) && is.logical(.x[["value"]])
}

is_obeyer <- function(.x) {
  if (!is.logical(.x)) {
    return(rep(FALSE, length.out = length(.x)))
  } else {
    .x %in% TRUE
  }
}


# Getters and setters -----------------------------------------------------
set_exposure <- function(.object, .exposure) {
  attr(.object, "exposure") <- .exposure

  .object
}

#' @rdname exposure
#' @export
get_exposure <- function(.object) {
  if (is_exposure(.object)) {
    .object
  } else {
    attr(.object, "exposure")
  }
}

#' @rdname exposure
#' @export
remove_exposure <- function(.object) {
  set_exposure(.object, NULL)
}

#' @rdname packs_info
#' @export
get_packs_info <- function(.object) {
  get_exposure(.object)[["packs_info"]]
}

#' @rdname ruler-report
#' @export
get_report <- function(.object) {
  get_exposure(.object)[["report"]]
}


# Printers ----------------------------------------------------------------
#' @export
print.exposure <- function(x, ..., .validate_packs_info = TRUE,
                           n_packs_info = NULL, width_packs_info = NULL,
                           n_extra_packs_info = NULL,
                           .validate_report = TRUE,
                           n_report = NULL, width_report = NULL,
                           n_extra_report = NULL) {
  cat("  Exposure\n\n")
  print(x[["packs_info"]], ..., .validate = .validate_packs_info,
        n = n_packs_info, width = width_packs_info,
        n_extra = n_extra_packs_info)
  cat("\n")
  print(x[["report"]], ..., .validate = .validate_report, n = n_report,
        width = width_report, n_extra = n_extra_report)

  invisible(x)
}

#' @export
print.packs_info <- function(x, ..., .validate = TRUE) {
  if (.validate && !is_packs_info(x)) {
    cat("Input is not proper 'packs_info' object.\n")
    return(invisible(x))
  }

  cat("Packs info:\n")

  NextMethod()
}

#' @export
print.ruler_report <- function(x, ..., .validate = TRUE) {
  if (.validate && !is_report(x)) {
    cat("Input is not proper 'ruler_report' object.\n")
    return(invisible(x))
  }

  cat("Tidy data validation report:\n")

  NextMethod()
}
