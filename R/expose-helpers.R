# General expose helpers --------------------------------------------------
guess_pack_type <- function(.pack_out, .rule_sep = inside_punct("\\._\\.")) {
  all_logical <- all(vapply(.pack_out, is.logical, TRUE))
  n_rows_one <- nrow(.pack_out) == 1
  all_contain_sep <- all(grepl(pattern = .rule_sep, x = colnames(.pack_out)))

  if (!all_logical) {
    return("group_pack")
  }

  if (n_rows_one) {
    if (all_contain_sep) {
      return("col_pack")
    } else {
      return("data_pack")
    }
  } else {
    if (all_contain_sep) {
      return("cell_pack")
    } else {
      return("row_pack")
    }
  }
}

remove_obeyers <- function(.report, .do_remove) {
  if (!.do_remove) {
    return(.report)
  } else {
    .report %>% filter(!is_obeyer(.data[["value"]]))
  }
}

impute_exposure_pack_names <- function(.single_exposures, .exposure_ref) {
  pack_names <- rlang::names2(.single_exposures)
  is_empty_pack_names <- pack_names == ""
  if (sum(is_empty_pack_names) == 0) {
    return(.single_exposures)
  }

  # Collect data about imputed pack types
  pack_types <- vapply(.single_exposures, function(cur_single_exposure) {
    cur_single_exposure[["pack_info"]][["type"]][1]
  }, "chr")
  pack_types_table <- table(pack_types)
  unique_pack_types <- names(pack_types_table)

  start_ind_vec <- rep(1, length(unique_pack_types))
  names(start_ind_vec) <- unique_pack_types

  # Account for reference pack types
  if (!identical(.exposure_ref, NULL)) {
    ref_pack_types <- .exposure_ref[["packs_info"]][["type"]]
    ref_pack_types_table <- table(ref_pack_types)
    common_pack_types <- intersect(unique_pack_types,
                                   names(ref_pack_types_table))

    start_ind_vec[common_pack_types] <-
      ref_pack_types_table[common_pack_types] + 1
  }

  # Impute
  def_names <- mapply(
    compute_def_names,
    .n = pack_types_table, .root = unique_pack_types,
    .start_ind = start_ind_vec,
    SIMPLIFY = FALSE
  ) %>%
    unsplit(f = pack_types)

  names(.single_exposures)[is_empty_pack_names] <-
    def_names[is_empty_pack_names]

  .single_exposures
}

#' Add pack names to single exposures
#'
#' Function to add pack names to single exposures. Converts list of
#' [single exposures][single_exposure] to list of [exposures][exposure] without
#' validating.
#'
#' @param .single_exposures List of [single exposures][single_exposure].
#'
#' @keywords internal
add_pack_names <- function(.single_exposures) {
  pack_names <- names(.single_exposures)

  lapply(pack_names, function(pack_name) {
    single_exposure <- .single_exposures[[pack_name]]

    # Add pack name to report
    report <- single_exposure[["report"]]
    new_report <- report
    new_report[["pack"]] <- rep(pack_name, nrow(report))
    new_report <- new_report[, c("pack", colnames(report))] %>%
      as_report(.validate = FALSE)

    # Add pack name to pack info and convert to `packs_info`
    packs_info <- single_exposure[["pack_info"]]
    packs_info[["name"]] <- rep(pack_name, nrow(packs_info))
    packs_info <-
      packs_info[, c("name", colnames(single_exposure[["pack_info"]]))] %>%
      as_packs_info(.validate = FALSE)

    new_exposure(packs_info, new_report, .validate = FALSE)
  }) %>%
    rlang::set_names(pack_names)
}


# Binder ------------------------------------------------------------------
#' Bind exposures
#'
#' Function to bind several exposures into one.
#'
#' @param ... Exposures to bind.
#' @param .validate_output Whether to validate with [is_exposure()] if the
#'   output is exposure.
#'
#' @details __Note__ that the output might not have names in list-column `fun`
#'   in [packs info][packs_info], which depends on version of
#'   [dplyr][dplyr::dplyr-package] package.
#'
#' @examples
#' my_data_packs <- data_packs(
#'   data_dims = . %>% dplyr::summarise(nrow_low = nrow(.) < 10),
#'   data_sum = . %>% dplyr::summarise(sum = sum(.) < 1000)
#' )
#'
#' ref_exposure <- mtcars %>% expose(my_data_packs) %>% get_exposure()
#'
#' exposure_1 <- mtcars %>% expose(my_data_packs[1]) %>% get_exposure()
#' exposure_2 <- mtcars %>% expose(my_data_packs[2]) %>% get_exposure()
#' exposure_binded <- bind_exposures(exposure_1, exposure_2)
#'
#' exposure_pipe <- mtcars %>%
#'   expose(my_data_packs[1]) %>% expose(my_data_packs[2]) %>%
#'   get_exposure()
#'
#' identical(exposure_binded, ref_exposure)
#'
#' identical(exposure_pipe, ref_exposure)
#'
#' @export
bind_exposures <- function(..., .validate_output = TRUE) {
  exposures <- rlang::dots_list(...) %>% rlang::squash() %>%
    filter_not_null()

  if (length(exposures) == 0) {
    return(NULL)
  }

  binded_packs_info <- lapply(exposures, `[[`, "packs_info") %>% bind_rows() %>%
    as_packs_info(.validate = FALSE)
  binded_report <- lapply(exposures, `[[`, "report") %>% bind_rows() %>%
    as_report(.validate = FALSE)

  new_exposure(binded_packs_info, binded_report, .validate = .validate_output)
}

filter_not_null <- function(.x) {
  is_null_x <- vapply(.x, identical, FUN.VALUE = TRUE, y = NULL)

  .x[!is_null_x]
}


# Assertions for pack outputs ---------------------------------------------
assert_pack_out_one_row <- function(.pack_out, .pack_type) {
  if (nrow(.pack_out) != 1) {
    stop(paste0("Some ", .pack_type, " has output with not 1 row."))
  }

  TRUE
}

assert_pack_out_all_logical <- function(.pack_out, .pack_type) {
  is_lgl_col <- vapply(.pack_out, is.logical, TRUE)

  if (all(is_lgl_col)) {
    return(TRUE)
  } else {
    stop(paste0("Some ", .pack_type, " has not logical output column"))
  }
}

assert_pack_out_all_have_separator <-
  function(.pack_out, .pack_type, .rule_sep) {
    has_sep <- grepl(pattern = .rule_sep, x = colnames(.pack_out))

    if (all(has_sep)) {
      return(TRUE)
    } else {
      stop(paste0("In some ", .pack_type, " not all columns contain rule ",
                  "separator"))
    }
  }
