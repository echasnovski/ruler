# Results of some packs ---------------------------------------------------
input_data_pack_out <- dplyr::tibble('rule..1' = TRUE, 'nrow' = FALSE)
input_col_pack_out <- dplyr::tibble(
  'vs_._.rule..1' = TRUE, 'am_._.rule..1' = FALSE,
  'cyl_._.not_outlier' = TRUE, 'vs_._.not_outlier' = TRUE
)
input_row_pack_out <- dplyr::tibble(
  'row_rule..1' = rep(TRUE, 2),
  '._.rule..2' = c(TRUE, FALSE)
) %>% keyholder::assign_keys(dplyr::tibble(.id = c(1, 3)))
input_cell_pack_out <- dplyr::tibble(
  'vs_._.rule..1' = rep(TRUE, 2), 'am_._.rule..1' = rep(FALSE, 2),
  'cyl_._.not_outlier' = c(TRUE, FALSE), 'vs_._.not_outlier' = c(TRUE, FALSE)
) %>% keyholder::assign_keys(dplyr::tibble(.id = c(1, 4)))


# Exposure data -----------------------------------------------------------
input_packs <- list(
  data_packs(
    . %>% dplyr::summarise(
      nrow_low = nrow(.) > 10, nrow_high = nrow(.) < 20,
      ncol_low = ncol(.) > 5, ncol_high = ncol(.) < 10
    )
  )[[1]],
  col_packs(
    . %>% dplyr::summarise_if(rlang::is_integerish,
                              rules(tot_sum = sum(.) > 100))
  )[[1]],
  row_packs(
    . %>% dplyr::transmute(row_sum = rowSums(.)) %>%
      dplyr::transmute(
        outlier_sum = abs(row_sum - mean(row_sum)) / sd(row_sum) < 1
      ) %>%
      dplyr::slice(15:1)
  )[[1]],
  cell_packs(
    . %>% dplyr::transmute_if(
      Negate(rlang::is_integerish),
      rules(abs(. - mean(.)) / sd(.) < 2)
    )
  )[[1]],
  col_packs(
    . %>% dplyr::summarise_if(
      rlang::is_integerish,
      rules(tot_sum = sum(.) > 100,
            .prefix = "_._")
    )
  )[[1]],
  cell_packs(
    . %>% dplyr::transmute_if(
      Negate(rlang::is_integerish),
      rules(abs(. - mean(.)) / sd(.) < 2,
            .prefix = "_._")
    )
  )[[1]]
)
input_remove_obeyers <- c(TRUE, FALSE, TRUE, TRUE)
input_reports <- list(
  dplyr::tibble(
    rule = c("nrow_high", "ncol_high"),
    var = rep(".all", 2),
    id = rep(0L, 2),
    value = rep(FALSE, 2)
  ),
  dplyr::tibble(
    rule = rep("tot_sum", 6),
    var = c("cyl", "hp", "vs", "am", "gear", "carb"),
    id = rep(0L, 6),
    value = c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE)
  ),
  dplyr::tibble(
    rule = rep("outlier_sum", 2),
    var = rep(".all", 2),
    id = c(15L, 7L),
    value = rep(FALSE, 2)
  ),
  dplyr::tibble(
    rule = rep("rule..1", 7),
    var = c("mpg", "mpg", "drat", "wt", "wt", "wt", "qsec"),
    id = c(18L, 20L, 19L, 15L, 16L, 17L, 9L),
    value = rep(FALSE, 7)
  )
)

# Construction of exposure data
add_pack_name_to_single_report <- function(.report, .pack_name) {
  res <- .report
  res[["pack"]] <- rep(.pack_name, nrow(.report))

  res[, c("pack", colnames(.report))] %>% add_class("ruler_report")
}

single_exposure_inds <- c(1, 4, 2, 2, 1, 3, 1)
exposure_names <- c("data_dims", "cell_not_outlier", "col_proper_sums",
                    "new_col_proper_sums", "new_data_pack", "row_not_outlier",
                    "another_data_pack")

input_single_exposures <- mapply(
  new_single_exposure,
  input_packs[single_exposure_inds], input_remove_obeyers[single_exposure_inds],
  input_reports[single_exposure_inds],
  SIMPLIFY = FALSE
) %>%
  setNames(exposure_names)

input_exposures <- mapply(
  new_exposure,
  mapply(new_packs_info,
         exposure_names, lapply(input_packs[single_exposure_inds], list),
         input_remove_obeyers[single_exposure_inds],
         SIMPLIFY = FALSE),
  mapply(add_pack_name_to_single_report,
         input_reports[single_exposure_inds], exposure_names,
         SIMPLIFY = FALSE),
  SIMPLIFY = FALSE
) %>%
  setNames(exposure_names)

exposure_ref_inds <- c(2, 2, 4, 1, 1, 3)
exposure_ref_pack_names <- c("col_pack_n1", "col_pack_n2", "cell_pack_n1",
                             "data_pack_n1", "data_pack_n2", "row_pack_n1")
input_exposure_ref <- new_exposure(
  new_packs_info(exposure_ref_pack_names, input_packs[exposure_ref_inds],
                 input_remove_obeyers[exposure_ref_inds]),
  mapply(add_pack_name_to_single_report,
         input_reports[exposure_ref_inds], exposure_ref_pack_names,
         SIMPLIFY = FALSE) %>%
    dplyr::bind_rows() %>% as_report(.validate = FALSE)
)
