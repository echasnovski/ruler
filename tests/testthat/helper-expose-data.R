# Results of some packs ---------------------------------------------------
input_data_pack_out <- tibble::tibble('rule__1' = TRUE, 'nrow' = FALSE)
input_group_pack_out <- tibble::tibble(
  'vs' = c(0, 0, 1, 1), 'am' = c(0, 1, 0, 1),
  'n_low' = c(TRUE, FALSE, FALSE, FALSE),
  'n_high' = c(TRUE, TRUE, TRUE, TRUE)
)
input_col_pack_out <- tibble::tibble(
  'vs_._.rule__1' = TRUE, 'am_._.rule__1' = FALSE,
  'cyl_._.not_outlier' = TRUE, 'vs_._.not_outlier' = TRUE
)
input_row_pack_out <- tibble::tibble(
  'row_rule__1' = rep(TRUE, 2),
  '._.rule__2' = c(TRUE, FALSE)
) %>% keyholder::assign_keys(tibble::tibble(.id = c(1, 3)))
input_cell_pack_out <- tibble::tibble(
  'vs_._.rule__1' = rep(TRUE, 2), 'am_._.rule__1' = rep(FALSE, 2),
  'cyl_._.not_outlier' = c(TRUE, FALSE), 'vs_._.not_outlier' = c(TRUE, FALSE)
) %>% keyholder::assign_keys(tibble::tibble(.id = c(1, 4)))


# Exposure data -----------------------------------------------------------
input_packs <- list(
  data = data_packs(
    . %>% dplyr::summarise(
      nrow_low = nrow(.) > 10, nrow_high = nrow(.) < 20,
      ncol_low = ncol(.) > 5, ncol_high = ncol(.) < 10
    )
  )[[1]],
  group = group_packs(
    . %>% dplyr::group_by(vs, am) %>%
      dplyr::summarise(n_low = dplyr::n() > 10, n_high = dplyr::n() < 15) %>%
      dplyr::ungroup(),
    .group_vars = c("vs", "am"), .group_sep = "."
  )[[1]],
  col = col_packs(
    . %>% dplyr::summarise_if(rlang::is_integerish,
                              rules(tot_sum = sum(.) > 100))
  )[[1]],
  row = row_packs(
    . %>% dplyr::transmute(row_sum = rowSums(.)) %>%
      dplyr::transmute(
        outlier_sum = abs(row_sum - mean(row_sum)) / sd(row_sum) < 1
      ) %>%
      dplyr::slice(15:1)
  )[[1]],
  cell = cell_packs(
    . %>% dplyr::transmute_if(
      Negate(rlang::is_integerish),
      rules(abs(. - mean(.)) / sd(.) < 2)
    )
  )[[1]],
  col_other = col_packs(
    . %>% dplyr::summarise_if(
      rlang::is_integerish,
      rules(tot_sum = sum(.) > 100,
            .prefix = "_._")
    )
  )[[1]],
  cell_other = cell_packs(
    . %>% dplyr::transmute_if(
      Negate(rlang::is_integerish),
      rules(abs(. - mean(.)) / sd(.) < 2,
            .prefix = "_._")
    )
  )[[1]]
)
input_remove_obeyers <- c(data = TRUE, group = FALSE, col = FALSE,
                          row = TRUE, cell = TRUE)
input_reports <- list(
  data = tibble::tibble(
    rule = c("nrow_high", "ncol_high"),
    var = rep(".all", 2),
    id = rep(0L, 2),
    value = rep(FALSE, 2)
  ),
  group = tibble::tibble(
    rule = rep(c("n_low", "n_high"), each = 4),
    var = rep(c("0.0", "0.1", "1.0", "1.1"), times = 2),
    id = rep(0L, 8),
    value = c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
  ),
  col = tibble::tibble(
    rule = rep("tot_sum", 6),
    var = c("cyl", "hp", "vs", "am", "gear", "carb"),
    id = rep(0L, 6),
    value = c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE)
  ),
  row = tibble::tibble(
    rule = rep("outlier_sum", 2),
    var = rep(".all", 2),
    id = c(15L, 7L),
    value = rep(FALSE, 2)
  ),
  cell = tibble::tibble(
    rule = rep("rule__1", 7),
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

single_exposure_inds <- c("data", "cell", "col", "col", "data", "row", "data",
                          "group")
exposure_names <- c("data_dims", "cell_not_outlier", "col_proper_sums",
                    "new_col_proper_sums", "new_data_pack", "row_not_outlier",
                    "another_data_pack", "first_group_pack")

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

exposure_ref_inds <- c("col", "col", "cell", "data", "data", "row", "group")
exposure_ref_pack_names <- c("col_pack_n1", "col_pack_n2", "cell_pack_n1",
                             "data_pack_n1", "data_pack_n2", "row_pack_n1",
                             "group_pack_n1")
input_exposure_ref <- new_exposure(
  new_packs_info(exposure_ref_pack_names, input_packs[exposure_ref_inds],
                 input_remove_obeyers[exposure_ref_inds]),
  mapply(add_pack_name_to_single_report,
         input_reports[exposure_ref_inds], exposure_ref_pack_names,
         SIMPLIFY = FALSE) %>%
    dplyr::bind_rows() %>% as_report(.validate = FALSE)
)
