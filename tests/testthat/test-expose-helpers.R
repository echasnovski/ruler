context("expose-helpers")


# guess_pack_type ---------------------------------------------------------
test_that("guess_pack_type works", {
  expect_identical(guess_pack_type(input_data_pack_out), "data_pack")
  expect_identical(guess_pack_type(input_group_pack_out), "group_pack")
  expect_identical(guess_pack_type(input_col_pack_out), "col_pack")
  expect_identical(guess_pack_type(input_row_pack_out), "row_pack")
  expect_identical(guess_pack_type(input_cell_pack_out), "cell_pack")

  input_col_pack_out_1 <- input_col_pack_out
  names(input_col_pack_out_1) <-
    gsub("\\._\\.", "\\.___\\.", names(input_col_pack_out_1))

  expect_identical(
    guess_pack_type(
      input_col_pack_out_1,
      inside_punct("\\.___\\.")
    ),
    "col_pack"
  )
})


# remove_obeyers ----------------------------------------------------------
test_that("remove_obeyers works", {
  input_report <- tibble::tibble(
    pack = rep("data_pack", 4), rule = paste0("rule__", 1:4),
    var = rep(".all", 4), id = rep(0L, 4),
    value = c(TRUE, FALSE, TRUE, NA)
  )

  expect_identical(remove_obeyers(input_report, FALSE), input_report)
  expect_identical(remove_obeyers(input_report, TRUE), input_report[c(2, 4), ])
})


# impute_exposure_pack_names ----------------------------------------------
test_that("impute_exposure_pack_names works with NULL reference exposure", {
  expect_identical(
    impute_exposure_pack_names(input_single_exposures, input_exposure_ref),
    input_single_exposures
  )

  cur_input_single_exposures <- input_single_exposures
  names_remove_inds <- c(1, 2, 3, 5, 6, 8)
  names(cur_input_single_exposures)[names_remove_inds] <-
    rep("", length(names_remove_inds))

  expect_identical(
    names(impute_exposure_pack_names(cur_input_single_exposures, NULL)),
    c(
      "data_pack__1", "cell_pack__1", "col_pack__1", "new_col_proper_sums",
      "data_pack__2", "row_pack__1", "another_data_pack", "group_pack__1"
    )
  )
})

test_that("impute_exposure_pack_names works with not NULL reference exposure", {
  cur_input_single_exposures <- input_single_exposures
  names_remove_inds <- c(1, 2, 3, 5, 6, 8)
  names(cur_input_single_exposures)[names_remove_inds] <-
    rep("", length(names_remove_inds))

  expect_identical(
    names(impute_exposure_pack_names(
      cur_input_single_exposures,
      input_exposure_ref
    )),
    c(
      "data_pack__3", "cell_pack__2", "col_pack__3", "new_col_proper_sums",
      "data_pack__4", "row_pack__2", "another_data_pack", "group_pack__2"
    )
  )
})


# add_pack_names ----------------------------------------------------------
test_that("add_pack_names works", {
  expect_identical(
    add_pack_names(input_single_exposures),
    input_exposures
  )
})


# bind_exposures ----------------------------------------------------------
test_that("bind_exposures works", {
  expect_identical(
    bind_exposures(list(input_exposure_ref, NULL)),
    input_exposure_ref
  )
  expect_identical(
    bind_exposures(list(NULL, NULL)),
    NULL
  )

  output_ref <- new_exposure(
    .packs_info = new_packs_info(
      rep(input_exposure_ref$packs_info$name, 2),
      c(input_exposure_ref$packs_info$fun, input_exposure_ref$packs_info$fun),
      rep(input_exposure_ref$packs_info$remove_obeyers, 2)
    ),
    .report = bind_rows(
      input_exposure_ref$report,
      input_exposure_ref$report
    ) %>%
      add_class_cond("ruler_report")
  )

  expect_identical(
    bind_exposures(list(input_exposure_ref, input_exposure_ref)),
    output_ref
  )
  expect_identical(
    bind_exposures(input_exposure_ref, input_exposure_ref),
    output_ref
  )
})


# filter_not_null ---------------------------------------------------------
test_that("filter_not_null works", {
  input <- list(NULL, 1, list(2), NULL, "a", "b", list(NULL))
  output_ref <- input[-c(1, 4)]

  expect_identical(filter_not_null(input), output_ref)
})


# assert_pack_out_one_row -------------------------------------------------
test_that("assert_pack_out_one_row works", {
  expect_silent(assert_pack_out_one_row(input_data_pack_out, "data_pack"))
  expect_error(
    assert_pack_out_one_row(input_row_pack_out, "row_pack"),
    "row_pack.*not.*row"
  )
})


# assert_pack_out_all_logical ---------------------------------------------
test_that("assert_pack_out_all_logical works", {
  expect_silent(assert_pack_out_all_logical(input_data_pack_out, "data_pack"))

  input_bad <- tibble::tibble(good = c(TRUE, FALSE), bad = 1:2)

  expect_error(
    assert_pack_out_all_logical(input_bad, "cell_pack"),
    "cell_pack.*not.*logical"
  )
})


# assert_pack_out_all_have_separator --------------------------------------
test_that("assert_pack_out_all_have_separator works", {
  expect_silent(
    assert_pack_out_all_have_separator(
      input_col_pack_out, "col_pack", inside_punct("\\._\\.")
    )
  )
  expect_error(
    assert_pack_out_all_have_separator(
      input_data_pack_out, "data_pack", inside_punct("\\._\\.")
    ),
    "data_pack.*not.*separator"
  )
  expect_error(
    assert_pack_out_all_have_separator(
      input_col_pack_out, "col_pack", inside_punct("\\.___\\.")
    ),
    "col_pack.*not.*separator"
  )
})
