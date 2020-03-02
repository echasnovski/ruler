context("expose")


# Input data --------------------------------------------------------------
input_tbl <- mtcars %>% tibble::as_tibble()
input_tbl_keyed <- input_tbl %>% keyholder::use_id()

input_data_pack <- input_packs[["data"]]
input_data_pack_report_with_remove <- input_reports[["data"]]
input_data_pack_report_no_remove <- tibble::tibble(
  rule = c("nrow_low", "nrow_high", "ncol_low", "ncol_high"),
  var = rep(".all", 4),
  id = rep(0L, 4),
  value = c(TRUE, FALSE, TRUE, FALSE)
)

group_pack_ref <- . %>% group_by(vs, am) %>%
  summarise(n_low = dplyr::n() > 10, n_high = dplyr::n() < 15) %>%
  ungroup()
input_group_pack <- input_packs[["group"]]
input_group_pack_report_with_remove <- input_reports[["group"]] %>%
  filter(!(value %in% TRUE))
input_group_pack_report_no_remove <- input_reports[["group"]]

input_col_pack <- input_packs[["col"]]
input_col_pack_report_with_remove <- input_reports[["col"]] %>%
  filter(!(value %in% TRUE))
input_col_pack_report_no_remove <- input_reports[["col"]]

input_row_pack <- input_packs[["row"]]
input_row_pack_report_with_remove <- input_reports[["row"]]
input_row_pack_report_no_remove <- tibble::tibble(
  rule = rep("outlier_sum", 15),
  var = rep(".all", 15),
  id = 15:1,
  value = c(FALSE, rep(TRUE, 7), FALSE, rep(TRUE, 6))
)

input_cell_pack <- input_packs[["cell"]]
input_cell_pack_report_with_remove <- input_reports[["cell"]]
input_cell_pack_report_no_remove <- tibble::tibble(
  rule = rep("rule__1", 160),
  var = rep(c("mpg", "disp", "drat", "wt", "qsec"), each = 32),
  id = rep(1:32, 5),
  value = c(rep(TRUE, 17), FALSE, TRUE, FALSE, rep(TRUE, 62),
            FALSE, rep(TRUE, 27), rep(FALSE, 3), rep(TRUE, 23),
            FALSE, rep(TRUE, 23))
)


# Custom expectations -----------------------------------------------------
expect_error_expose_single <-
  function(.tbl, .pack, .error, .rule_sep = inside_punct("\\._\\."),
           .remove_obeyers = TRUE, .guess = TRUE) {
    expect_error(
      expose_single(
        .tbl = .tbl, .pack = .pack, .rule_sep = .rule_sep,
        .remove_obeyers = .remove_obeyers, .guess = .guess
      ),
      .error
    )
  }

expect_expose_single_works <- function(.tbl, .pack, .used_pack = .pack,
                                       .rule_sep = inside_punct("\\._\\."),
                                       .report_with_remove,
                                       .report_no_remove) {
  output_ref_remove <- new_single_exposure(
    .pack = .pack,
    .remove_obeyers = TRUE,
    .report = .report_with_remove
  )

  expect_identical(
    expose_single(.tbl = .tbl, .pack = .used_pack,
                  .rule_sep = .rule_sep,
                  .remove_obeyers = TRUE, .guess = TRUE),
    output_ref_remove
  )

  output_ref_no_remove <- new_single_exposure(
    .pack = .pack,
    .remove_obeyers = FALSE,
    .report = .report_no_remove
  )

  expect_identical(
    expose_single(.tbl = .tbl, .pack = .used_pack,
                  .rule_sep = .rule_sep,
                  .remove_obeyers = FALSE, .guess = TRUE),
    output_ref_no_remove
  )

  TRUE
}

expect_expose_single_default_works <-
  function(.tbl, .pack, .rule_sep = inside_punct("\\._\\."),
           .report_with_remove, .report_no_remove) {
    input_rule_pack <- .pack
    class(input_rule_pack) <- c("fseq", "function")

    expect_error_expose_single(
      .tbl = .tbl, .pack = input_rule_pack,
      .error = "unsupported", .rule_sep = .rule_sep,
      .remove_obeyers = TRUE, .guess = FALSE
    )

    expect_expose_single_works(
      .tbl = .tbl, .pack = .pack, .used_pack = input_rule_pack,
      .rule_sep = .rule_sep,
      .report_with_remove = .report_with_remove,
      .report_no_remove = .report_no_remove
    )
  }

expect_expose_guesses <- function(.tbl, .pack) {
  input_rule_pack <- .pack
  class(input_rule_pack) <- c("fseq", "function")

  expect_error(
    input_tbl %>% expose(input_rule_pack, .guess = FALSE),
    "unsupported"
  )

  expect_identical(
    input_tbl %>% expose(input_rule_pack, .guess = TRUE),
    input_tbl %>% expose(.pack)
  )
}


# expose ------------------------------------------------------------------
test_that("expose works", {
  # First expose
  expose_res_1 <- input_tbl %>%
    expose(input_data_pack, list(my_row_pack = input_row_pack),
           .remove_obeyers = TRUE, .guess = TRUE)

  output_ref_1 <- new_exposure(
    .packs_info = new_packs_info(
      .names = c("data_pack__1", "my_row_pack"),
      .packs = list(input_data_pack, input_row_pack),
      .remove_obeyers = c(TRUE, TRUE)
    ),
    .report = bind_rows(
      add_pack_name_to_single_report(
        .report = input_data_pack_report_with_remove,
        .pack_name = "data_pack__1"
      ),
      add_pack_name_to_single_report(
        .report = input_row_pack_report_with_remove,
        .pack_name = "my_row_pack"
      )
    ) %>% add_class_cond("ruler_report")
  )

  expect_equivalent(expose_res_1, input_tbl)
  expect_true(identical(get_exposure(expose_res_1), output_ref_1))

  # Second expose
  expose_res_2 <- expose_res_1 %>%
    expose(input_data_pack, list(list(input_col_pack)),
           .remove_obeyers = FALSE)

  output_ref_2 <- bind_exposures(
    output_ref_1,
    new_exposure(
      .packs_info = new_packs_info(
        .names = c("data_pack__2", "col_pack__1"),
        .packs = list(input_data_pack, input_col_pack),
        .remove_obeyers = c(FALSE, FALSE)
      ),
      .report = bind_rows(
        add_pack_name_to_single_report(
          input_data_pack_report_no_remove,
          "data_pack__2"
        ),
        add_pack_name_to_single_report(
          input_col_pack_report_no_remove,
          "col_pack__1"
        )
      ) %>% add_class_cond("ruler_report")
    )
  )

  expect_equivalent(expose_res_2, expose_res_1)
  expect_identical(get_exposure(expose_res_2), output_ref_2)
})

test_that("expose removes obeyers", {
  output_1 <- input_tbl %>% expose(input_data_pack, .remove_obeyers = TRUE) %>%
    get_exposure()
  output_ref_1 <- new_exposure(
    .packs_info = new_packs_info('data_pack__1', list(input_data_pack), TRUE),
    .report = add_pack_name_to_single_report(
        .report = input_data_pack_report_with_remove,
        .pack_name = "data_pack__1"
      )
    )

  expect_identical(output_1, output_ref_1)

  output_2 <- input_tbl %>% expose(input_data_pack, .remove_obeyers = FALSE) %>%
    get_exposure()
  output_ref_2 <- new_exposure(
    .packs_info = new_packs_info('data_pack__1', list(input_data_pack), FALSE),
    .report = add_pack_name_to_single_report(
      .report = input_data_pack_report_no_remove,
      .pack_name = "data_pack__1"
    )
  )

  expect_identical(output_2, output_ref_2)
})

test_that("expose preserves pack names", {
  output <- input_tbl %>%
    expose(my_data_pack = input_data_pack,
           list(my_col_pack = input_col_pack),
           list(list(my_row_pack = input_row_pack)),
           .remove_obeyers = TRUE) %>%
    get_exposure()
  output_ref <- new_exposure(
    .packs_info = new_packs_info(
      c("my_data_pack", "my_col_pack", "my_row_pack"),
      list(input_data_pack, input_col_pack, input_row_pack),
      c(TRUE, TRUE, TRUE)
    ),
    .report = bind_rows(
      add_pack_name_to_single_report(
        .report = input_data_pack_report_with_remove,
        .pack_name = "my_data_pack"
      ),
      add_pack_name_to_single_report(
        .report = input_col_pack_report_with_remove,
        .pack_name = "my_col_pack"
      ),
      add_pack_name_to_single_report(
        .report = input_row_pack_report_with_remove,
        .pack_name = "my_row_pack"
      )
    )
  )

  expect_identical(output, output_ref)
})

test_that("expose accounts for rule separator", {
  output <- input_tbl %>%
    expose(input_packs[["col_other"]], .rule_sep = inside_punct("_\\._"),
           .remove_obeyers = TRUE) %>%
    get_exposure()
  output_ref <- new_exposure(
    .packs_info = new_packs_info('col_pack__1',
                                 list(input_packs[["col_other"]]),
                                 TRUE),
    .report = add_pack_name_to_single_report(
      .report = input_col_pack_report_with_remove,
      .pack_name = "col_pack__1"
    )
  )

  expect_identical(output, output_ref)
})

test_that("expose guesses", {
  expect_expose_guesses(input_tbl, input_data_pack)
  expect_expose_guesses(input_tbl, input_col_pack)
  expect_expose_guesses(input_tbl, input_row_pack)
  expect_expose_guesses(input_tbl, input_cell_pack)
})


# expose_single.default ---------------------------------------------------
test_that("expose_single.default guesses data pack", {
  expect_expose_single_default_works(
    .tbl = input_tbl_keyed,
    .pack = input_data_pack,
    .report_with_remove = input_data_pack_report_with_remove,
    .report_no_remove = input_data_pack_report_no_remove
  )
})

test_that("expose_single.default guesses group pack", {
  expect_expose_single_default_works(
    .tbl = input_tbl_keyed,
    .pack = input_group_pack,
    .report_with_remove = input_group_pack_report_with_remove,
    .report_no_remove = input_group_pack_report_no_remove
  )
})

test_that("expose_single.default adds guessed attributes to group pack", {
  output_single_exposure <- expose_single(
    .tbl = input_tbl_keyed, .pack = group_pack_ref,
    .rule_sep = inside_punct("\\._\\."),
    .remove_obeyers = TRUE, .guess = TRUE
  )

  guessed_fun <- output_single_exposure[["pack_info"]][["fun"]][[1]]

  expect_identical(attr(guessed_fun, "group_vars"), c("vs", "am"))
  expect_true(is.character(attr(guessed_fun, "group_sep")))
  expect_true(length(attr(guessed_fun, "group_sep")) == 1L)
})

test_that("expose_single.default guesses col pack", {
  expect_expose_single_default_works(
    .tbl = input_tbl_keyed,
    .pack = input_col_pack,
    .report_with_remove = input_col_pack_report_with_remove,
    .report_no_remove = input_col_pack_report_no_remove
  )

  expect_expose_single_default_works(
    .tbl = input_tbl_keyed,
    .pack = input_packs[["col_other"]],
    .rule_sep = inside_punct("_\\._"),
    .report_with_remove = input_col_pack_report_with_remove,
    .report_no_remove = input_col_pack_report_no_remove
  )
})

test_that("expose_single.default guesses row pack", {
  expect_expose_single_default_works(
    .tbl = input_tbl_keyed,
    .pack = input_row_pack,
    .report_with_remove = input_row_pack_report_with_remove,
    .report_no_remove = input_row_pack_report_no_remove
  )
})

test_that("expose_single.default guesses cell pack", {
  expect_expose_single_default_works(
    .tbl = input_tbl_keyed,
    .pack = input_cell_pack,
    .report_with_remove = input_cell_pack_report_with_remove,
    .report_no_remove = input_cell_pack_report_no_remove
  )

  expect_expose_single_default_works(
    .tbl = input_tbl_keyed,
    .pack = input_packs[["cell_other"]],
    .rule_sep = inside_punct("_\\._"),
    .report_with_remove = input_cell_pack_report_with_remove,
    .report_no_remove = input_cell_pack_report_no_remove
  )
})


# expose_single.data_pack -------------------------------------------------
test_that("expose_single.data_pack works", {
  expect_expose_single_works(
    .tbl = input_tbl_keyed, .pack = input_data_pack,
    .report_with_remove = input_data_pack_report_with_remove,
    .report_no_remove = input_data_pack_report_no_remove
  )

  expect_error_expose_single(
    .tbl = input_tbl_keyed,
    .pack = data_packs(. %>% summarise(nrow = nrow(.)))[[1]],
    .error = "logical"
  )

  expect_error_expose_single(
    .tbl = input_tbl_keyed,
    .pack = data_packs(. %>% transmute_at("vs", list(~ . > 0)))[[1]],
    .error = "row"
  )
})


# expose_single.group_pack ------------------------------------------------
test_that("expose_single.group_pack works", {
  expect_expose_single_works(
    .tbl = input_tbl_keyed, .pack = input_group_pack,
    .report_with_remove = input_group_pack_report_with_remove,
    .report_no_remove = input_group_pack_report_no_remove
  )

  bad_pack_1 <- `attr<-`(input_group_pack, "group_vars", 1L)
  expect_error_expose_single(.tbl = input_tbl_keyed, .pack = bad_pack_1,
                             .error = "[Gg]roup var.*should.*character")

  bad_pack_2 <- `attr<-`(input_group_pack, "group_vars", character(0))
  expect_error_expose_single(.tbl = input_tbl_keyed, .pack = bad_pack_2,
                             .error = "[Gg]roup var.*should.*positive.*length")

  bad_pack_3 <- `attr<-`(input_group_pack, "group_sep", 1L)
  expect_error_expose_single(.tbl = input_tbl_keyed, .pack = bad_pack_3,
                             .error = "[Gg]roup sep.*should.*character")

  bad_pack_4 <- `attr<-`(input_group_pack, "group_sep", c(".", "+"))
  expect_error_expose_single(.tbl = input_tbl_keyed, .pack = bad_pack_4,
                             .error = "[Gg]roup sep.*should.*length.*1")
})


# expose_single.col_pack --------------------------------------------------
test_that("expose_single.col_pack works", {
  expect_expose_single_works(
    .tbl = input_tbl_keyed, .pack = input_col_pack,
    .report_with_remove = input_col_pack_report_with_remove,
    .report_no_remove = input_col_pack_report_no_remove
  )

  expect_expose_single_works(
    .tbl = input_tbl_keyed, .pack = input_packs[["col_other"]],
    .rule_sep = inside_punct("_\\._"),
    .report_with_remove = input_col_pack_report_with_remove,
    .report_no_remove = input_col_pack_report_no_remove
  )

  expect_error_expose_single(
    .tbl = input_tbl_keyed,
    .pack = col_packs(. %>% summarise_at("vs", rules(length = length(.))))[[1]],
    .error = "logical"
  )

  expect_error_expose_single(
    .tbl = input_tbl_keyed,
    .pack = col_packs(. %>% transmute_at("vs", rules(. > 0)))[[1]],
    .error = "row"
  )

  expect_error_expose_single(
    .tbl = input_tbl_keyed,
    .pack = col_packs(. %>% summarise_at("vs", list(sum = ~ sum(.) > 0)))[[1]],
    .error = "separator"
  )
})


# expose_single.row_pack --------------------------------------------------
test_that("expose_single.row_pack works", {
  expect_expose_single_works(
    .tbl = input_tbl_keyed, .pack = input_row_pack,
    .report_with_remove = input_row_pack_report_with_remove,
    .report_no_remove = input_row_pack_report_no_remove
  )

  expect_error_expose_single(
    .tbl = input_tbl_keyed,
    .pack = row_packs(. %>% transmute(row_mean = rowMeans(.)))[[1]],
    .error = "logical"
  )
})


# expose_single.cell_pack -------------------------------------------------
test_that("expose_single.cell_pack works", {
  expect_expose_single_works(
    .tbl = input_tbl_keyed, .pack = input_cell_pack,
    .report_with_remove = input_cell_pack_report_with_remove,
    .report_no_remove = input_cell_pack_report_no_remove
  )

  expect_expose_single_works(
    .tbl = input_tbl_keyed, .pack = input_packs[["cell_other"]],
    .rule_sep = inside_punct("_\\._"),
    .report_with_remove = input_cell_pack_report_with_remove,
    .report_no_remove = input_cell_pack_report_no_remove
  )

  expect_error_expose_single(
    .tbl = input_tbl_keyed,
    .pack = cell_packs(. %>% transmute_at("am", rules(. + 1)))[[1]],
    .error = "logical"
  )

  expect_error_expose_single(
    .tbl = input_tbl_keyed,
    .pack = cell_packs(. %>%
                         mutate(row_mean = rowMeans(.)) %>%
                         transmute_at("qsec", list(~ . > row_mean)))[[1]],
    .error = "separator"
  )
})


# interp_data_pack_out ----------------------------------------------------
test_that("interp_data_pack_out works", {
  output_ref <- tibble::tibble(
    rule = c("rule__1", "nrow"),
    var = rep(".all", 2),
    id = rep(0L, 2),
    value = c(TRUE, FALSE)
  )

  expect_identical(interp_data_pack_out(input_data_pack_out),
                   output_ref)

  input_bad_1 <- input_data_pack_out
  input_bad_1[[1]][1] <- "a"

  expect_error(interp_data_pack_out(input_bad_1), "logical")

  input_bad_2 <- bind_rows(input_data_pack_out, input_data_pack_out)

  expect_error(interp_data_pack_out(input_bad_2), "row")
})


# interp_group_pack_out ---------------------------------------------------
test_that("interp_group_pack_out works", {
  output_ref_single <- tibble::tibble(
    rule = rep(c("n_low", "n_high"), each = 2),
    var = rep(c("0", "1"), times = 2),
    id = rep(0L, 4),
    value = c(TRUE, FALSE, TRUE, TRUE)
  )
  output_ref_1 <- tibble::tibble(
    rule = rep(c("n_low", "n_high"), each = 4),
    var = rep(c("0.0", "0.1", "1.0", "1.1"), times = 2),
    id = rep(0L, 8),
    value = c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
  )

  expect_identical(
    interp_group_pack_out(input_group_pack_out, .group_vars = c("vs", "am"),
                          .group_sep = "."),
    output_ref_1
  )
  expect_identical(
    input_group_pack_out %>%
      slice(c(1, 3)) %>%
      select(-am) %>%
      interp_group_pack_out(.group_vars = "vs", .group_sep = "."),
    output_ref_single
  )

  output_ref_2 <- output_ref_1
  output_ref_2[["var"]] <- gsub("\\.", "__", output_ref_2[["var"]])

  expect_identical(
    interp_group_pack_out(input_group_pack_out, .group_vars = c("vs", "am"),
                          .group_sep = "__"),
    output_ref_2
  )

  expect_error(
    interp_group_pack_out(input_group_pack_out, .group_vars = "vs",
                          .group_sep = "."),
    "non-unique"
  )
  expect_error(
    input_group_pack_out %>%
      slice(c(1, 3)) %>%
      interp_group_pack_out(.group_vars = "vs", .group_sep = "."),
    "logical"
  )
})


# interp_col_pack_out -----------------------------------------------------
test_that("interp_col_pack_out works", {
  output_ref <- tibble::tibble(
    rule = c(rep("rule__1", 2), rep("not_outlier", 2)),
    var = c("vs", "am", "cyl", "vs"),
    id = rep(0L, 4),
    value = c(TRUE, FALSE, TRUE, TRUE)
  )

  expect_identical(
    interp_col_pack_out(input_col_pack_out,
                        inside_punct("\\._\\.")),
    output_ref
  )

  input_bad_1 <- input_col_pack_out
  input_bad_1[[1]][1] <- "a"

  expect_error(interp_col_pack_out(input_bad_1,
                                   inside_punct("\\._\\.")),
               "logical")

  input_bad_2 <- bind_rows(input_col_pack_out, input_col_pack_out)

  expect_error(interp_col_pack_out(input_bad_2,
                                   inside_punct("\\._\\.")),
               "row")

  input_bad_3 <- input_col_pack_out
  names(input_bad_3)[1] <- "vs...rule__1"

  expect_error(interp_col_pack_out(input_bad_3,
                                   inside_punct("\\._\\.")),
               "separator")
})


# interp_row_pack_out -----------------------------------------------------
test_that("interp_row_pack_out works", {
  output_ref <- tibble::tibble(
    rule = c(rep("row_rule__1", 2), rep("._.rule__2", 2)),
    var = rep(".all", 4),
    id = rep(c(1, 3), 2),
    value = c(TRUE, TRUE, TRUE, FALSE)
  )

  expect_identical(interp_row_pack_out(input_row_pack_out), output_ref)

  input_bad_1 <- input_row_pack_out
  input_bad_1[[1]] <- c("a", "b")

  expect_error(interp_row_pack_out(input_bad_1), "logical")
})


# interp_cell_pack_out ----------------------------------------------------
test_that("interp_cell_pack_out works", {
  output_ref <- tibble::tibble(
    rule = c(rep("rule__1", 4), rep("not_outlier", 4)),
    var = c(rep("vs", 2), rep("am", 2), rep("cyl", 2), rep("vs", 2)),
    id = rep(c(1, 4), 4),
    value = c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE)
  )

  expect_identical(interp_cell_pack_out(input_cell_pack_out,
                                        inside_punct("\\._\\.")),
                   output_ref)

  input_bad_1 <- input_cell_pack_out
  input_bad_1[[1]] <- c("c", "d")

  expect_error(interp_cell_pack_out(input_bad_1,
                                    inside_punct("\\._\\.")),
               "logical")

  input_bad_2 <- input_cell_pack_out
  names(input_bad_2)[1] <- "vs_...rule__1"

  expect_error(interp_cell_pack_out(input_bad_2,
                                    inside_punct("\\._\\.")),
               "separator")
})
