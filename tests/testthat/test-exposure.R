context("exposure")


# Input data --------------------------------------------------------------
input_pack <- data_packs(. %>% summarise(nrow_low = nrow(.) > 10,
                                         nrow_high = nrow(.) < 20))[[1]]
input_remove_obeyers <- FALSE
input_packs_info <- tibble::tibble(
  name = "data_pack__1",
  type = "data_pack",
  fun = list(input_pack),
  remove_obeyers = input_remove_obeyers
) %>% add_class("packs_info")

input_single_report <- tibble::tibble(
  rule = c("nrow_low", "nrow_high"),
  var = rep(".all", 2),
  id = rep(0L, 2),
  value = c(TRUE, FALSE)
)
input_report <- input_single_report
input_report[["pack"]] <- rep("data_pack__1", 2)
input_report <- input_report %>% select(pack, everything()) %>%
  add_class("ruler_report")

input_exposure <- structure(
  list(packs_info = input_packs_info,
       report = input_report),
  class = "exposure"
)

tibble_class <- class(tibble::tibble())

print_packs_info_not_validate_output <- "Packs info.*[Tt]ibble"
print_report_not_validate_output <- "Tidy data validation report.*[Tt]ibble"
print_exposure_not_validate_output <-
  paste0(c("Exposure", print_packs_info_not_validate_output,
           print_report_not_validate_output),
         collapse = ".*")


# Custom expectations -----------------------------------------------------
expect_print_validates <- function(bad_input, validate_par_name,
                                   validate_output, not_validate_output) {
  expect_output(
    do.call(print, setNames(list(bad_input, TRUE), c("x", validate_par_name))),
    validate_output
  )
  expect_output(
    do.call(print, setNames(list(bad_input, FALSE), c("x", validate_par_name))),
    not_validate_output
  )
}


# new_exposure ------------------------------------------------------------
test_that("new_exposure works", {
  output <- new_exposure(
    .packs_info = input_packs_info,
    .report = input_report
  )
  output_ref <- input_exposure

  expect_identical(output, output_ref)
})

test_that("new_exposure validates input", {
  expect_error(new_exposure("a", input_report), "[Ii]nvalid")
  expect_error(new_exposure(input_packs_info, "input_report"), "[Ii]nvalid")

  expect_silent(new_exposure("a", input_report, .validate = FALSE))
})


# new_single_exposure -----------------------------------------------------
test_that("new_single_exposure works", {
  output <- new_single_exposure(
    .pack = input_pack,
    .remove_obeyers = input_remove_obeyers,
    .report = input_single_report
  )
  output_ref <- structure(
    list(pack_info = new_pack_info(input_pack, input_remove_obeyers),
         report = input_single_report),
    class = "single_exposure"
  )

  expect_identical(output, output_ref)
})


# new_pack_info -----------------------------------------------------------
test_that("new_pack_info works", {
  output <- new_pack_info(.pack = input_pack,
                          .remove_obeyers = input_remove_obeyers)
  output_ref <- input_packs_info[, c("type", "fun", "remove_obeyers")] %>%
    tibble::as_tibble() %>%
    add_class("pack_info")

  expect_true(identical(output, output_ref))
})


# new_packs_info -----------------------------------------------------------
test_that("new_packs_info works", {
  output <- new_packs_info(.names = "data_pack__1",
                           .packs = list(input_pack),
                           .remove_obeyers = input_remove_obeyers)

  expect_true(identical(output, input_packs_info))
})


# as_packs_info -----------------------------------------------------------
test_that("as_packs_info works", {
  input <- input_packs_info
  class(input) <- tibble_class

  expect_true(identical(as_packs_info(input), input_packs_info))
  expect_error(as_packs_info(input[, -1], .validate = TRUE), "[Ii]nvalid")
  expect_silent(as_packs_info(input[, -1], .validate = FALSE))
})


# as_report ---------------------------------------------------------------
test_that("as_report works", {
  input <- input_report
  class(input) <- tibble_class

  expect_true(identical(as_report(input), input_report))
  expect_error(as_report(input[, -1], .validate = TRUE), "[Ii]nvalid")
  expect_silent(as_report(input[, -1], .validate = FALSE))
})


# is_exposure -------------------------------------------------------------
test_that("is_exposure works", {
  output <- new_exposure(
    .packs_info = input_packs_info,
    .report = input_report
  )

  expect_true(is_exposure(output))
  expect_false(is_exposure(output[1]))
  expect_false(is_exposure(output[2]))

  output_1 <- output
  class(output_1) <- "something"

  expect_false(is_exposure(output_1))

  output_2 <- output
  names(output_2) <- c("pack_info", "report")

  expect_false(is_exposure(output_2))

  output_3 <- output
  output_3$packs_info[[1]] <- 1L

  expect_false(is_exposure(output_3))

  output_4 <- output
  output_4$report <- tibble::tibble(value = TRUE)

  expect_false(is_exposure(output_4))
})


# is_packs_info -----------------------------------------------------------
test_that("is_packs_info works", {
  output <- new_packs_info("name", list(input_pack), input_remove_obeyers)

  expect_true(is_packs_info(output))
  expect_false(is_packs_info(output[1]))
  expect_false(is_packs_info(output[2]))

  output_1 <- output
  class(output_1) <- c("pack_infos", tibble_class)

  expect_false(is_packs_info(output_1))
  expect_true(is_packs_info(output_1, .skip_class = TRUE))

  output_2 <- output
  names(output_2)[1] <- "info"

  expect_false(is_packs_info(output_2))

  output_3 <- output
  output_3[["name"]] <- 1

  expect_false(is_packs_info(output_3))

  output_4 <- output
  output_4[["type"]] <- 1

  expect_false(is_packs_info(output_4))

  output_5 <- output
  output_5[["fun"]] <- list("a")

  expect_false(is_packs_info(output_5))

  output_6 <- output
  output_6[["remove_obeyers"]] <- "a"

  expect_false(is_packs_info(output_6))
})


# is_report ---------------------------------------------------------------
test_that("is_report works", {
  output <- input_report

  expect_true(is_report(output))
  expect_false(is_report(as.list(output)))
  expect_false(is_report(as.data.frame(output)))

  output_1 <- output
  class(output_1) <- c("some_report", tibble_class)

  expect_false(is_report(output_1))
  expect_true(is_report(output_1, .skip_class = TRUE))

  output_2 <- output
  names(output_2)[1] <- "pack_name"

  expect_false(is_report(output_2))

  output_3 <- output
  output_3[["pack"]] <- rep(1L, 2)

  expect_false(is_report(output_3))

  output_4 <- output
  output_4[["rule"]] <- rep(1L, 2)

  expect_false(is_report(output_4))

  output_5 <- output
  output_5[["var"]] <- rep(1L, 2)

  expect_false(is_report(output_5))

  output_6 <- output
  output_6[["id"]] <- rep(1.0, 2)

  expect_false(is_report(output_6))

  output_7 <- output
  output_7[["value"]] <- rep(1L, 2)

  expect_false(is_report(output_7))
})


# is_obeyer ---------------------------------------------------------------
test_that("is_obeyer works", {
  expect_identical(is_obeyer(c(TRUE, FALSE, NA)),
                             c(TRUE, FALSE, FALSE))

  expect_identical(is_obeyer(c("TRUE", "FALSE", "a")),
                             c(FALSE, FALSE, FALSE))

  expect_identical(is_obeyer(c(1L, 0L)),
                   c(FALSE, FALSE))
})


# get_exposure ------------------------------------------------------------
test_that("get_exposure works", {
  input <- mtcars
  attr(input, "exposure") <- input_exposure

  expect_identical(get_exposure(mtcars), NULL)
  expect_identical(get_exposure(input), input_exposure)

  expect_identical(get_exposure(input_exposure), input_exposure)

  bad_exposure <- structure(list(some = "value"), class = "exposure")
  expect_identical(get_exposure(bad_exposure), NULL)
})


# set_exposure ------------------------------------------------------------
test_that("set_exposure works", {
  output <- set_exposure(mtcars, input_exposure)

  expect_identical(attr(output, "exposure"), input_exposure)
})


# remove_exposure ---------------------------------------------------------
test_that("remove_exposure works", {
  output <- set_exposure(mtcars, input_exposure)

  expect_identical(remove_exposure(output), mtcars)
})


# get_packs_info ----------------------------------------------------------
test_that("get_packs_info works", {
  input <- set_exposure(mtcars, input_exposure)

  expect_identical(get_packs_info(input), input_exposure$packs_info)
  expect_identical(get_packs_info(input_exposure), input_exposure$packs_info)
})


# get_report --------------------------------------------------------------
test_that("get_report works", {
  input <- set_exposure(mtcars, input_exposure)

  expect_identical(get_report(input), input_exposure$report)
  expect_identical(get_report(input_exposure), input_exposure$report)
})


# print.exposure ----------------------------------------------------------
test_that("print.exposure works", {
  expect_output(output <- print(input_exposure),
                print_exposure_not_validate_output)
  expect_identical(output, input_exposure)
})

test_that("print.exposure validates input", {
  input_1 <- input_exposure
  input_1[["packs_info"]][["name"]] <- rep(1, nrow(input_1[["packs_info"]]))

  expect_print_validates(
    input_1, ".validate_packs_info",
    paste0(c("Exposure", "not proper", "packs_info",
             "Tidy data validation report", "[Tt]ibble"),
           collapse = ".*"),
    print_exposure_not_validate_output)

  input_2 <- input_exposure
  input_2[["report"]][["pack"]] <- rep(1, nrow(input_2[["report"]]))

  expect_print_validates(
    input_2, ".validate_report",
    paste0(c("Exposure", "Packs info", "[Tt]ibble",
             "not proper", "ruler_report"),
           collapse = ".*"),
    print_exposure_not_validate_output)
})

test_that("print.exposure passes tibble options", {
  input_print_exposure <- lapply(1:30, function(i) {input_exposure}) %>%
    bind_exposures(.validate_output = TRUE)

  input_print_pack_info_tbl <- input_print_exposure$packs_info
  class(input_print_pack_info_tbl) <- class(tibble::tibble())

  input_print_report_tbl <- input_print_exposure$report
  class(input_print_report_tbl) <- class(tibble::tibble())

  # Option `n`
  output_ref_packs_info_n <- capture_output(
    print(input_print_pack_info_tbl, n = 13)
  )
  expect_output(
    print(input_print_exposure, n_packs_info = 13),
    output_ref_packs_info_n,
    fixed = TRUE
  )

  output_ref_report_n <- capture_output(
    print(input_print_report_tbl, n = 23)
  )
  expect_output(
    print(input_print_exposure, n_report = 23),
    output_ref_report_n,
    fixed = TRUE
  )

  # Option `width`
  output_ref_packs_info_width <- capture_output(
    print(input_print_pack_info_tbl, width = 30)
  )
  expect_output(
    print(input_print_exposure, width_packs_info = 30),
    output_ref_packs_info_width,
    fixed = TRUE
  )

  output_ref_report_width <- capture_output(
    print(input_print_report_tbl, width = 20)
  )
  expect_output(
    print(input_print_exposure, width_report = 20),
    output_ref_report_width,
    fixed = TRUE
  )

  # Option `n_extra`
  output_ref_packs_info_n_extra <- capture_output(
    print(input_print_pack_info_tbl, width = 30, n_extra = 1)
  )
  expect_output(
    print(input_print_exposure, width_packs_info = 30, n_extra_packs_info = 1),
    output_ref_packs_info_n_extra,
    fixed = TRUE
  )

  output_ref_report_n_extra <- capture_output(
    print(input_print_report_tbl, width = 20, n_extra = 1)
  )
  expect_output(
    print(input_print_exposure, width_report = 20, n_extra_report = 1),
    output_ref_report_n_extra,
    fixed = TRUE
  )
})


# print.packs_info --------------------------------------------------------
test_that("print.packs_info works", {
  expect_output(print(input_packs_info), print_packs_info_not_validate_output)
})

test_that("print.packs_info validates input", {
  bad_input <- input_exposure[["packs_info"]]
  bad_input[["name"]] <- rep(1, nrow(bad_input))

  expect_print_validates(
    bad_input, ".validate",
    "not proper.*packs_info",
    print_packs_info_not_validate_output
  )
})

test_that("print.packs_info handles extra arguments", {
  input_print_packs_info <- lapply(1:20, function(i) {input_packs_info}) %>%
    bind_rows() %>% as_packs_info()

  input_print_packs_info_tbl <- input_print_packs_info
  class(input_print_packs_info_tbl) <- class(tibble::tibble())

  output_ref_packs_info_n <- capture_output(
    print(input_print_packs_info_tbl, n = 11)
  )
  expect_output(
    print(input_print_packs_info, n = 11),
    output_ref_packs_info_n,
    fixed = TRUE
  )
})


# print.ruler_report ------------------------------------------------------
test_that("print.ruler_report works", {
  expect_output(print(input_report), print_report_not_validate_output)
})

test_that("print.ruler_report validates input", {
  bad_input <- input_exposure[["report"]]
  bad_input[["pack"]] <- rep(1, nrow(bad_input))

  expect_print_validates(
    bad_input, ".validate",
    "not proper.*ruler_report",
    print_report_not_validate_output
  )
})

test_that("print.ruler_report handles extra arguments", {
  input_print_ruler_report <- lapply(1:10, function(i) {input_report}) %>%
    bind_rows() %>% as_report()

  input_print_ruler_report_tbl <- input_print_ruler_report
  class(input_print_ruler_report_tbl) <- class(tibble::tibble())

  output_ref_ruler_report_n <- capture_output(
    print(input_print_ruler_report_tbl, n = 11)
  )
  expect_output(
    print(input_print_ruler_report, n = 11),
    output_ref_ruler_report_n,
    fixed = TRUE
  )
})
