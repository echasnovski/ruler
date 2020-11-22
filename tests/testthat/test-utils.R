context("utils")


# Input data --------------------------------------------------------------
df <- mtcars


# inside_punct ------------------------------------------------------------
test_that("inside_punct works", {
  input1 <- c(
    "._.", "._.a", "a._.", "a._.a",
    "a_._.a", "a._._a", "a_._._a",
    "a__._._a", "a_._.__a", "a__._.__a",
    "._.*_.", "._.._.",
    "__.a", ".__a", "...", "a_a"
  )

  expect_identical(grep(inside_punct(), input1), 1:12)

  input2 <- c(
    "a", "_a", "a_", "_a_",
    "__a", "a__", "__a__",
    "_"
  )

  expect_identical(grep(inside_punct("a"), input2), 1:7)
})


# negate_select_cols ------------------------------------------------------
test_that("negate_select_cols works", {
  output_1 <- negate_select_cols(mtcars, vs, am)
  output_ref_1 <- setdiff(colnames(mtcars), c("vs", "am"))

  expect_identical(output_1, output_ref_1)

  output_2 <- negate_select_cols(mtcars, one_of("vs", "am"))
  output_ref_2 <- output_ref_1

  expect_identical(output_2, output_ref_2)

  output_3 <- negate_select_cols(mtcars, dplyr::matches("p|a"))
  output_ref_3 <- c("cyl", "wt", "qsec", "vs")

  expect_identical(output_3, output_ref_3)

  output_4 <- negate_select_cols(mtcars, cyl:am)
  output_ref_4 <- c("mpg", "gear", "carb")

  expect_identical(output_4, output_ref_4)

  output_5 <- negate_select_cols(mtcars, -(cyl:am))
  output_ref_5 <- c("cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am")

  expect_identical(output_5, output_ref_5)
})


# assert_positive_length --------------------------------------------------
test_that("assert_positive_length works", {
  expect_error(
    assert_positive_length(list(), "Some name"),
    "^Some name.*positive.*length"
  )

  expect_identical(assert_positive_length(1:2, "Some name"), 1:2)
  expect_identical(assert_positive_length(list(1:2), "Some name"), list(1:2))
})


# assert_length -----------------------------------------------------------
test_that("assert_length works", {
  expect_error(
    assert_length(c("a", "b"), 1, "New name"),
    "^New name.*length.*1"
  )
  expect_error(
    assert_length(1, 2, "New name"),
    "^New name.*length.*2"
  )

  expect_identical(assert_length(list("c"), 1, "New name"), list("c"))
})


# assert_character --------------------------------------------------------
test_that("assert_character works", {
  expect_error(
    assert_character(1L, "Tmp name"),
    "Tmp name.*character"
  )
  expect_error(
    assert_character(list("a"), "Tmp name"),
    "Tmp name.*character"
  )

  expect_identical(assert_positive_length(c("a", "A"), "Tmp name"), c("a", "A"))
})


# add_class ---------------------------------------------------------------
test_that("add_class works", {
  expect_equal(class(add_class(df, "some")), c("some", "data.frame"))
})


# add_class_cond ----------------------------------------------------------
test_that("add_class_cond works", {
  expect_equal(class(add_class_cond(df, "data.frame")), "data.frame")
  expect_equal(class(add_class_cond(df, "some")), c("some", "data.frame"))
})


# remove_class_cond -------------------------------------------------------
test_that("remove_class_cond works", {
  input <- structure(1, class = c("a", "b"))

  expect_equal(remove_class_cond(input, "a"), structure(1, class = "b"))
  expect_equal(remove_class_cond(input, "b"), input)
})


# compute_def_names -------------------------------------------------------
test_that("compute_def_names works", {
  expect_identical(compute_def_names(0), character(0))
  expect_identical(compute_def_names(10), paste0("__", seq_len(10)))
  expect_identical(compute_def_names(10, "base"), paste0("base__", seq_len(10)))
  expect_identical(
    compute_def_names(10, .start_ind = 4),
    paste0("__", seq_len(10) + 3)
  )
  expect_identical(
    compute_def_names(10, "base", 4),
    paste0("base__", seq_len(10) + 3)
  )
})


# enhance_names -----------------------------------------------------------
test_that("enhance_names works", {
  expect_identical(enhance_names(character(0)), character(0))

  input <- c("", "name", "", "name", "var")
  output_ref_1 <- c("__1", "name", "__3", "name", "var")

  expect_identical(enhance_names(input), output_ref_1)
  expect_identical(
    enhance_names(input, .prefix = "._."),
    paste0("._.", output_ref_1)
  )
  expect_identical(
    enhance_names(input, .suffix = "__"),
    paste0(output_ref_1, "__")
  )
  expect_identical(
    enhance_names(input, .prefix = "._.", .suffix = "__"),
    paste0("._.", output_ref_1, "__")
  )

  expect_identical(
    enhance_names(input, .root = "base"),
    c("base__1", "name", "base__3", "name", "var")
  )
  expect_identical(
    enhance_names(input, .root = "base", .start_ind = 5),
    c("base__5", "name", "base__7", "name", "var")
  )
})
