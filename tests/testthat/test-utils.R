context("utils")


# Input data --------------------------------------------------------------
df <- mtcars


# add_class ---------------------------------------------------------------
test_that("add_class works", {
  expect_equal(class(add_class(df, "some")), c("some", "data.frame"))
})


# add_class_cond ----------------------------------------------------------
test_that("add_class_cond works", {
  expect_equal(class(add_class_cond(df, "data.frame")), "data.frame")
  expect_equal(class(add_class_cond(df, "some")), c("some", "data.frame"))
})


# remove_class ------------------------------------------------------------
test_that("remove_class works", {
  class(df) <- c("some", "data.frame")

  expect_equal(class(remove_class(df)), "data.frame")
})


# remove_class_cond -------------------------------------------------------
test_that("remove_class_cond works", {
  class(df) <- c("some", "data.frame")

  expect_equal(class(remove_class_cond(df, "some")), "data.frame")
  expect_equal(class(remove_class_cond(df, "another")),
               c("some", "data.frame"))
})


# compute_def_names -------------------------------------------------------
test_that("compute_def_names works", {
  expect_identical(compute_def_names(0), character(0))
  expect_identical(compute_def_names(10), paste0("..", seq_len(10)))
  expect_identical(compute_def_names(10, "base"),
                   paste0("base..", seq_len(10)))
  expect_identical(compute_def_names(10, start_ind = 4),
                   paste0("..", seq_len(10) + 3))
  expect_identical(compute_def_names(10, "base", 4),
                   paste0("base..", seq_len(10) + 3))
})


# enhance_names -----------------------------------------------------------
test_that("enhance_names works", {
  expect_identical(enhance_names(character(0)), character(0))

  input <- c("", "name", "", "name", "var")
  output_ref_1 <- c("..1", "name", "..3", "name", "var")

  expect_identical(enhance_names(input), output_ref_1)
  expect_identical(
    enhance_names(input, prefix = "._."),
    paste0("._.", output_ref_1)
  )
  expect_identical(
    enhance_names(input, suffix = "__"),
    paste0(output_ref_1, "__")
  )
  expect_identical(
    enhance_names(input, prefix = "._.", suffix = "__"),
    paste0("._.", output_ref_1, "__")
  )

  expect_identical(
    enhance_names(input, root = "base"),
    c("base..1", "name", "base..3", "name", "var")
  )
  expect_identical(
    enhance_names(input, root = "base", start_ind = 5),
    c("base..5", "name", "base..7", "name", "var")
  )
})
