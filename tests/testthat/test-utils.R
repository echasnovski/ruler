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
