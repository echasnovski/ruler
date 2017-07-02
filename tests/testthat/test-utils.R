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


# diff_tbl ----------------------------------------------------------------
test_that("diff_tbl works", {
  df_1 <- data.frame(x = 1:10, y = 2:11)
  df_2 <- data.frame(y = 3:12, z = 4:13)

  expect_identical(diff_tbl(df_1, df_2), data.frame(x = df_1$x))
  expect_silent(output_2 <- diff_tbl(df_1, df_1))
  expect_equal(dim(output_2), c(nrow(df_1), 0))
})

test_that("diff_tbl preserves class of .tbl1", {
  df_1 <- data.frame(x = 1:10, y = 2:11)
  df_2 <- tibble(y = 3:12, z = 4:13)

  expect_identical(diff_tbl(df_1, df_2), data.frame(x = df_1$x))

  df_3 <- tibble(x = 1:10, y = 2:11)
  df_4 <- data.frame(y = 3:12, z = 4:13)

  expect_identical(diff_tbl(df_3, df_4), tibble(x = df_1$x))
})


# assign_tbl --------------------------------------------------------------
test_that("assign_tbl works", {
  df_1 <- data.frame(y = 2:11, x = 1:10)
  df_2 <- data.frame(y = 3:12, z = letters[1:10], stringsAsFactors = FALSE)
  output <- assign_tbl(df_1, df_2)
  output_ref <- data.frame(y = df_2$y, x = df_1$x, z = df_2$z,
                           stringsAsFactors = FALSE)

  expect_identical(output, output_ref)
})

test_that("assign_tbl preserves class of .tbl1", {
  df_1 <- tibble(y = 2:11, x = 1:10)
  df_2 <- data.frame(y = 3:12, z = letters[1:10], stringsAsFactors = FALSE)
  output_1 <- assign_tbl(df_1, df_2)
  output_ref_1 <- tibble(y = df_2$y, x = df_1$x, z = df_2$z)

  expect_identical(output_1, output_ref_1)

  df_3 <- data.frame(y = 2:11, x = 1:10)
  df_4 <- tibble(y = 3:12, z = letters[1:10])
  output_2 <- assign_tbl(df_3, df_4)
  output_ref_2 <- data.frame(y = df_4$y, x = df_3$x, z = df_4$z,
                             stringsAsFactors = FALSE)

  expect_identical(output_2, output_ref_2)
})
