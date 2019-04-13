context("rules")


# rules -------------------------------------------------------------------
test_that("rules works with `dplyr` version lower than 0.8.0", {
  skip_if(packageVersion("dplyr") >= "0.8.0")

  expect_identical(rules(), funs())

  output_1 <- rules(mean(.), sd = sd(., na.rm = TRUE), var, var = "var")
  output_ref_1 <- funs(
    ._.rule__1 = mean(.),
    ._.sd = sd(., na.rm = TRUE),
    ._.rule__3 = var,
    ._.var = "var"
  )

  expect_identical(output_1, output_ref_1)

  output_2 <- rules(mean, sd, .args = list(na.rm = TRUE))
  output_ref_2 <- funs(
    ._.rule__1 = mean,
    ._.rule__2 = sd,
    .args = list(na.rm = TRUE)
  )

  expect_identical(output_2, output_ref_2)

  output_3 <- rules(mean, sd = "sd", var, .prefix = "a_a_")
  output_ref_3 <- funs(
    'a_a_rule__1' = mean,
    'a_a_sd' = "sd",
    'a_a_rule__3' = var
  )

  expect_identical(output_3, output_ref_3)
})

test_that("rules works with `dplyr` version higher than 0.8.0",  {
  skip_if(packageVersion("dplyr") < "0.8.0")

  output_1 <- rules(mean, "mean", mean(.), ~ mean(.))
  output_ref_1 <- list(
    ._.rule__1 = mean,
    ._.rule__2 = "mean",
    ._.rule__3 = ~ mean(.),
    ._.rule__4 = output_1[[4]]
  )

  expect_identical(output_1, output_ref_1)

  output_2 <- rules(~ mean(.), .prefix = "a_a_")
  output_ref_2 <- list(a_a_rule__1 = output_2[[1]])

  expect_identical(output_2, output_ref_2)

  expect_error(rules(mean2), "`mean2`")
})


# extract_funs_input ------------------------------------------------------
# Tested in `rules()`


# has_dot_symbol ----------------------------------------------------------
# Tested in `rules()`


# squash_expr -------------------------------------------------------------
# Tested in `rules()`


# quo_get_function --------------------------------------------------------
# Tested in `rules()`
