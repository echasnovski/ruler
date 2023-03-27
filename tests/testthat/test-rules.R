context("rules")


# rules -------------------------------------------------------------------
test_that("rules works", {
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
