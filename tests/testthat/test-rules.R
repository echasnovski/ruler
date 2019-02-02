context("rules")


# rules -------------------------------------------------------------------
test_that("rules works", {
  expect_identical(rules(), funs())

  output_1 <- rules(mean(.), sd = sd(., na.rm = TRUE), var, var = "var")
  output_ref_1 <- funs(
    ._.rule..1 = mean(.),
    ._.sd = sd(., na.rm = TRUE),
    ._.rule..3 = var,
    ._.var = "var"
  )

  expect_identical(output_1, output_ref_1)

  output_2 <- rules(mean, sd, .args = list(na.rm = TRUE))
  output_ref_2 <- funs(
    ._.rule..1 = mean,
    ._.rule..2 = sd,
    .args = list(na.rm = TRUE)
  )

  expect_identical(output_2, output_ref_2)

  output_3 <- rules(mean, sd = "sd", var, .prefix = "a_a_")
  output_ref_3 <- funs(
    'a_a_rule..1' = mean,
    'a_a_sd' = "sd",
    'a_a_rule..3' = var
  )

  expect_identical(output_3, output_ref_3)
})
