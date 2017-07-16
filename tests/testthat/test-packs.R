context("packs")


# Input data --------------------------------------------------------------
input <- list(1, dot2 = "a", mean, list(new = 2, 3))

compute_output_ref <- function(.extra_class) {
  list(
    structure(1, class = c(.extra_class, "rule_pack", "numeric")),
    dot2 = structure("a", class = c(.extra_class, "rule_pack", "character")),
    structure(mean, class = c(.extra_class, "rule_pack", "function")),
    new = structure(2, class = c(.extra_class, "rule_pack", "numeric")),
    structure(3, class = c(.extra_class, "rule_pack", "numeric"))
  )
}


# data_packs --------------------------------------------------------------
test_that("data_packs works", {
  output <- data_packs(UQS(input))
  output_ref <- compute_output_ref(.extra_class = "data_pack")

  expect_identical(output, output_ref)
})


# col_packs ---------------------------------------------------------------
test_that("col_packs works", {
  output <- col_packs(UQS(input))
  output_ref <- compute_output_ref(.extra_class = "col_pack")

  expect_identical(output, output_ref)
})


# row_packs ---------------------------------------------------------------
test_that("row_packs works", {
  output <- row_packs(UQS(input))
  output_ref <- compute_output_ref(.extra_class = "row_pack")

  expect_identical(output, output_ref)
})


# cell_packs --------------------------------------------------------------
test_that("cell_packs works", {
  output <- cell_packs(UQS(input))
  output_ref <- compute_output_ref(.extra_class = "cell_pack")

  expect_identical(output, output_ref)
})


# squash_dots_rule_pack ---------------------------------------------------
test_that("squash_dots_rule_pack returns a list", {
  output <- squash_dots_rule_pack(1, .extra_class = "extra")
  output_ref <- list(structure(1, class = c("extra", "rule_pack", "numeric")))

  expect_identical(output, output_ref)
})

test_that("squash_dots_rule_pack returns a named list", {
  output <- squash_dots_rule_pack(UQS(input[1:3]), .extra_class = "extra")
  output_ref <- compute_output_ref(.extra_class = "extra")[1:3]

  expect_identical(output, output_ref)
})

test_that("squash_dots_rule_pack squashes", {
  output <- squash_dots_rule_pack(list(list(1L), list(2L, list(3L))),
                                 list(list(list(4L)), list(5L, list(6L))),
                                 .extra_class = "extra")
  output_ref <- lapply(1:6, structure,
                       class = c("extra", "rule_pack", "integer"))

  expect_identical(output, output_ref)
})
