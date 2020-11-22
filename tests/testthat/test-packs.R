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
  output <- data_packs(!!!input)
  output_ref <- compute_output_ref(.extra_class = "data_pack")

  expect_identical(output, output_ref)
})


# group_packs -------------------------------------------------------------
test_that("group_packs works", {
  output_1 <- group_packs(!!!input, .group_vars = c("x", "y"))
  output_2 <- group_packs(
    !!!input,
    .group_vars = c("x", "y"),
    .group_sep = "+"
  )
  output_ref <- compute_output_ref(.extra_class = "group_pack") %>%
    lapply(`attr<-`, which = "group_vars", value = c("x", "y"))
  output_ref_1 <- lapply(output_ref, `attr<-`, which = "group_sep", value = ".")
  output_ref_2 <- lapply(output_ref, `attr<-`, which = "group_sep", value = "+")

  expect_identical(output_1, output_ref_1)
  expect_identical(output_2, output_ref_2)
})

test_that("group_packs throws errors", {
  expect_error(group_packs(!!!input, .group_vars = character(0)))
  expect_error(group_packs(!!!input, .group_vars = 1:2))

  expect_error(group_packs(!!!input, .group_vars = "a", .group_sep = 1))
  expect_error(
    group_packs(!!!input, .group_vars = "a", .group_sep = c("+", "-"))
  )
})


# col_packs ---------------------------------------------------------------
test_that("col_packs works", {
  output <- col_packs(!!!input)
  output_ref <- compute_output_ref(.extra_class = "col_pack")

  expect_identical(output, output_ref)
})


# row_packs ---------------------------------------------------------------
test_that("row_packs works", {
  output <- row_packs(!!!input)
  output_ref <- compute_output_ref(.extra_class = "row_pack")

  expect_identical(output, output_ref)
})


# cell_packs --------------------------------------------------------------
test_that("cell_packs works", {
  output <- cell_packs(!!!input)
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
  output <- squash_dots_rule_pack(!!!input[1:3], .extra_class = "extra")
  output_ref <- compute_output_ref(.extra_class = "extra")[1:3]

  expect_identical(output, output_ref)
})

test_that("squash_dots_rule_pack squashes", {
  output <- squash_dots_rule_pack(
    list(list(1L), list(2L, list(3L))),
    list(list(list(4L)), list(5L, list(6L))),
    .extra_class = "extra"
  )
  output_ref <- lapply(
    1:6,
    structure,
    class = c("extra", "rule_pack", "integer")
  )

  expect_identical(output, output_ref)
})


# print.data_pack ---------------------------------------------------------
test_that("print.data_pack works", {
  expect_output(print(data_packs(!!!input)[[1]]), "Data.*ule.*ack")
})


# print.group_pack --------------------------------------------------------
test_that("print.group_pack works", {
  expect_output(
    print(group_packs(!!!input, .group_vars = "a")[[1]]),
    "Group.*ule.*ack"
  )
})


# print.col_pack ----------------------------------------------------------
test_that("print.col_pack works", {
  expect_output(print(col_packs(!!!input)[[1]]), "Column.*ule.*ack")
})


# print.row_pack ----------------------------------------------------------
test_that("print.row_pack works", {
  expect_output(print(row_packs(!!!input)[[1]]), "Row.*ule.*ack")
})


# print.cell_pack ---------------------------------------------------------
test_that("print.cell_pack works", {
  expect_output(print(cell_packs(!!!input)[[1]]), "Cell.*ule.*ack")
})
