context("id")


# add_id ------------------------------------------------------------------
test_that("add_id works", {
  input <- tibble(id = 1, .id = 2, a = "3")
  output_ref <- tibble(.id1 = 1L, id = 1, .id = 2, a = "3")

  expect_equal(add_id(input), output_ref)
})


# add_id_key --------------------------------------------------------------
test_that("add_id_key works", {
  input_1 <- tibble(id = 1, .id = 2, a = "3")
  output_1 <- add_id_key(input_1)
  output_ref <- tibble(.id1 = 1L, id = 1, .id = 2, a = "3") %>%
    key_by(.id1)

  expect_identical(output_1, output_ref)

  input_2 <- tibble(id = 1, .id = 2, a = "3", .id1 = 2L) %>%
    key_by(.id1, .exclude = TRUE)
  output_2 <- add_id_key(input_2)

  expect_identical(output_2, output_ref)
})

test_that("add_id_key handles .add and .exclude arguments", {
  input <- tibble(id = 1, .id = 2, a = "3", .id1 = 2L) %>%
    key_by(.id1, .exclude = TRUE)
  output_1 <- add_id_key(input, .add = FALSE, .exclude = FALSE)
  output_ref_1 <- tibble(.id1 = 1L, id = 1, .id = 2, a = "3") %>%
    key_by(.id1, .add = FALSE, .exclude = FALSE)

  expect_identical(output_1, output_ref_1)

  output_2 <- add_id_key(input, .add = TRUE, .exclude = FALSE)
  output_ref_2 <- tibble(.id11 = 1L, id = 1, .id = 2, a = "3", .id1 = 2L) %>%
    key_by(.id1, .exclude = TRUE) %>%
    key_by(.id11, .add = TRUE, .exclude = FALSE)

  expect_identical(output_2, output_ref_2)

  output_3 <- add_id_key(input, .add = FALSE, .exclude = TRUE)
  output_ref_3 <- tibble(.id1 = 1L, id = 1, .id = 2, a = "3") %>%
    key_by(.id1, .add = FALSE, .exclude = TRUE)

  expect_identical(output_3, output_ref_3)

  output_4 <- add_id_key(input, .add = TRUE, .exclude = TRUE)
  output_ref_4 <- tibble(.id11 = 1L, id = 1, .id = 2, a = "3", .id1 = 2L) %>%
    key_by(.id1, .id11, .exclude = TRUE)

  expect_identical(output_4, output_ref_4)
})


# compute_id_name ---------------------------------------------------------
test_that("compute_id_name works", {
  expect_equal(compute_id_name(c("id", ".ID", "_id", "a.id", "..id", ".id12")),
               ".id")
  expect_equal(compute_id_name(c(".id")), ".id1")
  expect_equal(compute_id_name(c(".id1")), ".id")
  expect_equal(compute_id_name(c(".id11")), ".id")
  expect_equal(compute_id_name(c(".id", ".id1")), ".id11")
})
