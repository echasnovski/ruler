context("id")


# add_id ------------------------------------------------------------------
test_that("add_id works", {
  input <- tibble(id = 1, .id = 2, a = 3)
  output_ref <- tibble(.id1 = 1L, id = 1, .id = 2, a = 3)

  expect_equal(add_id(input), output_ref)
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
