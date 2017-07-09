context("keyed-df")


# Input data --------------------------------------------------------------
keys_mtcars <- mtcars %>% select(vs, am) %>% as_tibble()
assign_keys_mtcars <- . %>% assign_keys(keys_mtcars)

mtcars_df <- mtcars
mtcars_tbl <- as_tibble(mtcars)
mtcars_grouped <- mtcars_tbl %>% group_by(am, gear)
mtcars_rowwise <- rowwise(mtcars)

df <- mtcars
df_keyed <- df %>% key_by(vs, am)
keys_df <- keys(df_keyed)


# Expectations and funcitons ----------------------------------------------
expect_commute <- function(input, .f1, .f2, type = "identical") {
  output_1 <- input %>% .f1 %>% .f2
  output_2 <- input %>% .f2 %>% .f1

  if (type == "identical") {
    expect_identical(output_1, output_2)
  } else {
    expect_equal(output_1, output_2)
  }
}

expect_commute_with_keys <- function(input, .f, type = "identical") {
  expect_commute(input = input, .f1 = .f, .f2 = assign_keys_mtcars, type = type)
}

get_assign_permuted_keys <- function(.tbl = mtcars_df, .f,
                                     base_keys = keys_mtcars,
                                     tbl_groups = list()) {
  permutation <- .tbl %>%
    mutate(.id = 1:n()) %>%
    group_by(rlang::UQS(tbl_groups)) %>%
    .f() %>%
    pull(.id)

  . %>% assign_keys(base_keys[permutation, ])
}

# is_keyed_df -------------------------------------------------------------
test_that("is_keyed_df works", {
  expect_true(is_keyed_df(df_keyed))
  expect_false(is_keyed_df(df))

  class(df) <- c("keyed_df", "data.frame")

  expect_false(is_keyed_df(df))

  attr(df, "keys") <- seq_len(nrow(df) - 1)

  expect_false(is_keyed_df(df))

  attr(df, "keys") <- matrix(seq_len(nrow(df) - 1), ncol = 1)

  expect_false(is_keyed_df(df))

  attr(df, "keys") <- data.frame(x = seq_len(nrow(df) - 1))

  expect_false(is_keyed_df(df))

  df_bad_keyed <- add_class(df_keyed[[1]], "keyed_df")
  attr(df_bad_keyed, "keys") <- keys_df

  expect_false(is_keyed_df(df_bad_keyed))
})


# is.keyed_df -------------------------------------------------------------
test_that("is.keyed_df works", {
  expect_identical(is.keyed_df, is_keyed_df)
})


# print -------------------------------------------------------------------
test_that("print.keyed_df works", {
  expect_output(print(df_keyed), "keyed object.*vs.*am")

  expect_output(
    df_keyed %>% remove_keys(everything(), .unkey = FALSE) %>% print(),
    "keyed object.*no.*key"
  )
})


# [ -----------------------------------------------------------------------
test_that("`[.keyed_df` works", {
  i_idx <- 1:10
  j_idx <- 1:3
  output_1 <- df_keyed[i_idx, j_idx]
  output_ref_1 <- df[i_idx, j_idx] %>%
    assign_keys(keys_df[i_idx, ])

  expect_identical(output_1, output_ref_1)

  output_2 <- df_keyed[, j_idx]
  output_ref_2 <- df[, j_idx] %>%
    assign_keys(keys_df)

  expect_identical(output_2, output_ref_2)

  output_3 <- df_keyed[i_idx, logical(0)]
  output_ref_3 <- df[i_idx, logical(0)] %>%
    assign_keys(keys_df[i_idx, ])

  expect_identical(output_3, output_ref_3)
})


# select ------------------------------------------------------------------
test_that("select.keyed_df works", {
  select_f <- . %>% select(mpg, am, gear)

  expect_commute_with_keys(mtcars_df, select_f)
  expect_commute_with_keys(mtcars_tbl, select_f)
  expect_commute_with_keys(mtcars_grouped, select_f)
  expect_commute_with_keys(mtcars_rowwise, select_f)
})

test_that("select_all works", {
  select_all_f <- . %>% select_all(toupper)

  expect_commute_with_keys(mtcars_df, select_all_f)
  expect_commute_with_keys(mtcars_tbl, select_all_f)
  expect_commute_with_keys(mtcars_grouped, select_all_f)
  expect_commute_with_keys(mtcars_rowwise, select_all_f)
})

test_that("select_if works", {
  is_whole <- function(x) all(floor(x) == x)
  select_if_f <- . %>% select_if(is_whole, toupper)

  expect_commute_with_keys(mtcars_df, select_if_f)
  expect_commute_with_keys(mtcars_tbl, select_if_f)
  expect_commute_with_keys(mtcars_grouped, select_if_f)
  expect_commute_with_keys(mtcars_rowwise, select_if_f)
})

test_that("select_at works", {
  select_at_f <- . %>% select_at(vars(mpg, hp))

  expect_commute_with_keys(mtcars_df, select_at_f)
  expect_commute_with_keys(mtcars_tbl, select_at_f)
  expect_commute_with_keys(mtcars_grouped, select_at_f)
  expect_commute_with_keys(mtcars_rowwise, select_at_f)
})

# rename ------------------------------------------------------------------
test_that("rename.keyed_df works", {
  rename_f <- . %>% rename(newMPG = mpg)

  expect_commute_with_keys(mtcars_df, rename_f)
  expect_commute_with_keys(mtcars_tbl, rename_f)
  expect_commute_with_keys(mtcars_grouped, rename_f)
  expect_commute_with_keys(mtcars_rowwise, rename_f)
})

test_that("rename_all works", {
  rename_all_f <- . %>% rename_all(toupper)

  expect_commute_with_keys(mtcars_df, rename_all_f)
  expect_commute_with_keys(mtcars_tbl, rename_all_f)
  # No mtcars_grouped check because scoped renaming works badly on grouped_df
  expect_commute_with_keys(mtcars_rowwise, rename_all_f)
})

test_that("rename_if works", {
  is_not_whole <- function(x) any(floor(x) != x)
  rename_if_f <- . %>% rename_if(is_not_whole, toupper)

  expect_commute_with_keys(mtcars_df, rename_if_f)
  expect_commute_with_keys(mtcars_tbl, rename_if_f)
  # No mtcars_grouped check because scoped renaming works badly on grouped_df
  expect_commute_with_keys(mtcars_rowwise, rename_if_f)
})

test_that("rename_at works", {
  rename_at_f <- . %>% rename_at(vars(gear, vs), toupper)

  expect_commute_with_keys(mtcars_df, rename_at_f)
  expect_commute_with_keys(mtcars_tbl, rename_at_f)
  # No mtcars_grouped check because scoped renaming works badly on grouped_df
  expect_commute_with_keys(mtcars_rowwise, rename_at_f)
})

# mutate ------------------------------------------------------------------
test_that("mutate.keyed_df works", {
  mutate_f <- . %>% mutate(mpg = 0, vs = gear * gear)

  expect_commute_with_keys(mtcars_df, mutate_f)
  expect_commute_with_keys(mtcars_tbl, mutate_f)
  expect_commute_with_keys(mtcars_grouped, mutate_f)
  expect_commute_with_keys(mtcars_rowwise, mutate_f)
})

test_that("mutate_all works", {
  mutate_all_f <- . %>% mutate_all(as.character)

  expect_commute_with_keys(mtcars_df, mutate_all_f)
  expect_commute_with_keys(mtcars_tbl, mutate_all_f)
  expect_commute_with_keys(mtcars_grouped, mutate_all_f)
  expect_commute_with_keys(mtcars_rowwise, mutate_all_f)
})

test_that("mutate_if works", {
  mutate_if_f <- . %>% mutate_if(is.double, as.integer)

  expect_commute_with_keys(mtcars_df, mutate_if_f)
  expect_commute_with_keys(mtcars_tbl, mutate_if_f)
  expect_commute_with_keys(mtcars_grouped, mutate_if_f)
  expect_commute_with_keys(mtcars_rowwise, mutate_if_f)
})

test_that("mutate_at works", {
  mutate_at_f <- . %>% mutate_at(vars(mpg, vs), as.integer)

  expect_commute_with_keys(mtcars_df, mutate_at_f)
  expect_commute_with_keys(mtcars_tbl, mutate_at_f)
  expect_commute_with_keys(mtcars_grouped, mutate_at_f)
  expect_commute_with_keys(mtcars_rowwise, mutate_at_f)
})


# transmute ---------------------------------------------------------------
test_that("transmute.keyed_df works", {
  transmute_f <- . %>% transmute(mpg = 1, vs = gear)

  expect_commute_with_keys(mtcars_df, transmute_f)
  expect_commute_with_keys(mtcars_tbl, transmute_f)
  expect_commute_with_keys(mtcars_grouped, transmute_f)
  expect_commute_with_keys(mtcars_rowwise, transmute_f)
})

test_that("transmute_all works", {
  transmute_all_f <- . %>% transmute_all(as.character)

  expect_commute_with_keys(mtcars_df, transmute_all_f)
  expect_commute_with_keys(mtcars_tbl, transmute_all_f)
  # No mtcars_grouped check because grouping variables can't be modified
  expect_commute_with_keys(mtcars_rowwise, transmute_all_f)
})

test_that("transmute_if works", {
  transmute_if_f <- . %>% transmute_if(is.double, as.integer)

  expect_commute_with_keys(mtcars_df, transmute_if_f)
  expect_commute_with_keys(mtcars_tbl, transmute_if_f)
  # No mtcars_grouped check because grouping variables can't be modified
  expect_commute_with_keys(mtcars_rowwise, transmute_if_f)
})

test_that("transmute_at works", {
  transmute_at_f <- . %>% transmute_at(vars(mpg, hp), as.integer)

  expect_commute_with_keys(mtcars_df, transmute_at_f)
  expect_commute_with_keys(mtcars_tbl, transmute_at_f)
  # No mtcars_grouped check because grouping variables can't be modified
  expect_commute_with_keys(mtcars_rowwise, transmute_at_f)
})


# summarise ---------------------------------------------------------------
test_that("summarise.keyed_df works", {
  summarise_f <- . %>% summarise(meanGear = mean(gear))

  expect_false(mtcars_df %>% key_by(vs) %>% summarise_f() %>% is_keyed_df())
  expect_false(mtcars_tbl %>% key_by(vs) %>% summarise_f() %>% is_keyed_df())
  expect_false(mtcars_grouped %>% key_by(vs) %>% summarise_f() %>%
                 is_keyed_df())
  expect_false(mtcars_rowwise %>% key_by(vs) %>% summarise_f() %>%
                 is_keyed_df())
})

test_that("summarise_all works", {
  summarise_all_f <- . %>% summarise_all(funs(n()))

  expect_false(mtcars_df %>% key_by(vs) %>% summarise_all_f() %>% is_keyed_df())
  expect_false(mtcars_tbl %>% key_by(vs) %>% summarise_all_f() %>%
                 is_keyed_df())
  expect_false(mtcars_grouped %>% key_by(vs) %>% summarise_all_f() %>%
                 is_keyed_df())
  expect_false(mtcars_rowwise %>% key_by(vs) %>% summarise_all_f() %>%
                 is_keyed_df())
})

test_that("summarise_if works", {
  summarise_if_f <- . %>% summarise_if(is.double, mean)

  expect_false(mtcars_df %>% key_by(vs) %>% summarise_if_f() %>% is_keyed_df())
  expect_false(mtcars_tbl %>% key_by(vs) %>% summarise_if_f() %>% is_keyed_df())
  expect_false(mtcars_grouped %>% key_by(vs) %>% summarise_if_f() %>%
                 is_keyed_df())
  expect_false(mtcars_rowwise %>% key_by(vs) %>% summarise_if_f() %>%
                 is_keyed_df())
})

test_that("summarise_at works", {
  summarise_at_f <- . %>% summarise_at(vars(mpg, vs), mean)

  expect_false(mtcars_df %>% key_by(vs) %>% summarise_at_f() %>% is_keyed_df())
  expect_false(mtcars_tbl %>% key_by(vs) %>% summarise_at_f() %>% is_keyed_df())
  expect_false(mtcars_grouped %>% key_by(vs) %>% summarise_at_f() %>%
                 is_keyed_df())
  expect_false(mtcars_rowwise %>% key_by(vs) %>% summarise_at_f() %>%
                 is_keyed_df())
})


# group_by ----------------------------------------------------------------
test_that("group_by.keyed_df works", {
  group_by_f <- . %>% group_by(gear, vs)

  expect_commute_with_keys(mtcars_df, group_by_f)
  expect_commute_with_keys(mtcars_tbl, group_by_f)
  expect_commute_with_keys(mtcars_grouped, group_by_f)
  suppressWarnings(expect_commute_with_keys(mtcars_rowwise, group_by_f))
})

test_that("group_by_all works", {
  group_by_all_f <- . %>% group_by_all(as.character)

  expect_commute_with_keys(mtcars_df, group_by_all_f)
  expect_commute_with_keys(mtcars_tbl, group_by_all_f)
  expect_commute_with_keys(mtcars_grouped, group_by_all_f)
  suppressWarnings(expect_commute_with_keys(mtcars_rowwise, group_by_all_f))
})

test_that("group_by_if works", {
  group_by_if_f <- . %>% group_by_if(is.double, as.integer)

  expect_commute_with_keys(mtcars_df, group_by_if_f)
  expect_commute_with_keys(mtcars_tbl, group_by_if_f)
  expect_commute_with_keys(mtcars_grouped, group_by_if_f)
  suppressWarnings(expect_commute_with_keys(mtcars_rowwise, group_by_if_f))
})

test_that("group_by_at works", {
  group_by_at_f <- . %>% group_by_at(vars(mpg, vs), as.integer)

  expect_commute_with_keys(mtcars_df, group_by_at_f)
  expect_commute_with_keys(mtcars_tbl, group_by_at_f)
  expect_commute_with_keys(mtcars_grouped, group_by_at_f)
  suppressWarnings(expect_commute_with_keys(mtcars_rowwise, group_by_at_f))
})

# ungroup -----------------------------------------------------------------
test_that("ungroup.keyed_df works", {
  expect_commute_with_keys(mtcars_grouped, ungroup)
  suppressWarnings(expect_commute_with_keys(mtcars_rowwise, ungroup))
})


# rowwise.keyed_df --------------------------------------------------------
test_that("rowwise.keyed_df works", {
  expect_commute_with_keys(mtcars_df, rowwise.keyed_df)
  expect_commute_with_keys(mtcars_tbl, rowwise.keyed_df)
})

# arrange -----------------------------------------------------------------
test_that("arrange.keyed_df works", {
  arrange_f <- . %>% arrange(vs, desc(mpg))
  assign_perm_keys <- get_assign_permuted_keys(.f = arrange_f)

  output_1_f <- . %>% arrange_f() %>% assign_perm_keys()
  output_2_f <- . %>% assign_keys_mtcars() %>% arrange_f()

  expect_identical(output_1_f(mtcars_df), output_2_f(mtcars_df))
  expect_identical(output_1_f(mtcars_tbl), output_2_f(mtcars_tbl))
  expect_identical(output_1_f(mtcars_grouped), output_2_f(mtcars_grouped))
  expect_identical(output_1_f(mtcars_rowwise), output_2_f(mtcars_rowwise))

  arrange_f_by_group <- . %>% arrange(vs, desc(mpg), .by_group = TRUE)
  assign_perm_keys_by_group <- get_assign_permuted_keys(
    .f = arrange_f_by_group,
    tbl_groups = groups(mtcars_grouped)
  )

  output_1_f_by_group <- . %>% arrange_f_by_group() %>%
    assign_perm_keys_by_group()
  output_2_f_by_group <- . %>% assign_keys_mtcars() %>% arrange_f_by_group()

  expect_identical(
    output_1_f_by_group(mtcars_grouped),
    output_2_f_by_group(mtcars_grouped)
  )
})

test_that("arrange_all works", {
  arrange_all_f <- . %>% arrange_all(desc)
  assign_perm_keys <- get_assign_permuted_keys(.f = arrange_all_f)

  output_1_f <- . %>% arrange_all_f() %>% assign_perm_keys()
  output_2_f <- . %>% assign_keys_mtcars() %>% arrange_all_f()

  expect_identical(output_1_f(mtcars_df), output_2_f(mtcars_df))
  expect_identical(output_1_f(mtcars_tbl), output_2_f(mtcars_tbl))
  expect_identical(output_1_f(mtcars_grouped), output_2_f(mtcars_grouped))
  expect_identical(output_1_f(mtcars_rowwise), output_2_f(mtcars_rowwise))
})

test_that("arrange_if works", {
  # This is also a test for 'invisibility' of .id column
  arrange_if_f <- . %>% arrange_if(rlang::is_integerish)
  integerish_cols <- names(mtcars_df)[sapply(mtcars_df, rlang::is_integerish)]
  permutation <- mtcars_df %>%
    mutate(.id = 1:n()) %>%
    arrange_at(vars(integerish_cols)) %>%
    pull(.id)
  assign_perm_keys <- . %>% assign_keys(keys_mtcars[permutation, ])

  output_1_f <- . %>% arrange_if_f() %>% assign_perm_keys()
  output_2_f <- . %>% assign_keys_mtcars() %>% arrange_if_f()

  expect_identical(output_1_f(mtcars_df), output_2_f(mtcars_df))
  expect_identical(output_1_f(mtcars_tbl), output_2_f(mtcars_tbl))
  expect_identical(output_1_f(mtcars_grouped), output_2_f(mtcars_grouped))
  expect_identical(output_1_f(mtcars_rowwise), output_2_f(mtcars_rowwise))
})

test_that("arrange_at works", {
  arrange_at_f <- . %>% arrange_at(vars("disp", "qsec"))
  assign_perm_keys <- get_assign_permuted_keys(.f = arrange_at_f)

  output_1_f <- . %>% arrange_at_f() %>% assign_perm_keys()
  output_2_f <- . %>% assign_keys_mtcars() %>% arrange_at_f()

  expect_identical(output_1_f(mtcars_df), output_2_f(mtcars_df))
  expect_identical(output_1_f(mtcars_tbl), output_2_f(mtcars_tbl))
  expect_identical(output_1_f(mtcars_grouped), output_2_f(mtcars_grouped))
  expect_identical(output_1_f(mtcars_rowwise), output_2_f(mtcars_rowwise))
})


# filter ------------------------------------------------------------------
test_that("filter.keyed_df works", {
  filter_f <- . %>% filter(gear == 4, am == 1)
  assign_perm_keys <- get_assign_permuted_keys(.f = filter_f)

  output_1_f <- . %>% filter_f() %>% assign_perm_keys()
  output_2_f <- . %>% assign_keys_mtcars() %>% filter_f()

  expect_identical(output_1_f(mtcars_df), output_2_f(mtcars_df))
  expect_identical(output_1_f(mtcars_tbl), output_2_f(mtcars_tbl))
  expect_identical(output_1_f(mtcars_grouped), output_2_f(mtcars_grouped))
  expect_identical(output_1_f(mtcars_rowwise), output_2_f(mtcars_rowwise))
})

test_that("filter_all works", {
  filter_all_f <- . %>% filter_all(all_vars(. > 0))
  assign_perm_keys <- get_assign_permuted_keys(.f = filter_all_f)

  output_1_f <- . %>% filter_all_f() %>% assign_perm_keys()
  output_2_f <- . %>% assign_keys_mtcars() %>% filter_all_f()

  expect_identical(output_1_f(mtcars_df), output_2_f(mtcars_df))
  expect_identical(output_1_f(mtcars_tbl), output_2_f(mtcars_tbl))
  # filter_all removes grouping variables before applying filter so the result
  # is different for mtcars_grouped
  expect_identical(output_1_f(mtcars_rowwise), output_2_f(mtcars_rowwise))

  # Checking mtcars_grouped
  assign_perm_keys_grouped <- get_assign_permuted_keys(
    .f = filter_all_f,
    tbl_groups = groups(mtcars_grouped)
  )

  output_1_f_grouped <- . %>% filter_all_f() %>% assign_perm_keys_grouped()

  expect_identical(output_1_f_grouped(mtcars_grouped),
                   output_2_f(mtcars_grouped))
})

test_that("filter_if works", {
  filter_if_f <- . %>% filter_if(rlang::is_integerish, all_vars(. < 100))
  assign_perm_keys <- get_assign_permuted_keys(.f = filter_if_f)

  output_1_f <- . %>% filter_if_f() %>% assign_perm_keys()
  output_2_f <- . %>% assign_keys_mtcars() %>% filter_if_f()

  expect_identical(output_1_f(mtcars_df), output_2_f(mtcars_df))
  expect_identical(output_1_f(mtcars_tbl), output_2_f(mtcars_tbl))
  expect_identical(output_1_f(mtcars_grouped), output_2_f(mtcars_grouped))
  expect_identical(output_1_f(mtcars_rowwise), output_2_f(mtcars_rowwise))
})

test_that("filter_at works", {
  filter_at_f <- . %>% filter_at(vars("mpg", "hp"), all_vars(. > 15))
  assign_perm_keys <- get_assign_permuted_keys(.f = filter_at_f)

  output_1_f <- . %>% filter_at_f() %>% assign_perm_keys()
  output_2_f <- . %>% assign_keys_mtcars() %>% filter_at_f()

  expect_identical(output_1_f(mtcars_df), output_2_f(mtcars_df))
  expect_identical(output_1_f(mtcars_tbl), output_2_f(mtcars_tbl))
  expect_identical(output_1_f(mtcars_grouped), output_2_f(mtcars_grouped))
  expect_identical(output_1_f(mtcars_rowwise), output_2_f(mtcars_rowwise))
})


# slice -------------------------------------------------------------------
test_that("slice.keyed_d works", {
  slice_f <- . %>% slice(c(4, 5, 31, 3, 3, 14, 18, 30, 31, 44))
  assign_perm_keys <- get_assign_permuted_keys(.f = slice_f)

  output_1_f <- . %>% slice_f() %>% assign_perm_keys()
  output_2_f <- . %>% assign_keys_mtcars() %>% slice_f()

  expect_identical(output_1_f(mtcars_df), output_2_f(mtcars_df))
  expect_identical(output_1_f(mtcars_tbl), output_2_f(mtcars_tbl))
  # Slicing grouped_df is done differently
  expect_identical(output_1_f(mtcars_rowwise), output_2_f(mtcars_rowwise))

  # Checking mtcars_grouped
  assign_perm_keys_grouped <- get_assign_permuted_keys(
    .f = slice_f,
    tbl_groups = groups(mtcars_grouped)
  )

  output_1_f_grouped <- . %>% slice_f() %>% assign_perm_keys_grouped()

  expect_identical(output_1_f_grouped(mtcars_grouped),
                   output_2_f(mtcars_grouped))
})


# next_method_keys --------------------------------------------------------
test_that("next_method_keys ensures tibble in attr(.tbl, 'keys')", {
  input <- mtcars_df
  class(input) <- c("keyed_df", "data.frame")
  output <- next_method_keys(input, select, mpg) %>% attr("keys")
  output_ref <- tibble(logical(32))[-1]

  expect_identical(output, output_ref)
})


# next_method_keys_track --------------------------------------------------
test_that("next_method_keys_track ensures tibble in attr(.tbl, 'keys')", {
  input <- mtcars_df
  class(input) <- c("keyed_df", "data.frame")
  output_1 <- next_method_keys_track(input, arrange, mpg) %>% attr("keys")
  output_ref_1 <- tibble(logical(32))[-1]

  expect_identical(output_1, output_ref_1)

  output_2 <- next_method_keys_track(input, filter, cyl == 4) %>% attr("keys")
  output_ref_2 <- tibble(logical(11))[-1]

  expect_identical(output_2, output_ref_2)
})
