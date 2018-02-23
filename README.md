
ruler: Rule Your Data
=====================

[![Travis-CI Build Status](https://travis-ci.org/echasnovski/ruler.svg?branch=master)](https://travis-ci.org/echasnovski/ruler) [![Coverage Status](https://codecov.io/gh/echasnovski/ruler/graph/badge.svg)](https://codecov.io/github/echasnovski/ruler?branch=master)

`ruler` offers a set of tools for creating tidy data validation reports using [dplyr](http://dplyr.tidyverse.org) grammar of data manipulation. It is structured to be flexible and extendable in terms of creating rules and using their output.

To fully use this package a solid knowledge of `dplyr` is required. The key idea behind `ruler`'s design is to validate data by modifying regular `dplyr` code with as little overhead as possible.

Some functionality is powered by the [keyholder](https://echasnovski.github.io/keyholder) package. It is highly recommended to use its supported functions during rule construction. All one- and two-table `dplyr` verbs applied to local data frames are supported and considered the most appropriate way to create rules.

This README is structured as follows:

-   **Installation** shows ways to install package.
-   **Example** shows the basic usage of `ruler` for exploration of obeying user-defined rules and its automatic validation.
-   **Overview** explains basic data and function types with design behind them.
-   **Usage** describes `ruler`'s capabilities in more detail.
-   **Other packages for validation and assertions** lists alternatives for described tasks.

Installation
------------

You can install current stable version from CRAN with:

``` r
install.packages("ruler")
```

Also you can install development version from github with:

``` r
# install.packages("devtools")
devtools::install_github("echasnovski/ruler")
```

Example
-------

``` r
# Utilities functions
is_integerish <- function(x) {all(x == as.integer(x))}
z_score <- function(x) {abs(x - mean(x)) / sd(x)}

# Define rule packs
my_packs <- list(
  data_packs(
    dims = . %>% summarise(nrow_low = nrow(.) >= 10, nrow_high = nrow(.) <= 15,
                           ncol_low = ncol(.) >= 20, ncol_high = ncol(.) <= 30)
  ),
  group_packs(
    vs_am_num = . %>% group_by(vs, am) %>% summarise(vs_am_low = n() >= 7),
    .group_vars = c("vs", "am")
  ),
  col_packs(
    enough_col_sum = . %>%
      summarise_if(is_integerish, rules(is_enough = sum(.) >= 14))
  ),
  row_packs(
    enough_row_sum = . %>%
      filter(vs == 1) %>%
      transmute(is_enough = rowSums(.) >= 200)
  ),
  cell_packs(
    dbl_not_outlier = . %>%
      transmute_if(is.numeric, rules(is_not_out = z_score(.) < 1)) %>%
      slice(-(1:5))
  )
)

# Expose data to rules
mtcars_exposed <- mtcars %>% as_tibble() %>%
  expose(my_packs)

# View exposure
mtcars_exposed %>% get_exposure()
#>   Exposure
#> 
#> Packs info:
#> # A tibble: 5 x 4
#>   name            type       fun              remove_obeyers
#>   <chr>           <chr>      <list>           <lgl>         
#> 1 dims            data_pack  <S3: data_pack>  T             
#> 2 vs_am_num       group_pack <S3: group_pack> T             
#> 3 enough_col_sum  col_pack   <S3: col_pack>   T             
#> 4 enough_row_sum  row_pack   <S3: row_pack>   T             
#> 5 dbl_not_outlier cell_pack  <S3: cell_pack>  T             
#> 
#> Tidy data validation report:
#> # A tibble: 117 x 5
#>   pack            rule       var      id value
#>   <chr>           <chr>      <chr> <int> <lgl>
#> 1 dims            nrow_high  .all      0 F    
#> 2 dims            ncol_low   .all      0 F    
#> 3 vs_am_num       vs_am_low  0.1       0 F    
#> 4 enough_col_sum  is_enough  am        0 F    
#> 5 enough_row_sum  is_enough  .all     19 F    
#> 6 dbl_not_outlier is_not_out mpg      15 F    
#> # ... with 111 more rows

# Assert any breaker
invisible(mtcars_exposed %>% assert_any_breaker())
#>   Breakers report
#> # A tibble: 117 x 5
#>   pack            rule       var      id value
#>   <chr>           <chr>      <chr> <int> <lgl>
#> 1 dims            nrow_high  .all      0 F    
#> 2 dims            ncol_low   .all      0 F    
#> 3 vs_am_num       vs_am_low  0.1       0 F    
#> 4 enough_col_sum  is_enough  am        0 F    
#> 5 enough_row_sum  is_enough  .all     19 F    
#> 6 dbl_not_outlier is_not_out mpg      15 F    
#> # ... with 111 more rows
#> Error: assert_any_breaker: Some breakers found in exposure.
```

Overview
--------

**Rule** is a function which converts data unit of interest (data, group, column, row, cell) to logical value indicating whether this object satisfies certain condition.

**Rule pack** is a function which combines several rules into one functional block. The recommended way of creating rules is by creating packs right away with the use of `dplyr` and [magrittr](http://magrittr.tidyverse.org/)'s pipe operator.

**Exposing** data to rules means applying rules to data, collecting results in common format and attaching them to the data as an `exposure` attribute. In this way actual exposure can be done in multiple steps and also be a part of a general data preparation pipeline.

**Exposure** is a format designed to contain uniform information about validation of different data units. For reproducibilty it also saves information about applied packs. Basically exposure is a list with two elements:

1.  **Packs info**: a [tibble](http://tibble.tidyverse.org/) with the following structure:
    -   *name* &lt;chr&gt; : Name of the pack. If not set manually it will be imputed during exposure.
    -   *type* &lt;chr&gt; : Name of pack type. Indicates which data unit pack checks.
    -   *fun* &lt;list&gt; : List of rule pack functions.
    -   *remove\_obeyers* &lt;lgl&gt; : Whether rows about obeyers (data units that obey certain rule) were removed from report after applying pack.
2.  **Tidy data validation report**: a `tibble` with the following structure:
    -   *pack* &lt;chr&gt; : Name of rule pack from column 'name' in packs info.
    -   *rule* &lt;chr&gt; : Name of the rule defined in rule pack.
    -   *var* &lt;chr&gt; : Name of the variable which validation result is reported. Value '.all' is reserved and interpreted as 'all columns as a whole'. **Note** that *var* doesn't always represent the actual column in data frame: for group packs it represents the created group name.
    -   *id* &lt;int&gt; : Index of the row in tested data frame which validation result is reported. Value 0 is reserved and interpreted as 'all rows as a whole'.
    -   *value* &lt;lgl&gt; : Whether the described data unit obeys the rule.

There are four basic combinations of `var` and `id` values which define five basic data units:

-   `var == '.all'` and `id == 0`: Data as a whole.
-   `var != '.all'` and `id == 0`: Group (`var` shouldn't be an actual column name) or column (`var` should be an actual column name) as a whole.
-   `var == '.all'` and `id != 0`: Row as a whole.
-   `var != '.all'` and `id != 0`: Described cell.

With exposure attached to data one can perform different kinds of actions: exploration, assertion, imputation and so on.

Usage
-----

### Creating packs

#### Data packs

``` r
# List of two rule packs for checking data properties
my_data_packs <- data_packs(
  # data_dims is a pack name
  data_dims = . %>% summarise(
    # ncol and nrow are rule names
    ncol = ncol(.) == 12,
    nrow = nrow(.) == 32
  ),
  
  # Data after subsetting should have number of rows in between 10 and 30
  # Rules are applied separately
  vs_1 = . %>% filter(vs == 1) %>%
    summarise(
      nrow_low = nrow(.) > 10,
      nrow_high = nrow(.) < 30
    )
)
```

#### Group packs

``` r
# List of one nameless rule pack for checking group property
my_group_packs <- group_packs(
  # Name will be imputed during exposure
  . %>% group_by(vs, am) %>%
    summarise(any_cyl_6 = any(cyl == 6)),
  
  # One should supply grouping variables for correct interpretation of output
  .group_vars = c("vs", "am")
)
```

#### Column packs

``` r
# rules() is a dplyr::funs() with necessary name imputations
# In column packs it should always be used instead of dplyr::funs()

# List of two rule pack for checking certain columns' properties
my_col_packs <- col_packs(
  sum_bounds = . %>% summarise_at(
    # Check only columns with names starting with 'c'
    vars(starts_with("c")),
    rules(sum_low = sum(.) > 300, sum_high = sum(.) < 400)
  ),
  
  # In the edge case of checking one column with one rule there is a need
  # for forcing inclusion of names in the output of summarise_at().
  # This is done with naming argument in vars()
  vs_mean = . %>% summarise_at(vars(vs = vs), rules(mean(.) > 0.5))
)
```

#### Row packs

``` r
z_score <- function(x) {(x - mean(x)) / sd(x)}

# List of one rule pack checking certain rows' property
my_row_packs <- row_packs(
  row_mean = . %>% mutate(rowMean = rowMeans(.)) %>%
    transmute(is_common_row_mean = abs(z_score(rowMean)) < 1) %>%
    # Check only rows 10-15
    # Values in 'id' column of report will be based on input data (i.e. 10-15)
    # and not on output data (1-6)
    slice(10:15)
)
```

#### Cell packs

``` r
is_integerish <- function(x) {all(x == as.integer(x))}

# List of two cell pack checking certain cells' property
my_cell_packs <- cell_packs(
  my_cell_pack_1 = . %>% transmute_if(
    # Check only integer-like columns
    is_integerish,
    rules(is_common = abs(z_score(.)) < 1)
  ) %>%
    # Check only rows 20-30
    slice(20:30),
  
  # The same edge case as in column rule pack
  vs_side = . %>% transmute_at(vars(vs = "vs"), rules(. > mean(.)))
)
```

### Exposing

By default exposing removes obeyers.

``` r
mtcars %>%
  expose(my_data_packs, my_group_packs) %>%
  get_exposure()
#>   Exposure
#> 
#> Packs info:
#> # A tibble: 3 x 4
#>   name          type       fun              remove_obeyers
#>   <chr>         <chr>      <list>           <lgl>         
#> 1 data_dims     data_pack  <S3: data_pack>  T             
#> 2 vs_1          data_pack  <S3: data_pack>  T             
#> 3 group_pack..1 group_pack <S3: group_pack> T             
#> 
#> Tidy data validation report:
#> # A tibble: 3 x 5
#>   pack          rule      var      id value
#>   <chr>         <chr>     <chr> <int> <lgl>
#> 1 data_dims     ncol      .all      0 F    
#> 2 group_pack..1 any_cyl_6 0.0       0 F    
#> 3 group_pack..1 any_cyl_6 1.1       0 F
```

One can leave obeyers by setting `.remove_obeyers` to `FALSE`.

``` r
mtcars %>%
  expose(my_data_packs, my_group_packs, .remove_obeyers = FALSE) %>%
  get_exposure()
#>   Exposure
#> 
#> Packs info:
#> # A tibble: 3 x 4
#>   name          type       fun              remove_obeyers
#>   <chr>         <chr>      <list>           <lgl>         
#> 1 data_dims     data_pack  <S3: data_pack>  F             
#> 2 vs_1          data_pack  <S3: data_pack>  F             
#> 3 group_pack..1 group_pack <S3: group_pack> F             
#> 
#> Tidy data validation report:
#> # A tibble: 8 x 5
#>   pack          rule      var      id value
#>   <chr>         <chr>     <chr> <int> <lgl>
#> 1 data_dims     ncol      .all      0 F    
#> 2 data_dims     nrow      .all      0 T    
#> 3 vs_1          nrow_low  .all      0 T    
#> 4 vs_1          nrow_high .all      0 T    
#> 5 group_pack..1 any_cyl_6 0.0       0 F    
#> 6 group_pack..1 any_cyl_6 0.1       0 T    
#> # ... with 2 more rows
```

By default `expose()` guesses the pack type if 'not-pack' function is supplied. This behaviour has some edge cases but is useful for interactive use.

``` r
mtcars %>%
  expose(
    some_data_pack = . %>% summarise(nrow = nrow(.) == 10),
    some_col_pack = . %>% summarise_at(vars(vs = "vs"), rules(is.character(.)))
  ) %>%
  get_exposure()
#>   Exposure
#> 
#> Packs info:
#> # A tibble: 2 x 4
#>   name           type      fun             remove_obeyers
#>   <chr>          <chr>     <list>          <lgl>         
#> 1 some_data_pack data_pack <S3: data_pack> T             
#> 2 some_col_pack  col_pack  <S3: col_pack>  T             
#> 
#> Tidy data validation report:
#> # A tibble: 2 x 5
#>   pack           rule    var      id value
#>   <chr>          <chr>   <chr> <int> <lgl>
#> 1 some_data_pack nrow    .all      0 F    
#> 2 some_col_pack  rule..1 vs        0 F
```

To write strict and robust code one can set `.guess` to `FALSE`.

``` r
mtcars %>%
  expose(
    some_data_pack = . %>% summarise(nrow = nrow(.) == 10),
    some_col_pack = . %>% summarise_at(vars(vs = "vs"), rules(is.character(.))),
    .guess = FALSE
  ) %>%
  get_exposure()
#> Error in expose_single.default(X[[i]], ...): There is unsupported class of rule pack.
```

### Acting after exposure

General actions are recommended to be done with `act_after_exposure()`. It takes two arguments:

-   `.trigger` - a function which takes the data with attached exposure and returns `TRUE` if some action should be made.
-   `.actor` - a function which takes the same argument as `.trigger` and performs some action.

If trigger didn't notify then the input data is returned untouched. Otherwise the output of `.actor()` is returned. **Note** that `act_after_exposure()` is often used for creating side effects (printing, throwing error etc.) and in that case should invisibly return its input (to be able to use it with pipe).

``` r
trigger_one_pack <- function(.tbl) {
  packs_number <- .tbl %>%
    get_packs_info() %>%
    nrow()
  
  packs_number > 1
}

actor_one_pack <- function(.tbl) {
  cat("More than one pack was applied.\n")
  
  invisible(.tbl)
}

mtcars %>%
  expose(my_col_packs, my_row_packs) %>%
  act_after_exposure(
    .trigger = trigger_one_pack,
    .actor = actor_one_pack
  ) %>%
  invisible()
#> More than one pack was applied.
```

`ruler` has function `assert_any_breaker()` which can notify about presence of any breaker in exposure.

``` r
mtcars %>%
  expose(my_col_packs, my_row_packs) %>%
  assert_any_breaker()
#>   Breakers report
#> # A tibble: 4 x 5
#>   pack       rule               var      id value
#>   <chr>      <chr>              <chr> <int> <lgl>
#> 1 sum_bounds sum_low            cyl       0 F    
#> 2 sum_bounds sum_low            carb      0 F    
#> 3 vs_mean    rule..1            vs        0 F    
#> 4 row_mean   is_common_row_mean .all     15 F
#> Error: assert_any_breaker: Some breakers found in exposure.
```

Other packages for validation and assertions
--------------------------------------------

More leaned towards assertions:

-   [assertr](https://github.com/ropensci/assertr)
-   [assertthat](https://github.com/hadley/assertthat)
-   [checkmate](https://github.com/mllg/checkmate)
-   [ensurer](https://github.com/smbache/ensurer)
-   [tester](https://github.com/gastonstat/tester)

More leaned towards validation:

-   [naniar](https://github.com/njtierney/naniar)
-   [skimr](https://github.com/ropenscilabs/skimr)
-   [validate](https://github.com/data-cleaning/validate)
