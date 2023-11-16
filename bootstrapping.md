cross_validation
================
2023-11-14

## Generate a relevant example

``` r
n_samp = 250

# dataset with constant variance
sim_df_const = 
  tibble(
    x = rnorm(n_samp, 1,1),
    error = rnorm(n_samp, 0,1),
    y = 2+3 * x + error
  )

# dataset with nonconstant variance
sim_df_nonconst = 
  sim_df_const|>
  mutate(
      error = error *.75 * x,
      y = 2+3 * x + error
  )

sim_df_const |>
  ggplot(aes(x =x, y = y )) +geom_point()
```

<img src="bootstrapping_files/figure-gfm/unnamed-chunk-2-1.png" width="90%" />

``` r
sim_df_nonconst |>
  ggplot(aes(x =x, y = y )) +geom_point()
```

<img src="bootstrapping_files/figure-gfm/unnamed-chunk-2-2.png" width="90%" />

``` r
# variance at intercept (x=0) is low for nonconst, slop of the line should be relatively large becuase the tail is loose
```

fit some linear models

``` r
sim_df_const |>
  lm(y ~ x, data = _) |>
  broom::tidy()
```

    ## # A tibble: 2 × 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)     1.98    0.0981      20.2 3.65e- 54
    ## 2 x               3.04    0.0699      43.5 3.84e-118

``` r
sim_df_nonconst |>
  lm(y ~ x, data = _) |>
  broom::tidy()
```

    ## # A tibble: 2 × 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)     1.93    0.105       18.5 1.88e- 48
    ## 2 x               3.11    0.0747      41.7 5.76e-114
