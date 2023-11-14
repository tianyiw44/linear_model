cross_validation
================
2023-11-14

## Nonlinear data and CV

``` r
nonlin_df = 
  tibble(
    id = 1:100,
    x = runif(100, 0, 1),
    y = 1 - 10 * (x - .3) ^ 2 + rnorm(100, 0, .3)
  )

nonlin_df |> 
  ggplot(aes(x = x, y = y)) + 
  geom_point()
```

<img src="cross_validation_files/figure-gfm/unnamed-chunk-2-1.png" width="90%" />

Do the Train / Test split

``` r
train_df = sample_n(nonlin_df, 80)
test_df = anti_join(nonlin_df, train_df, by = "id")
```

``` r
train_df |>
  ggplot(aes(x = x, y=y)) +
  geom_point() +
  geom_point(data = test_df, color = "red")
```

<img src="cross_validation_files/figure-gfm/unnamed-chunk-4-1.png" width="90%" />

``` r
linear_mod = lm(y ~ x, data = train_df)
smooth_mod = mgcv::gam(y ~ s(x), data = train_df)
wiggly_mod = mgcv::gam(y ~ s(x, k = 30), sp = 10e-6, data = train_df)
```

quick visualization of the linear model

``` r
train_df |>
  modelr :: add_predictions(linear_mod) |>
  ggplot(aes(x=x, y=y)) +
  geom_point() +
  geom_path (aes(y = pred))
```

<img src="cross_validation_files/figure-gfm/unnamed-chunk-6-1.png" width="90%" />

``` r
train_df |>
  modelr :: add_predictions(smooth_mod) |>
  ggplot(aes(x=x, y=y)) +
  geom_point() +
  geom_line (aes(y = pred))
```

<img src="cross_validation_files/figure-gfm/unnamed-chunk-7-1.png" width="90%" />

``` r
train_df |>
  modelr :: add_predictions(wiggly_mod) |>
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_line (aes(y = pred))
```

<img src="cross_validation_files/figure-gfm/unnamed-chunk-8-1.png" width="90%" />

load key packages.

``` r
library(tidyverse)
library(modelr)

set.seed(1)
```

RMSEs on training data can be misleading

``` r
rmse(linear_mod, train_df)
```

    ## [1] 0.7178747

``` r
rmse(smooth_mod, train_df)
```

    ## [1] 0.2874834

``` r
rmse(wiggly_mod, train_df)
```

    ## [1] 0.2498309

RMSEs on testing data gives a sense of out-of-sample prediction
accuracy!

``` r
rmse(linear_mod, test_df)
```

    ## [1] 0.7052956

``` r
rmse(smooth_mod, test_df)
```

    ## [1] 0.2221774

``` r
rmse(wiggly_mod, test_df)
```

    ## [1] 0.289051

### use modelr for Cross Validation

``` r
cv_df = 
  nonlin_df |>
  crossv_mc(n = 100)
```

``` r
cv_df |> pull (train) |> nth(1) |> as.tibble()
```

    ## # A tibble: 79 × 3
    ##       id      x       y
    ##    <int>  <dbl>   <dbl>
    ##  1     1 0.266   1.11  
    ##  2     2 0.372   0.764 
    ##  3     3 0.573   0.358 
    ##  4     4 0.908  -3.04  
    ##  5     6 0.898  -1.99  
    ##  6     7 0.945  -3.27  
    ##  7     9 0.629   0.0878
    ##  8    10 0.0618  0.392 
    ##  9    12 0.177   0.836 
    ## 10    13 0.687  -0.291 
    ## # ℹ 69 more rows

``` r
cv_df = 
  nonlin_df |>
  crossv_mc(n = 100) |>
  mutate(
    train = map(train, as_tibble),
    test = map(test, as_tibble)
  )
```

Apply each model to all training datasets, and evaluate on all testing
datasets.

``` r
cv_results = 
  cv_df|>
  #fit linear model to all training dataset 
  #`\(df)` is setting a function. The function is the linear model after the `\(df)`. 
  mutate(
    linear_fit = map(train, \(df) lm (y ~ x, data =df ))
  )|>
  # conduct rmse for the linear model, using the lm in the linear_fit to get the RMSE of the test dataset, mapping two columns instead of one 
  # use map2_dbl instead of map will turn the list in to the number 
  mutate(
    rmse_linear = map2_dbl(linear_fit, test, \(mod, df) rmse(mod, df))
  )
```

``` r
cv_results = 
  cv_df|>
  #fit linear model to all training dataset 
  #`\(df)` is setting a function. The function is the linear model after the `\(df)`. 
  mutate(
    linear_fit = map(train, \(df) lm (y ~ x, data =df )),
    smooth_fit = map(train, \(df) mgcv::gam(y~s(x), data = df)),
    wiggly_fit = map(train, \(df) mgcv::gam(y ~ s(x, k = 30), sp = 10e-6, data = df))
  )|>
  # conduct rmse for the linear model, using the lm in the linear_fit to get the RMSE of the test dataset, mapping two columns instead of one 
  # use map2_dbl instead of map will turn the list in to the number 
  mutate(
    rmse_linear = map2_dbl(linear_fit, test, \(mod, df) rmse(mod, df)),
    rmse_smooth = map2_dbl(smooth_fit, test, \(mod, df) rmse(mod, df)),
    rmse_wiggly = map2_dbl(wiggly_fit, test, \(mod, df) rmse(mod, df))
  )
```

``` r
cv_results |>
  select(starts_with("rmse")) |> 
  pivot_longer(
    everything(),
    names_to = "model_type", 
    values_to = "rmse",
    names_prefix = "rmse_") |> 
  mutate(model = fct_inorder(model_type)) |> 
  ggplot(aes(x = model_type, y = rmse)) + geom_violin()
```

<img src="cross_validation_files/figure-gfm/unnamed-chunk-17-1.png" width="90%" />
