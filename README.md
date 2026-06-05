
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nlfh

`nlfh` fits linear and nonlinear Bayesian Fay-Herriot models for small
area estimation with area-level direct estimates and known sampling
variances.

The main entry point is `fit_fh()`. Use `method = "linear"` for the
standard linear Fay-Herriot model, `method = "rnn"` for the
random-weight neural network extension described by Parker (2024), and
`method = "bart"` for the BART-FH model described by Parker and Eideh
(2026).

## Installation

You can install the development version of nlfh from GitHub with:

``` r
# install.packages("pak")
pak::pak("paparker/nlfh")
```

## Example

The included `acs_dat` data set contains 2021 direct estimates of median
income (`MedInc`) for the state of Missouri, its standard error
(`MedIncSE`), and area-level covariates. Pass the sampling variance
separately as `MedIncSE^2`.

``` r
library(nlfh)

data(acs_dat)
acs_small <- as.data.frame(acs_dat[1:500, ])
example_control <- list(n_iter = 500, burn_in = 250, progress = FALSE)

fit_linear <- fit_fh(
  MedInc ~ SNAPRate + PovRate + White + Black + Hispanic + Asian,
  sampling_variance = MedIncSE^2,
  data = acs_small,
  method = "linear",
  control = example_control
)

summary(fit_linear)
#> <summary.nlfh_fit>
#> Model type: linear Fay-Herriot
#> Formula: MedInc ~ SNAPRate + PovRate + White + Black + Hispanic + Asian
#> DIC: 14154
#> 
#> MCMC:
#>  n_iter burn_in posterior_draws areas
#>     500     250             250   500
#> 
#> Details:
#>  retained_draws burn_in_fraction progress
#>             250              0.5    FALSE
#> 
#> Variance parameters:
#>               parameter   mean     sd median   q2.5  q97.5
#>  random_effect_variance 0.3876 0.2155 0.3202 0.1183 0.9157
#> 
#> Coefficients:
#>    parameter    mean   sd  median    q2.5   q97.5
#>  (Intercept)  104700 4432  105000   95720  113500
#>     SNAPRate  -66560 3114  -66570  -72100  -59940
#>      PovRate -139800 2006 -139900 -143800 -135800
#>        White   -1505 4419   -1402   -9874    6982
#>        Black  -10740 4320  -10500  -19040   -2147
#>     Hispanic    4918 5566    5181   -5800   14970
#> ... 1 more rows
#> 
#> Area-level estimates theta_i:
#>  area  mean    sd median  q2.5 q97.5
#>     1 43720 280.6  43720 43190 44270
#>     2 30210 441.9  30210 29470 31020
#>     3 44310 635.2  44270 43110 45620
#>     4 61840 271.8  61820 61330 62360
#>     5 64310 278.1  64300 63790 64860
#>     6 40630 350.7  40640 39970 41280
#> ... 494 more rows
```

The formula interface specifies the available predictors. For nonlinear
methods, it does not impose an additive linear mean structure; the model
estimates an unknown function of the model matrix.

``` r
fit_rnn <- fit_fh(
  MedInc ~ .,
  sampling_variance = MedIncSE^2,
  data = acs_small,
  method = "rnn",
  control = example_control
)

fit_bart <- fit_fh(
  MedInc ~ SNAPRate + PovRate + White,
  sampling_variance = MedIncSE^2,
  data = acs_small,
  method = "bart",
  control = example_control
)

c(linear = fit_linear$dic, rnn = fit_rnn$dic, bart = fit_bart$dic)
#>   linear      rnn     bart 
#> 14154.20 11669.31 11696.77
```

``` r
fit_bart$variable_importance
#>  SNAPRate   PovRate     White 
#> 0.3054538 0.3853294 0.3092167
```

For BART fits, the first model-matrix column is treated as a
baseline/intercept column and is excluded from `variable_importance`.
With the formula interface this is usually the default `(Intercept)`
column; with the matrix interface, put the baseline or intercept column
first.

You can also use the matrix interface directly.

``` r
X <- model.matrix(
  MedInc ~ SNAPRate + PovRate + White + Black + Hispanic + Asian,
  data = acs_small
)

fit_matrix <- fit_fh(
  y = acs_small$MedInc,
  X = X,
  sampling_variance = acs_small$MedIncSE^2,
  method = "linear",
  control = example_control
)

dim(fit_matrix$predictions)
#> [1] 500 250
```

## References

Parker, P. A. (2024). Nonlinear Fay-Herriot Models for Small Area
Estimation Using Random Weight Neural Networks. *Journal of Official
Statistics*, 40(2), 317-332. <doi:10.1177/0282423X241244671>

Parker, P. A. and Eideh, A. (2026). BART-FH: Flexible Nonlinear Modeling
for Small Area Estimation. *Journal of Survey Statistics and
Methodology*, 00, 1-18. <doi:10.1093/jssam/smaf050>
