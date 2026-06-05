
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
#> DIC: 14156
#> 
#> MCMC:
#>  n_iter burn_in posterior_draws areas
#>     500     250             250   500
#> 
#> Details:
#>  retained_draws burn_in_fraction progress scale
#>             250              0.5    FALSE FALSE
#> 
#> Variance parameters:
#>               parameter mean     sd median q2.5 q97.5
#>  random_effect_variance 1.97 0.3603  1.911 1.35 2.731
#> 
#> Coefficients:
#>    parameter    mean   sd  median    q2.5   q97.5
#>  (Intercept)  104700 4459  104600   96920  113600
#>     SNAPRate  -66270 3246  -66140  -72070  -59540
#>      PovRate -139700 2156 -139700 -143400 -135000
#>        White   -1647 4544   -1523  -10740    6418
#>        Black  -10860 4496  -10800  -20080   -2823
#>     Hispanic    5126 5913    4918   -6132   16730
#> ... 1 more rows
#> 
#> Area-level estimates theta_i:
#>  area  mean    sd median  q2.5 q97.5
#>     1 43730 274.0  43730 43170 44310
#>     2 30240 442.7  30270 29370 31070
#>     3 44360 642.6  44360 43110 45660
#>     4 61810 266.7  61810 61260 62340
#>     5 64280 272.1  64290 63670 64780
#>     6 40650 359.5  40640 39940 41340
#> ... 494 more rows
```

The formula interface specifies the available predictors. For nonlinear
methods, it does not impose an additive linear mean structure; the model
estimates an unknown function of the model matrix. By default, the RNN
model centers and scales non-intercept covariates before fitting; use
`control = list(scale = FALSE)` to disable this covariate preprocessing.
The RNN response and sampling variances are standardized internally and
returned on the original response scale.

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
#> 14155.66 10535.90 11782.53
```

``` r
fit_bart$variable_importance
#>  SNAPRate   PovRate     White 
#> 0.3155422 0.3835334 0.3009243
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
