
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nlfh

`nlfh` fits linear and nonlinear Bayesian Fay-Herriot models for small
area estimation with area-level direct estimates and known sampling
variances.

The main entry point is `fit_fh()`. Use `method = "linear"` for the
standard linear Fay-Herriot model, `method = "rnn"` for the
random-weight neural network extension, and `method = "bart"` for the
BART-FH model.

## Installation

You can install the development version of nlfh from GitHub with:

``` r
# install.packages("pak")
pak::pak("paparker/nlfh")
```

## Example

The included `acs_dat` data set contains a direct estimate of median
income (`MedInc`), its standard error (`MedIncSE`), and area-level
covariates. Pass the sampling variance separately as `MedIncSE^2`.

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
#> Method: linear
#> Iterations: 500
#> Burn-in: 250
#> DIC: 85203
#> 
#> Random-effect variance:
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>  0.4084  1.3760  2.3990  2.6473  3.7401  7.8430
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
#> 85202.62 22266.22 11761.98
```

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
