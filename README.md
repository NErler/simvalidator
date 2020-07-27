
<!-- README.md is generated from README.Rmd. Please edit that file -->

# simvalidator

<!-- badges: start -->

<!-- badges: end -->

<div style = "color:darkblue";><strong>Work in progress!!!</strong></div>

<br>

The goal of simvalidator is to perform simulation analysis to validate
model results, and to present these results in Rmarkdown documents.

## Installation

``` r
# install.packages("remotes")
remotes::install_github("NErler/simvalidator")
```

## Example Workflow

``` r
library(simvalidator)
```

### Simulating data

Specify an expression that generates the covariate part of the data,
e.g.:

``` r
covar_def <- expression(data.frame(sex = factor(rbinom(N, size = 1, prob = 0.5)),
                                   age = rnorm(N, 63, 9))
)
```

Provide a list of parameters used in the covariate definition and the
function that creates the outcome variable:

``` r
data_params = list(
  # standard arguments, always required:
  response_type = "lm",
  formula = resp ~ age + sex,
  # arguments for generating covariates (depend on the user-specified expression)
  N = 100,
  # arguments for creating the outcome (depend on the type of outcome)
  reg_coefs = c('(Intercept)' = -50.8, age = 0.7, sex1 = 1.2),
  resid_sd = 0.5)
```

This list has to include the model `formula`. All other arguments are
specific to the covariate definition and the data simulation function
that is used.

The outcome for a single dataset is simulated using the function
`sim_data`:

``` r
data <- sim_data(covar_def, data_params, seed = 123)
```

The seed value will typically be a seed derived from a `global_seed`
(specified in the `sim_params`). For a simulation study, `sim_data` is
usually called repeatedly internally to create a large number of
datasets.

New functions to simulate different types of outcomes should be called
`sim_outcome_<response_type>`.

### Fit models

Specify a list of model functions to be used. This list has to include
the arguments `fun`, the function that runs the model, and `result`,
which is used to identify the function used to extract the desired
elements of the results from the model.

``` r
models <- list(
  set_model(fun = lm, fun_args = NULL,
            result = "default", res_args = list(type = "lm")),
  set_model(fun = JointAI::lm_imp,
            fun_args = list(n.iter = 300),
            result = "JointAI", res_args = NULL)
)
```

New result functions must be named `get_result_<type>` where the type is
the character string specified in the argument `result`.
`get_result_<type>` functions have to use the argument `fitted_model`
which takes the fitted model.

With this information the function `fit_models` is called:

``` r
res <- fit_models(models, formula = data_params$formula, data, seed = 123)
```

This function fits each of the models and applies the corresponding
`get_result_<type>` to the fitted model.

### The simulation

``` r
sim_params <- list(
  global_seed = 2020,
  nr_sims = 100
)
```

``` r
library("doFuture")
registerDoFuture()

plan(list(multisession, sequential))
sim_out <- run_sim(sim_params, covar_def, data_params, response_type = "lm",
                   models)

sim_out
```
