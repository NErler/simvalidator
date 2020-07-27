
<!-- README.md is generated from README.Rmd. Please edit that file -->

# simvalidator

<!-- badges: start -->

<!-- badges: end -->

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
covar_def <- expression(data.frame(sex = rbinom(N, size = 1, prob = 0.5),
                                   age = rnorm(N, 63, 9))
)
```

Provide a list of parameters used in the covariate definition and the
function that creates the outcome variable:

``` r
data_params = list(global_seed = 2020,
                   formula = Surv(time, status) ~ age + sex,
                   betas = c('(Intercept)' = -50.8, age = 0.7, sex = 1.2),
                   phi = 4.8,
                   mean.Cens = 5,
                   N = 100)
```

This list has to include the `global_seed` and the `formula`. The global
seed is used to create the seeds for each dataset in the simulation.
`formula` is the model formula. All other arguments are specific to the
covariate definition and the data simulation function that is used.

The outcome for a single dataset is simulated using the function
`sim_data`:

``` r
data <- sim_data(covar_def, data_params, seed = 123, response_type = "survival")
```

The seed value will typically be a seed derived from the `global_seed`.

New functions to simulate different types of outcomes should be called
`sim_outcome_<response_type>`.

### Fit models

Specify a list of model functions to be used. This list has to include
the arguments `fun`, the function that runs the model, and `result`,
which is used to identify the function used to extract the desired
elements of the results from the model.

``` r
models <- list(list(fun = survival::coxph, args = NULL,
                    result = "default", res_args = list(type = "coxph")),
               list(fun = JointAI::coxph_imp, args = list(n.iter = 100),
                    result = "JointAI", res_args = NULL)
)
```

New result functions must be named `get_result_<type>` where the type is
the character string specified in the argument `result`.
`get_result_<type>` functions have to use the argument `fitted_model`
which takes the fitted model.

With this information the function `fit_models` is called:

``` r
fit_models(models, formula, data, seed)
```

This function fits each of the models and applies the corresponding
`get_result_<type>` to the fitted model.
