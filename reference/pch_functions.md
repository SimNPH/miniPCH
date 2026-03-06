# Survival Distributions with piece-wise Constant Hazards (function factories)

Densitiy, distribution function, quantiles, random numbers, hazard
function, cumulative hazard function and survival function of survival
distributions with piece-wise constant hazards (picewise exponential
distributions).

Those functions return functions of one parameter that can be evaluated
to give the density, distribution function, ... The parameters `t` and
`lambda` are checked only once and not at every function evaluation.

## Usage

``` r
dpch_fun(t, lambda)

ppch_fun(t, lambda)

qpch_fun(t, lambda)

rpch_fun(t, lambda, discrete = FALSE)

hpch_fun(t, lambda)

chpch_fun(t, lambda)

spch_fun(t, lambda)

pch_functions(t, lambda, discrete = FALSE)
```

## Arguments

- t:

  vector of left interval borders

- lambda:

  vector of hazards

- discrete:

  round survival times to whole numbers in RNG

## Value

`dpch_fun` gives the density.

`ppch_fun` gives the distribution function

`qpch_fun` gives the quantile function.

`rpch_fun` gives a function to sample from the given distribution.

`hpch_fun` gives the hazard function.

`chpch_fun` gives the cumulative hazard function.

`spch_fun` gives the survival function.

`pch_functions` gives an object of class "miniPCH"

## Functions

- `dpch_fun()`: density of survival distributions with piece-wise
  constant hazards

- `ppch_fun()`: distribution function of survival distributions with
  piece-wise constant hazards

- `qpch_fun()`: quantile function of survival distributions with
  piece-wise constant hazards

- `rpch_fun()`: RNG function of survival distributions with piece-wise
  constant hazards

- `hpch_fun()`: hazard function of survival distributions with
  piece-wise constant hazards

- `chpch_fun()`: cumulative hazard function of survival distributions
  with piece-wise constant hazards

- `spch_fun()`: survival function of survival distributions with
  piece-wise constant hazards

## See also

[pch](https://rdrr.io/r/graphics/points.html)

## Examples

``` r
pch_density <- dpch_fun(c(0, 3), c(2, 0.1))
pch_density(1:10)
#>  [1] 0.2706705665 0.0366312778 0.0002478752 0.0002242868 0.0002029431
#>  [6] 0.0001836305 0.0001661557 0.0001503439 0.0001360368 0.0001230912
pch_distr <- ppch_fun(c(0, 3), c(2, 0.1))
pch_distr(1:10)
#>  [1] 0.8646647 0.9816844 0.9975212 0.9977571 0.9979706 0.9981637 0.9983384
#>  [8] 0.9984966 0.9986396 0.9987691
pch_quant <- qpch_fun(c(0, 3), c(2, 0.1))
pch_quant(seq(0,1, by=0.1))
#>  [1] 0.00000000 0.05268026 0.11157178 0.17833747 0.25541281 0.34657359
#>  [7] 0.45814537 0.60198640 0.80471896 1.15129255        Inf
rpch_fun_cont  <- rpch_fun(c(0, 3), c(2, 0.1))
rpch_fun_discr <- rpch_fun(c(0, 3), c(2, 0.1), discrete=TRUE)
rpch_fun_cont(15)
#>  [1] 0.56997177 0.34542053 0.51316351 0.53982317 0.05047632 0.72536346
#>  [7] 0.73413154 2.33953283 1.76203692 0.24647874 0.30919286 0.18934471
#> [13] 0.09598956 0.37918808 0.34025075
rpch_fun_discr(15)
#>  [1] 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1
pch_haz <- hpch_fun(c(0, 3), c(2, 0.1))
pch_haz(1:10)
#>  [1] 2.0 2.0 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1
pch_cumhaz <- chpch_fun(c(0, 3), c(2, 0.1))
pch_cumhaz(1:10)
#>  [1] 2.0 4.0 6.0 6.1 6.2 6.3 6.4 6.5 6.6 6.7
pch_surv <- spch_fun(c(0, 3), c(2, 0.1))
pch_surv(1:10)
#>  [1] 0.135335283 0.018315639 0.002478752 0.002242868 0.002029431 0.001836305
#>  [7] 0.001661557 0.001503439 0.001360368 0.001230912
my_pch <- pch_functions(c(0, 3), c(2, 0.1))
my_pch$t
#> [1] 0 3
my_pch$r(15)
#>  [1] 0.29592970 0.23221852 0.01423110 0.31366773 0.24717389 0.01013463
#>  [7] 0.23658105 0.41039124 0.97274767 0.24291182 0.37530027 0.45894291
#> [13] 0.15148000 0.17128048 0.32703552
my_pch$ch(1:10)
#>  [1] 2.0 4.0 6.0 6.1 6.2 6.3 6.4 6.5 6.6 6.7
```
