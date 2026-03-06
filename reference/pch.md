# Survival Distributions with piece-wise Constant Hazards

Densitiy, distribution function, quantiles, random numbers, hazard
function, cumulative hazard function and survival function of survival
distributions with piece-wise constant hazards (picewise exponential
distributions).

## Usage

``` r
dpch(x, t, lambda)

ppch(q, t, lambda)

qpch(p, t, lambda)

rpch(n, t, lambda, discrete = FALSE)

hpch(x, t, lambda)

chpch(x, t, lambda)

spch(q, t, lambda)
```

## Arguments

- x:

  vector of quantiles

- t:

  vector of left interval borders

- lambda:

  vector of hazards

- q:

  vector of quantiles

- p:

  vector of probabilities

- n:

  number of random numbers

- discrete:

  round survival times to whole numbers

## Value

`dpch` gives the density evaluated at `x`.

`ppch` gives the distribution function evaluated at `q`.

`qpch` gives the `p`-quantiles.

`rpch` gives `n` random numbers.

`hpch` gives the hazard function evaluated at `x`.

`chpch` gives the cumulative hazard function evaluated at `x`.

`spch` gives the survival function evaluated at `q`.

## Functions

- `dpch()`: density of survival distributions with piece-wise constant
  hazards

- `ppch()`: distribution function of survival distributions with
  piece-wise constant hazards

- `qpch()`: quantiles of survival distributions with piece-wise constant
  hazards

- `rpch()`: random samples of survival distributions with piece-wise
  constant hazards

- `hpch()`: hazard of survival distributions with piece-wise constant
  hazards

- `chpch()`: cumulative hazard of survival distributions with piece-wise
  constant hazards

- `spch()`: survival function of survival distributions with piece-wise
  constant hazards

## Examples

``` r
dpch(1:10, c(0, 3), c(2, 0.1))
#>  [1] 0.2706705665 0.0366312778 0.0002478752 0.0002242868 0.0002029431
#>  [6] 0.0001836305 0.0001661557 0.0001503439 0.0001360368 0.0001230912
ppch(1:10, c(0, 3), c(2, 0.1))
#>  [1] 0.8646647 0.9816844 0.9975212 0.9977571 0.9979706 0.9981637 0.9983384
#>  [8] 0.9984966 0.9986396 0.9987691
qpch(seq(0,1, by=0.1), c(0, 3), c(2, 0.1))
#>  [1] 0.00000000 0.05268026 0.11157178 0.17833747 0.25541281 0.34657359
#>  [7] 0.45814537 0.60198640 0.80471896 1.15129255        Inf
rpch(15, c(0, 3), c(2, 0.1))
#>  [1] 0.66003236 0.74034980 1.03812596 0.09614996 0.01742065 0.19311495
#>  [7] 0.25735678 0.10887272 0.25836997 0.03288909 0.24608480 1.85551826
#> [13] 0.17116931 0.56719294 0.66461611
rpch(15, c(0, 3), c(2, 0.1), discrete=TRUE)
#>  [1] 1 2 1 1 1 1 1 1 1 1 1 1 1 1 2
hpch(1:10, c(0, 3), c(2, 0.1))
#>  [1] 2.0 2.0 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1
chpch(1:10, c(0, 3), c(2, 0.1))
#>  [1] 2.0 4.0 6.0 6.1 6.2 6.3 6.4 6.5 6.6 6.7
ppch(1:10, c(0, 3), c(2, 0.1))
#>  [1] 0.8646647 0.9816844 0.9975212 0.9977571 0.9979706 0.9981637 0.9983384
#>  [8] 0.9984966 0.9986396 0.9987691
```
