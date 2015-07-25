# rmaxmin

Provides the "missing" functions `colMax()`, `colMin()`, `rowMax()` and `rowMin()`, calculating the maximum or minimum by row or column for a numeric array, matrix or data frame.

Additionally, provides functions `max_complex()` and `min_complex()` to calculate maxima and minima for vectors. These default to the base package's `max()` and `min()`, and so could be used as full replacements for the standard functions.

The return type and class depend on the data supplied, but should always be a vector.

### Example

``` {r}
> ma <- matrix(runif(3*10), ncol = 3)
> colMax(ma)
[1] 0.8682987 0.8947111 0.9869130
> rowMax(ma)
 [1] 0.8918043 0.8111271 0.8438504 0.7434568 0.7311755 0.9869130 0.8947111 0.8720235 0.4291066 0.6898220
```

## Installation

1. Install the release version of devtools from CRAN with install.packages("devtools").

2. `devtools::install_github("tomhopper/rmaxmin")`

## Non-numeric data

For data frames that include character or factor vectors, returns a character vector. For all-numeric rows or columns, the numeric max or min is calculated and returned as its character representation. For rows or columns containing characters or factors, the max and min are the ASCII value, such that elements in the row or column that start with characters are always greater than elements starting with numbers.

### Example

```{r}
> df <- data.frame(x = runif(10), y = runif.digits(10, 2), a = letters[1:10])
> colMax(df)
[1] "0.95239866" "98"         "j"         
> colMin(df)
[1] "0.05719256" "20"         "a"         
> rowMax(df)
 [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j"
> rowMin(df)
 [1] "0.83560133" "0.54246203" "0.05719256" "0.95239866" "0.21359881" "0.53014575" "0.47729229" "0.62854356"
 [9] "0.07494556" "0.67397014"
```

## Complex numbers

Max and min of complex numbers are determined using `Re(x)^2 + Im(x)^2`, which can result in ties that are returned as lists.

### Example

``` {r}
> dfi <- data.frame(x = c(2+1i, 1+2i, 1+0i, 0+1i, 2+1i), y = c(0, 1, 2, 3, 4))
> colMax(dfi)
[[1]]
[1] 2+1i 1+2i

[[2]]
[1] 4+0i

```

## Changes

### 0.2
#### New features
* Functions `max_complex()` and `min_complex()` 

#### Fixes
* Fixed problem where ties would return a list showing ties, rather than just a vector.
* Improved calculation of max and min. Now uses `Mod()`. Ties are resolved with `Arg()`.
* Fixed error that triggered when row names were defined.

#### Miscellaneous
* Rewrite made code more maintainable.

###0.1
First release of functions previously used internally via `source()`

## To Do

* Check edge cases and bug fixes.
* Resolve handling of complex numbers to return vectors rather than lists in the case of ties with different real and imaginary parts.
