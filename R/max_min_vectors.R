#' Maxima of a vector
#' @description Returns the maxima of a vector, including vectors containing complex numbers. Essentially implements \code{max()} for complex numbers.
#'   Simply passes arguments through to \code{max()} if \code{x} is not of type \code{complex}.
#' @param x a numeric, complex or character vector
#' @param na.rm boolean. If \code{TRUE}, any \code{NA} values will be dropped before maxima are calculated. Otherwise, returns \code{NA} if any \code{NA} values are present.
#' @examples
#'   max_complex(runif(10))
#'
#'   vi <- c(complex(r = 1, i = 0), complex(r = 1, i = 1), complex(r = 2, i = 1), complex(r = 2, i = 2))
#'   max_complex(vi)
#'
#'   vi2 <- c(complex(r = 1, i = 0), complex(r = 1, i = 1), complex(r = 2, i = 1), complex(r = 2, i = 2), complex(r = 3, i = 2), complex(r = 2, i = 3))
#'   max_complex(vi2)
#' @seealso \link{max}, \link{complex}
max_complex <- function(x, na.rm = TRUE) {
  if(na.rm) x <- na.omit(x)
  else if(any(is.na(x))) return(NA)
  if(is.complex(x)) {
    ind <- which(Mod(x) == max(Mod(x), na.rm = na.rm))
    if(length(ind) == 1) max_c <- x[ind]
    else {
      max_ind <- 1
      for(i in 2:length(ind)) {
        if(Arg(x[ind[i]]) > Arg(x[ind[i-1]])) max_ind <- i
      }
      max_c <- x[ind[max_ind]]
    }
  } else max_c <- max(x, na.rm = na.rm)
  return(max_c)
}

#' Minima of a vector
#' @description Returns the minima of a vector, including vectors containing complex numbers. Essentially implements \code{min()} for complex numbers.
#'   Simply passes arguments through to \code{min()} if \code{x} is not of type \code{complex}.
#' @param x a numeric, complex or character vector
#' @param na.rm boolean. If \code{TRUE}, any \code{NA} values will be dropped before maxima are calculated. Otherwise, returns \code{NA} if any \code{NA} values are present.
#' @return a numeric, complex, or character vector of length == 1
#' @examples
#' min_complex(runif(10))
#'
#' vi <- c(complex(r = 1, i = 0), complex(r = 1, i = 1), complex(r = 2, i = 1), complex(r = 2, i = 2))
#' min_complex(vi)
#'
#' vi2 <- c(complex(r = 1, i = 0), complex(r = 1, i = 1), complex(r = 2, i = 1), complex(r = 2, i = 2), complex(r = 3, i = 2), complex(r = 0, i = 1))
#' min_complex(vi2)
#' @seealso \link{min}, \link{complex}
min_complex <- function(x, na.rm = TRUE) {
  if(na.rm) x <- na.omit(x)
  else if(any(is.na(x))) return(NA)
  if(is.complex(x)) {
    ind <- which(Mod(x) == min(Mod(x), na.rm = na.rm))
    if(length(ind) == 1) min_c <- x[ind]
    else {
      min_ind <- 1
      for(i in 2:length(ind)) {
        if(Arg(x[ind[i]]) < Arg(x[ind[i-1]])) min_ind <- i
      }
      min_c <- x[ind[min_ind]]
    }
  } else min_c <- min(x, na.rm = na.rm)
  return(min_c)
}
