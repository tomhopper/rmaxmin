## Missing colMax, colMin, rowMax, rowMin

#' @title Form column maximums
#' @description returns the maximum value by column for numeric arrays.
#' @param x an array of two or more dimensions, containing numeric, complex, integer or logical values, or a numeric data frame.
#' @param na.rm logical. if \code{TRUE}, remove \code{NA}s, otherwise do not remove (and return errors or warnings)
#' @param dims integer.
#' @return a vector of maximum values from each column
#' @details
#'  This function is equivalent to the use of apply with FUN = max, similar to \code{colSums()} and related functions in \code{base}.
#' @seealso \link{colSums}, \link{max}, \link{min}, \link{apply}
colMax  <- function(x, na.rm = TRUE, dims = 1L) {
  if (is.data.frame(x))
    x <- as.matrix(x)
  dn <- dim(x)
  if (!is.array(x) || length(dn) < 2L)
    stop("'x' must be an array of at least two dimensions")
  if (dims < 1L || dims > length(dn) - 1L)
    stop("invalid 'dims'")
  id <- seq_len(dims)
  dn <- dn[id]
  if (is.complex(x))
    z <- apply(x, 2, function(x) unique(x[which(Re(x)^2 + Im(x)^2 == max(Re(x)^2 + Im(x)^2, na.rm = na.rm))]))
  else
    z <- apply(x, 2, function(x) max(x, na.rm = na.rm))

  if (length(dn) > 1L) {
    dim(z) <- dn
    dimnames(z) <- dimnames(x)[id]
  }
  else names(z) <- dimnames(x)[[1L]]
  return(z)
}

#' @title Form column minimums
#' @description returns the minimum value by column for numeric arrays.
#' @param x an array of two or more dimensions, containing numeric, complex, integer or logical values, or a numeric data frame.
#' @param na.rm logical. if \code{TRUE}, remove \code{NA}s, otherwise do not remove (and return errors or warnings)
#' @param dims integer.
#' @return a vector of minimum values from each column
#' @details
#'  This function is equivalent to the use of apply with FUN = min, similar to \code{colSums()} and related functions in \code{base}.
#' @seealso \link{colSums}, \link{max}, \link{min}, \link{apply}
colMin <- function(x, na.rm = TRUE, dims = 1L) {
  if (is.data.frame(x))
    x <- as.matrix(x)
  dn <- dim(x)
  if (!is.array(x) || length(dn) < 2L)
    stop("'x' must be an array of at least two dimensions")
  if (dims < 1L || dims > length(dn) - 1L)
    stop("invalid 'dims'")
  id <- seq_len(dims)
  dn <- dn[id]
  if (is.complex(x)) {   z <- apply(x, 2, function(x) unique(x[which(Re(x)^2 + Im(x)^2 == min(Re(x)^2 + Im(x)^2, na.rm = na.rm))]))}
  else  {   z <- apply(x, 2, function(x) min(x, na.rm = na.rm))}

  if (length(dn) > 1L) {
    dim(z) <- dn
    dimnames(z) <- dimnames(x)[id]
  } else names(z) <- dimnames(x)[[1L]]
  return(z)
}

#' @title Form row maximums
#' @description returns the maximum value by row for numeric arrays.
#' @param x an array of two or more dimensions, containing numeric, complex, integer or logical values, or a numeric data frame.
#' @param na.rm logical. if \code{TRUE}, remove \code{NA}s, otherwise do not remove (and return errors or warnings)
#' @param dims integer.
#' @return a vector of maximum values from each row
#' @details
#'  This function is equivalent to the use of apply with FUN = max, similar to \code{colSums()} and related functions in \code{base}.
#' @seealso \link{colSums}, \link{max}, \link{min}, \link{apply}
rowMax  <- function(x, na.rm = TRUE, dims = 1L) {
  if (is.data.frame(x))
    x <- as.matrix(x)
  dn <- dim(x)
  if (!is.array(x) || length(dn) < 2L)
    stop("'x' must be an array of at least two dimensions")
  if (dims < 1L || dims > length(dn) - 1L)
    stop("invalid 'dims'")
  id <- seq_len(dims)
  dn <- dn[id]
  if (is.complex(x))
    z <- apply(x, 1, function(x) unique(x[which(Re(x)^2 + Im(x)^2 == max(Re(x)^2 + Im(x)^2, na.rm = na.rm))]))
  else
    z <- apply(x, 1, function(x) max(x, na.rm = na.rm))

  if (length(dn) > 1L) {
    dim(z) <- dn
    dimnames(z) <- dimnames(x)[id]
  }
  else names(z) <- dimnames(x)[[1L]]
  return(z)
}

#' @title Form row minimums
#' @description returns the minimum value by row for numeric arrays.
#' @param x an array of two or more dimensions, containing numeric, complex, integer or logical values, or a numeric data frame.
#' @param na.rm logical. if \code{TRUE}, remove \code{NA}s, otherwise do not remove (and return errors or warnings)
#' @param dims integer.
#' @return a vector of minimum values from each row
#' @details
#'  This function is equivalent to the use of apply with FUN = min, similar to \code{colSums()} and related functions in \code{base}.
#' @seealso \link{colSums}, \link{max}, \link{min}, \link{apply}
rowMin  <- function(x, na.rm = TRUE, dims = 1L) {
  if (is.data.frame(x))
    x <- as.matrix(x)
  dn <- dim(x)
  if (!is.array(x) || length(dn) < 2L)
    stop("'x' must be an array of at least two dimensions")
  if (dims < 1L || dims > length(dn) - 1L)
    stop("invalid 'dims'")
  id <- seq_len(dims)
  dn <- dn[id]
  if (is.complex(x))
    z <- apply(x, 1, function(x) unique(x[which(Re(x)^2 + Im(x)^2 == min(Re(x)^2 + Im(x)^2, na.rm = na.rm))]))
  else
    z <- apply(x, 1, function(x) min(x, na.rm = na.rm))

  if (length(dn) > 1L) {
    dim(z) <- dn
    dimnames(z) <- dimnames(x)[id]
  }
  else names(z) <- dimnames(x)[[1L]]
  return(z)
}
