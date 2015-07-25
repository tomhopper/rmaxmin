## Missing colMax, colMin, rowMax, rowMin

#' @title Form column maxima
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
  dn <- dn[-id]
  z <- apply(x, 2, function(x) max_complex(x))

  if (length(dn) > 1L) {
    dim(z) <- dn
    dimnames(z) <- dimnames(x)[-id]
  }
  else names(z) <- dimnames(x)[[dims + 1L]]
  return(z)
}

#' @title Form column minima
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
  dn <- dn[-id]
  z <- apply(x, 2, function(x) min_complex(x))

  if (length(dn) > 1L) {
    dim(z) <- dn
    dimnames(z) <- dimnames(x)[-id]
  } else names(z) <- dimnames(x)[[dims + 1L]]
  return(z)
}

#' @title Form row maxima
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
  z <- apply(x, 1, function(x) max_complex(x))

  if (length(dn) > 1L) {
    dim(z) <- dn
    dimnames(z) <- dimnames(x)[id]
  }
  else names(z) <- dimnames(x)[[1L]]
  return(z)
}

#' @title Form row minima
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
  z <- apply(x, 1, function(x) min_complex(x))

  if (length(dn) > 1L) {
    dim(z) <- dn
    dimnames(z) <- dimnames(x)[id]
  }
  else names(z) <- dimnames(x)[[1L]]
  return(z)
}
