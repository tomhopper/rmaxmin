% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/max_min_vectors.R
\name{max_complex}
\alias{max_complex}
\title{Maxima of a vector}
\usage{
max_complex(x, na.rm = TRUE)
}
\arguments{
\item{x}{a numeric, complex or character vector}

\item{na.rm}{boolean. If \code{TRUE}, any \code{NA} values will be dropped before maxima are calculated. Otherwise, returns \code{NA} if any \code{NA} values are present.}
}
\description{
Returns the maxima of a vector, including vectors containing complex numbers. Essentially implements \code{max()} for complex numbers.
  Simply passes arguments through to \code{max()} if \code{x} is not of type \code{complex}.
}
\examples{
max_complex(runif(10))

  vi <- c(complex(r = 1, i = 0), complex(r = 1, i = 1), complex(r = 2, i = 1), complex(r = 2, i = 2))
  max_complex(vi)

  vi2 <- c(complex(r = 1, i = 0), complex(r = 1, i = 1), complex(r = 2, i = 1), complex(r = 2, i = 2), complex(r = 3, i = 2), complex(r = 2, i = 3))
  max_complex(vi2)
}
\seealso{
\link{max}, \link{complex}
}

