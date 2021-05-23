#' Standardize/normalize vectors
#' 
#' Standardization is done by substracting the mean and dividing by the standard deviation. It does the same as \code{\link{scale}}, but works on vectors instead of matrices. Normalization is done by substracting the minimum and dividing by the maximum of the resulting vector. This bounds the vector between 0 and 1.
#' 
#' @param x a numeric vector. 
#' @param center the center of the original distribution. If not provided, it is read from the attribute \code{scaled:center}.
#' @param scale the scale of the original distribution. If not provided, it is read from the attribute \code{scaled:scale}
#' 
#' @return for \code{normalize}: the normalized vector
#' 
#' @name normalize
#' @rdname normalize
#' 
#' @examples 
#' 
#' x <- rnorm(10, 5, 2)
#' # Normalization
#' xn <- normalize(x)
#' range(xn)
#' # Standardization
#' xs <- standardize(x)
#' xorig <- unstandardize(xs)
#' 
#' mean(xs)
#' sd(xs)
#' identical(x, xorig)
#' 
#' @export
normalize <- function(x) { x <- x - min(x); x/max(x); }

#' @rdname normalize
#' @return for \code{standardize} a standardized vector with attributes \code{scaled:center} and \code{scaled:scale}, containing the center and scale used for standardization.
#' @aliases standardize 
#' @export
standardize <- function(x) {
  # No need to call scale(), too much overhead.
  center <- mean(x, na.rm = TRUE)
  scale <- sd(x, na.rm = TRUE)
  z <- (x - center) / scale
  attr(z,"scaled:center") <- center
  attr(z,"scaled:scale") <- scale
  return(z)
}

#' @rdname normalize
#' @return for \code{unstandardize}: a vector with the original values. 
#' @aliases unstandardize
#' @export
unstandardize <- function(x, 
                          center = attr(x,"scaled:center"),
                          scale = attr(x, "scaled:scale")) {
  # keep things sane.
  if(is.null(scale) || is.null(center)){
    stop("Not enough information about center or scale to unstandardize.")
  }
  z <- x*scale + center
  return( as.numeric(z) )
}
