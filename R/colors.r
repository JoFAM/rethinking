#' Color utility functions
#' 
#' Functions for calculating transparent and desaturated colors.
#' 
#'   These functions allow for calculating transparency and desaturation for colors. \code{col.alpha} makes a base color transparent, while \code{col.desat} makes a color have less saturation.
#'
#' \code{col.dist} is used to make a list of transparent colors of differing alpha level. The levels are chosen based upon Gaussian distance from a chosen value, \code{mu}, with a chosen width of the function that determines how quickly colors become fully transparent, \code{sd}. For example, if \code{x} contains a column of data, then \code{col.dist} will return a vector of same length with transparency increasing as each value in \code{x} is distant from \code{mu}. This is useful for plotting data that emphasize points near some value or values.
#'
#' \code{grau} simply returns a transparent version of black, producing effective gray values.
#' 
#' @param acol A color name or RGB color.
#' @param alpha alpha transparency, where 1 means fully opaque and 0 fully transparent.
#' @param amt amount of desaturation of color to apply, hwere
#' 1 means totally desaturated (grayscale)
#' @param x a vector of values to use for calculating distances.
#' See details below.
#' @param mu Value (or vector of values) to use for calculating distances.
#' @param sd Standard deviation of distance function used
#' to calculate transparency
#' @param col A color to apply transparency to, based on distance.
#' @param alpha Transarency of black (used by \code{grau})
#' 
#' @return a vector with colors
#' @author Richard McElreath
#' @seealso \code{\link{col2rgb}}, \code{\link{rgb2hsv}},
#'  \code{\link{rgb}}
#' 
#' @examples 
#' col.desat("red")
#' 
#' @rdname col.alpha
#' @name col.alpha
#' @aliases col.desat col.dist grau
#' @export
col.desat <- function( acol , amt=0.5 ) {
    acol <- col2rgb(acol)
    ahsv <- rgb2hsv(acol)
    ahsv[2] <- ahsv[2] * amt
    hsv( ahsv[1] , ahsv[2] , ahsv[3] )
}

#' @rdname col.alpha
#' @export
col.alpha <- function( acol , alpha=0.2 ) {
    acol <- col2rgb(acol)
    acol <- rgb(acol[1]/255,acol[2]/255,acol[3]/255,alpha)
    acol
}

#' @rdname col.alpha
#' @export
col.dist <- function( x , mu=0 , sd=1 , col="slateblue" ) {
    cols <- sapply( x , function(z) exp(-(z-mu)^2/sd) )
    cols <- sapply( cols , function(z) col.alpha(col,z) )
    cols
}

#' @rdname col.alpha
#' @export
grau <- function( alpha=0.5 ) col.alpha( "black" , alpha )

# objects created are moved to zz_objects.R
