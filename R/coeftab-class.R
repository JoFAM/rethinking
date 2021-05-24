#' Class coeftab
#' 
#' The class represents a table with coefficients.
#' 
#' @slot coefs a matrix with the coefficients
#' @slot se a matrix with the standard errors
#' @slot nobs a numeric vector with the number of observations
#' @slot AIC the AIC.
#' @slot digits the number of digits used to round numbers
#' @slot width the minimum field width, see \code{\link{format}}
#' 
#' @include aa_generics.r
#' 
#' @rdname coeftab-class
#' @name coeftab-class
#' @exportClass coeftab
setClass( "coeftab" , 
          slots=c( coefs="matrix" , 
                   se="matrix" , 
                   nobs="numeric" , 
                   AIC="numeric" , 
                   digits="numeric" , 
                   width="numeric" ) )

# show function
coeftab_show <- function( object ) {
  result <- object@coefs
  if ( !is.null(object@nobs) ) {
    result <- rbind( result , object@nobs )
    rownames(result)[ nrow(result) ] <- "nobs"
  }
  coefs <- rrformat( result , digits=object@digits , width=object@width )
  print( coefs , quote=FALSE , justify="right" )
}

setMethod( "show" , "coeftab" , function(object) coeftab_show(object) )
