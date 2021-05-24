#' The class compareIC
#' 
#' Output of the function \code{\link{compare}}. It contains
#' the class \code{data.frame}.
#' 
#' @slot dSE matrix of standard errors of differences in IC between pairs of models
#' @slot .Data contains the actual data frame with the results
#' 
#' @rdname compareIC-class
#' @name compareIC-class
#' @aliases compareIC
setClass( "compareIC" , slots=c( dSE="matrix" ) , contains="data.frame" )

#compare.show <- function( object ) {
#    r <- format_show( object@output , #digits=c('default__'=1,'weight'=2,'SE'=2,'dSE'=2) )
#    print( r )
#}
setMethod( "show" , "compareIC" , function(object) {
  r <- format_show( object , 
                    digits=c('default__'=1,'weight'=2,'SE'=2,'dSE'=2) )
  print( r )
} )
