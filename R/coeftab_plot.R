#' Plot method for \code{coeftab} objects
#' 
#' Plots coefficient tables produced by \code{\link{coeftab}},
#' clustered either by models or by parameter names.
#' 
#' This function plots the tabular output of \code{\link{coeftab}}, 
#' using a \code{\link{dotchart}}. By default, estimates are grouped
#' by parameter, with a row for each model. Model's without a 
#' parameter still appear as a row, but with no estimate. 
#' By setting \code{by.model=TRUE}, the dotchart will instead 
#' be grouped by model, with each row being a parameter. 
#' 
#' MAP estimates are displayed with percentile confidence 
#' (credible) intervals. Default is 95\% intervals. 
#' Use \code{prob} to change the interval mass.
#' 
#' @param x Object produced by \code{coeftab}.
#' @param y \code{NULL} and unused. Required for compatibility with base \code{plot}.
#' @param pars Optional vector of parameter names or indexes to display. If missing, all parameters are shown.
#' @param col.ci Color to draw confidence intervals.
#' @param by.model Cluster estimates by model instead of by parameter (default).
#' @param prob Probability mass for confidence intervals. Default is 0.95
#' 
#' @return \code{NULL} invisibly. This function is called for its side effect of creating a plot.
#' 
#' @examples 
#' 
#' data(WaffleDivorce)
#' d <- WaffleDivorce
#'
#' # standardize variables
#' d$D <- standardize( d$Divorce )
#' d$M <- standardize( d$Marriage )
#' d$A <- standardize( d$MedianAgeMarriage )
#' 
#' m5.1 <- quap(
#'     alist(
#'         D ~ dnorm( mu , sigma ) ,
#'         mu <- a + bA * A ,
#'         a ~ dnorm( 0 , 0.2 ) ,
#'         bA ~ dnorm( 0 , 0.5 ) ,
#'         sigma ~ dexp( 1 )
#'     ) , data = d )
#' 
#' m5.2 <- quap(
#' alist(
#'     D ~ dnorm( mu , sigma ) ,
#'     mu <- a + bM * M ,
#'     a ~ dnorm( 0 , 0.2 ) ,
#'     bM ~ dnorm( 0 , 0.5 ) ,
#'     sigma ~ dexp( 1 )
#' ) , data = d )
#'
#' plot(coeftab(m5.1,m5.2), par=c("bA","bM") )
#' 
#' @rdname coeftab_plot
#' @name coeftab_plot
#' @aliases coeftab.plot
coeftab_plot <- function( x , y , pars , col.ci="black" , by.model=FALSE , prob=0.95 , xlab="Value" , ... ) {
  x.orig <- x
  xse <- x@se
  x <- x@coefs
  if ( !missing(pars) ) {
    x <- x[pars,]
    xse <- xse[pars,]
  }
  if ( !by.model ) {
    xse <- t(xse)
    x <- t(x)
  }
  
  z <- qnorm( 1-(1-prob)/2 )
  
  left <- x
  right <- x
  for ( k in 1:nrow(x) ) {
    for ( m in 1:ncol(x) ) {
      ci <- x[k,m] + c(-1,1)*z*xse[k,m]
      left[k,m] <- ci[1]
      right[k,m] <- ci[2]
    }
  }
  
  llim <- min(left,na.rm=TRUE)
  rlim <- max(right,na.rm=TRUE)
  dotchart( x , xlab=xlab , xlim=c(llim,rlim) , ... )
  
  for ( k in 1:nrow(x) ) {
    for ( m in 1:ncol(x) ) {
      if ( !is.na(left[k,m]) ) {
        # to compute y position:
        # coefs in groups by model
        # groups have nrow(x)+1 lines
        # empty line between each group
        kn <- nrow(x)
        ytop <- ncol(x)*(kn+2)-1
        ypos <- ytop - (m-1)*(kn+2) - (kn-k+1)
        lines( c(left[k,m],right[k,m]) , c(ypos,ypos) , lwd=2 , col=col.ci )
      }
    }
  }
  abline( v=0 , lty=1 , col=col.alpha("black",0.15) )
}

# @rdname coeftab_plot
setMethod( "plot" , "coeftab" , function(x,y,...) coeftab_plot(x,y,...) )
