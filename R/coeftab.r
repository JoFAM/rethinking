#' Coefficient tables
#' 
#' Returns a table of model coefficients in rows and models in columns. This can be used to compare estimates across models.
#' 
#' @param ... a series of fit models, separated by commas
#' @param se if \code{TRUE}, includes standard errors in the table
#' @param se.inside if \code{TRUE} print standard errors in the same cell as estimates.
#' @param nobs if \code{TRUE}, print number of observations for each model.
#' @param digits Number of digits to round numbers to
#' @param rotate if \code{TRUE}, rows are models and columns are coefficients.
#' 
#' @return an object of class \code{coeftab}
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
#' coeftab(m5.1,m5.2) 
#' 
#' @name coeftab
#' @export
coeftab <- function( ... , se=FALSE , se.inside=FALSE , nobs=TRUE , digits=2 , width=7 , rotate=FALSE ) {
    
    # se=TRUE outputs standard errors
    # se.inside=TRUE prints standard errors in parentheses in same column as estimates
    
    if ( se.inside ) se <- TRUE
    
    # retrieve list of models
    L <- list(...)
    if ( is.list(L[[1]]) && length(L)==1 )
        L <- L[[1]]
    
    # retrieve model names from function call
    mnames <- match.call()
    mnames <- as.character(mnames)[2:(length(L)+1)]
    
    # count number of unique parameters
    param.names <- {}
    for ( i in 1:length(L) ) {
        c.names <- names( xcoef( L[[i]] ) )
        param.names <- unique( c( param.names , c.names ) )
    }
    # columns for standard errors
    if ( se && !se.inside ) {
        for ( i in 1:length(L) ) {
            kse.names <- paste( names( xcoef( L[[i]] ) ) , ".se" , sep="" )
            param.names <- unique( c( param.names , kse.names ) )
        }
    }
    
    # make empty table
    nk <- length(param.names)
    d <- matrix( NA , ncol=nk )
    d <- data.frame(d)
    colnames(d) <- c( param.names )
    dse <- d
    
    # loop over models and insert values
    for ( i in 1:length(L) ) {
        klist <- xcoef( L[[i]] )
        selist <- xse( L[[i]] )
        for ( j in 1:length(klist) ) {
            d[i,][ names( klist[j] ) ] <- as.numeric( round( klist[j] , digits ) )
            dse[i,][ names( klist[j] ) ] <- as.numeric( selist[j] )
        }
    }
    # insert standard errors
    if ( se ) {
        for ( i in 1:length(L) ) {
            kse <- xse( L[[i]] )
            names(kse) <- names( xcoef( L[[i]] ) )
            for ( j in 1:length(kse) ) {
                if ( !se.inside )
                    # own column
                    d[i,][ paste(names(kse)[j],".se",sep="") ] <- as.numeric( round(kse[j],digits) )
                else
                    # combine with estimate
                    d[i,][ names(kse)[j] ] <- paste( formatC( (d[i,][ names(kse)[j] ]) , digits=digits ) , " (" , formatC( as.real( kse[j] ) , digits=digits ) , ")" , sep="" )
            }
        }
    }
    
    # add model names to rows
    rownames(d) <- mnames
    
    # formatting for parenthetical standard errors
    if ( se.inside && se ) {
        comment(d) <- "Values in parentheses are quadratic estimate standard errors."
        colnames(d) <- paste( colnames(d) , "(se)" )
        for ( i in 1:nrow(d) ) {
            for ( j in 1:ncol(d) ) {
                d[i,j] <- ifelse( is.na(d[i,j]) , "" , d[i,j] )
            }
        }
    }
    
    # add nobs
    if ( nobs ) {
        nobs <- sapply( L , xnobs )
    } else {
        nobs <- 0
    }
    
    # return table
    if ( !rotate ) {
        d <- t(d) # models along top is default
        dse <- t(dse)
    }
    new( "coeftab" , coefs=as.matrix(d) , se=as.matrix(dse) , nobs=nobs , digits=digits , width=width )
}
