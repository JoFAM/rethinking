#' Samples from quadratic posterior densities of models 
#' 
#' Samples from the posterior density of a fit model or models, assuming multivariate normality. This is a legacy function and is no longer supported nor unit tested. 
#' 
#' This function provides a way to draw parameter values from a multivariate normal posterior density, estimated from the maximum a posterieri (MAP) estimates and variance-covariance (\code{vcov}) of a fit model or models.
#' 
#' When passing a single fit model object, the function returns a data frame in which each row is a sample and each column is a parameter.
#' 
#' When passing a list of fit model objects, the function used to return a data frame containing samples from the joint posterior across model families. The fraction of rows drawn from a specific model family is determined by the \code{model.weights} parameter. BIC, AIC, or AICc are used to compute approximate predictive probabilities of each model family, and the total samples \code{n} is proportioned according to these estimates. The user can also supply a numeric vector of model weights, computed by any method. This vector should sum to 1.
#' 
#' @param model a fit model object.
#' @param n number of samples to draw from joint posterior
#' @param model.weights If passing a list of models, method for computing posterior probability of each model family. Can be "AIC","AICc","BIC" or a vector of numeric weights.
#' @param nobs Number of observations used to fit model or all models in list. Sometimes needed for \code{model.weights} values, like \code{AICc}.
#' @param add.names Adds a column of model names, when passing a list of models
#' @param fill.na Fills missing values with 0, by default, for model families that do not contain a given parameter. Useful for linear models. Hazardous for non-linear ones.
#' @param verbose If \code{TRUE}, prints various debugging information.
#' @param ... arguments passed to other functions.
#' 
#' @section WARNING:
#' This function is deprecated, and doesn't work any longer as described. Specifically for a list of functions, it doesn't return a sensible result.
#' 
#' @return A data frame with samples for every parameter.
#' 
#' @author Richard McElereath
#' 
#' @seealso \code{\link{mvrnorm}} for sampling from a multivariate normal distribution, 
#' \code{\link{extract.samples}} for collecting posterior samples from a \code{\link{quap}} \code{\link{map}} or \code{\link{map2stan}} model.
#' 
#' @examples 
#' 
#' data(cars)
#' flist <- alist(
#'     dist ~ dnorm( mu , sigma ) ,
#'     mu <- a+b*speed ,
#'     c(a,b) ~ dnorm(0,1) , 
#'     sigma ~ dexp(1)
#' )
#' fit <- quap( flist , start=list(a=40,b=0.1,sigma=20) , 
#'             data=cars )
#' extract.samples(fit)
#' 
#' \dontrun{
#' # The old way, giving a warning now
#' sample.naive.posterior(fit)
#' }
#' 
#' @export
sample.naive.posterior <- function( ... ) sample.qa.posterior( ... )

# TO DO: Check whether this is still necessary. It doesn't work
# as advertised for lists of functions any longer, and there's
# no AICc to be found anywhere.

#' @rdname sample.naive.posterior
#' @export
sample.qa.posterior <- function( model , n=10000 , clean.names=TRUE , model.weights="AICc" , nobs=0 , add.names=FALSE , fill.na=0 , verbose=FALSE ) {
    # Add warning about legacy
    
    warning("This is a legacy function. ")
    
    # need own BIC, as one in stats doesn't allow nobs
    getdf <- function(x) {
        if (!is.null(df <- attr(x, "df"))) 
            return(df)
        else if (!is.null(df <- attr(logLik(x), "df"))) 
            return(df)
    }
    myBIC <- function(x,nobs) {
        k <- getdf(x)
        as.numeric( -2*logLik(x) + log(nobs)*k )
    }
    if ( class(model)[1]=="list" ) {
        # list of models passed
        
        # check for list of length 1
        if ( length( model ) < 2 ) {
            return( sample.qa.posterior( model[[1]] , n=n , nobs=nobs ) )
        }
        
        # check method
        valid.methods <- c("AIC","AICc","BIC")
        use.custom <- FALSE
        if ( class(model.weights)=="numeric" ) {
            if ( length(model.weights)!=length(model) ) {
                stop( "Custom model weights must be same length as list of models." )
            } else {
                use.custom <- TRUE
                post <- model.weights
            }
        } else {
            if ( !any(model.weights==valid.methods) ) {
                stop( paste("Unknown model averaging method:",model.weights) )
            }
        }
        
        # compute from metric
        if ( !use.custom ) {
            if ( model.weights=="AIC" ) factors <- sapply( model , AIC )
            if ( nobs==0 ) {
                if ( model.weights=="AICc" ) factors <- sapply( model , AICc )
                if ( model.weights=="BIC" ) factors <- sapply( model , BIC )
            } else {
                if ( model.weights=="AICc" ) factors <- sapply( model , function(z) AICc(z,nobs=nobs) )
                if ( model.weights=="BIC" ) factors <- sapply( model , function(z) myBIC(z,nobs=nobs) )
            }
            factors <- factors - min( factors )
            # compute weights
            post <- exp( -1/2 * factors )/sum( exp( -1/2 * factors ) )
        }
        
        sim.post <- vector( "list" , length(model) )
        f.zeros <- FALSE
        nn <- round(n*post)
        if ( verbose ) print(nn)
        for ( i in 1:length(model) ) {
            if ( nn[i]==0 ) {
                f.zeros <- TRUE
                sim.post[[i]] <- sample.qa.posterior( model[[i]] , n=2 ) 
                # 2 appears to be minimum to get columns to populate from mvrnorm
                # need column names from these zero samples models
            } else {
                sim.post[[i]] <- sample.qa.posterior( model[[i]] , n=max(nn[i],2) )
            }
        }
        if ( f.zeros ) {
            warning( "One or more models produced zero samples, because of very low posterior probability." )
        }
        # new algorithm
        if ( TRUE ) {
            # build list of unique parameter names
            par.names <- sapply( sim.post , function(i) colnames( i ) )
            upar.names <- unique( unlist( par.names ) )
            # build averaged posterior
            post.avg <- matrix( NA , nrow=sum(nn) , ncol=length(upar.names) )
            colnames(post.avg) <- upar.names
            # insert samples
            current.row <- 1
            for ( i in 1:length(model) ) {
                if ( nn[i] > 0 ) {
                    start.row <- current.row
                    end.row <- current.row + nn[i] - 1
                    for ( j in colnames(sim.post[[i]]) ) {
                        post.avg[ start.row:end.row , j ] <- sim.post[[i]][ 1:nn[i] , j ]
                    }
                    current.row <- end.row + 1
                }
            }
            post.avg <- as.data.frame( post.avg )
        } else {
        # old algorithm
            post.avg <- sim.post[[1]]
            # post.avg <- merge( sim.post[[1]] , sim.post[[2]] , all=TRUE , sort=FALSE )
            if ( length( model ) > 1 ) {
                for ( i in 2:length(model) ) {
                    # if ( nn[i] > 0 ) 
                    post.avg <- merge( post.avg , sim.post[[i]] , all=TRUE , sort=FALSE )
                    if ( nn[i] == 0 ) {
                        nr <- nrow(post.avg)
                        rcut <- (nr-1):nr
                        post.avg <- post.avg[ -rcut , ]
                    }
                }
                if ( nn[1] == 0 ) {
                    post.avg <- post.avg[ -(1:2) , ]
                }
            } # end old algorithm
        }
        if ( !is.logical(fill.na) )
            post.avg[is.na(post.avg)] <- fill.na
        if ( add.names ) {
            mnames <- match.call()
            mnames <- as.character(mnames[[2]])[2:(length(model)+1)]
            rnames <- rep( mnames , times=nn )
            post.avg$model <- rnames
        }
        result <- post.avg
    } else {
        # single model passed
        mu <- 0
        if ( class(model)[1] %in% c("mer","bmer","glmerMod","lmerMod") ) {
            mu <- fixef(model)
        } else {
            mu <- xcoef(model)
        }
        result <- as.data.frame( mvrnorm( n=n , mu=mu , Sigma=vcov(model) ) )
        if ( clean.names ) {
            # convert (Intercept) to Intercept
            for ( i in 1:ncol(result) ) {
                if ( colnames(result)[i] == "(Intercept)" ) {
                    colnames(result)[i] <- "Intercept"
                }
            }
        }
    }
    result
}

# converting to use extract_samples for everything

