#' Build and check integer index variables
#' 
#' These functions assist with building (\code{coerce_index}) and checking (\code{check_index}) integer index variables of the kind needed to define varying effect models.
#' 
#' Varying effect models often require index variables that begin at 1 and comprise only integers. These variables are used to lookup specific parameter values inside of the model. For example, it is common to define varying intercepts with a linear model like \code{a0 + a_id[id[i]]}. Here the variable \code{id} is an index variable. It has one value for each case, defining which individual applies to that case.
#' 
#' When raw data exist as factors, these index variables much be converted to integers. This is trickier than it sounds, because R uses an internal integer represntation for factors, \code{levels}, that can conflict with ordinary integer representations.
#' 
#' The function \code{coerce_index} deals with that complication. When the input is a single vector of factors, it returns an integer vector beginning at 1 and with contiguous values.
#' 
#' When the input is instead a comma-separated list of factors, it returns a list in which each factor has been converted to integers, but all levels in all factors were merged so that the same labels across factors always have the same integer values in the result. For example, suppose cases refer to dyads and there are two factors, \code{id1} and \code{id2}, that indicate which pair of individuals are present in each dyad. The labels in these variables should refer to the same individuals. Passing both simultaneously to \code{coerce_index} ensures that the results respect that fact.
#' 
#' The function \code{check_index} merely checks an integer vector to see if it is contiguous.
#' 
#' @param ... a comma-separated list of variables. See details.
#' @param x a vector of integers to check for contiguity
#' 
#' @return For \code{coerce_index}, the result is either a single vector of integers (if the input was a single vector) or rather a list of vectors of integers (if the input was a list of vectors).
#' 
#' @author Richard McElreath with adaptations by Joris Meys
#' 
#' @examples 
#' 
#' d <- data.frame(id1 = c("a","b", NA, "b","a","c"),
#'                 id2 = c("b", "c", "b", "a","d","a"))
#' coerce_index(d$id1)
#' coerce_index(d)
#' 
#' @export
coerce_index <- function( ... ){
  # grab data
  L <- list(...)
  # Check if a list or df is passed
  if(length(L) == 1 && is.list(L[[1]])){
    L <- L[[1]]
    onelist <- TRUE
  }  else {
    onelist <- FALSE
  }
  nfac <- length(L)
  
  if(nfac == 1){
    out <- as.integer(as.factor(L[[1]]))
    # as.factor checks whether it is a factor, and only
    # does conversion when necessary
  } else {
    # Check whether all factors have the same length,
    # otherwise this doesn't make sense. 
    nobs <- unique(sapply(L,length))
    if(length(nobs) != 1){
      stop("All factors need to be of the same length.")
    }
    # check names
    if(is.null(names(L)) && onelist){
      # a list without names needs to get names
      # match.call doesn't help, as it will get the name 
      # of the list.
      vnames <- paste0("X",seq_len(nfac))
      
    } else if(is.null(names(L))){
      # Get the names from the match call
      vnames <- as.character(match.call())[-1]
      # make valid names
      vnames <- make.names(vnames)
    } else {
      vnames <- names(L)
    }
    # PROCESS ALL 
    # works with new factor combination in R4.0
    tmp <- as.integer(as.factor(unlist(L)))
    out <- vector("list", nfac)
    for(i in seq_len(nfac)){
      out[[i]] <- tmp[seq_len(nobs)+ nobs*(i-1) ]
    }
    names(out) <- vnames
  }
  return(out)
}

#' @rdname coerce_index
#' @aliases check_index
#' @export
check_index <- function( x ) {
  y <- sort(unique(x))
  n <- length(y)
  message( concat( "Length: ",n ) )
  message( concat( "Range: ",min(y)," / ",max(y) ) )
  if ( max(y) != n ) message( "Maximum index different than number of unique values" )
  diffs <- sapply( 2:n , function(i) y[i] - y[i-1] )
  if ( any(diffs!=1) ) message( "At least one gap in consecutive values" )
  
  return(invisible(any(diffs!=1) || max(y) != n ))
}
