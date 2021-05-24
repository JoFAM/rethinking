# IC class and show method
# TO DO: Figure out where this one is actually used.
# It doesn't make sense to have an S4 class and then
# expect an S3-style attribute.

setClass( "ICnumeric" , contains="numeric" )

setMethod( "show" , "ICnumeric" , function(object) {
    cat( round( object , 1 ) )
    if ( !is.null( attr(object,"se") ) ) {
        cat( concat( " \u00B1 " , round( attr(object,"se") , 1 ) ) )
    }
    cat("\n")
} )
