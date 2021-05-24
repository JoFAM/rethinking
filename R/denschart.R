# dot-bar chart with density profiles instead of dots and bars
# takes a list X with densities represented as vectors (or matrixes) of samples

denschart <- function (x, pars, 
    labels = NULL, groups = NULL, gdata = NULL, cex = par("cex"), 
    pt.cex = cex, bg = par("bg"), color = "gray", 
    gcolor = par("fg"), lcolor = "gray", xlim = range(unlist(x)), 
    main = NULL, xlab = NULL, ylab = NULL, height=0.7 , border=NA, adjust=1, alpha=0.4, 
    draw.zero=function()abline(v=0,lty=2,lwd=0.5), 
    label_vectors=TRUE, drop_matrices=FALSE, drop_prefix=c("z_","L_"),
    sort=FALSE, 
    ...) 
{
    single_real <- function(x) {
        u <- unique(x)
        if ( length(u)==1 ) return(TRUE)
        if ( all.equal(u,rep(u[1],length(u)))[1] ) return(TRUE)
        return(FALSE)
    }
    opar <- par("mai", "mar", "cex", "yaxs")
    on.exit(par(opar))
    par(cex = cex, yaxs = "i")
    if (class(x) %in% c("map2stan","map","stanfit")) {
        x <- extract.samples(x)
        if ( inherits(x, "data.frame")) {
            x <- as.list(x)
            for ( i in 1:length(x) ) x[[i]] <- as.array(x[[i]])
        }
    }
    if (!is.list(x)) 
        stop("'x' must be a list of vectors or matrices")
    n <- length(x)
    glabels <- NULL
    if (is.list(x)) {
        if (is.null(labels)) 
            labels <- names(x)
        if (is.null(labels)) 
            labels <- as.character(1L:n)
        labels <- rep_len(labels, n)
        #if (is.null(groups)) 
        #    groups <- col(x, as.factor = TRUE)
        #glabels <- levels(groups)
        # check for matrix parameters and convert each row to a separate entry in list
        x_new <- list()
        flag_change <- FALSE
        for ( i in 1:n ) {
            if ( any( !is.na(pmatch(drop_prefix,labels[i])) ) ) {
                flag_change <- TRUE
                next
            }
            if ( !missing(pars) ) {
                if ( !(labels[i] %in% pars) ) {
                    flag_change <- TRUE
                    next
                }
            }
            nd <- length(dim(x[[i]]))
            if ( nd==0 ) {
                # no dim, so probably a numeric vector
                # convert to 1D array
                x[[i]] <- as.array(x[[i]])
            }
            if ( nd==3 ) {
                # matrix
                if ( !drop_matrices )
                    for ( j in 1:(dim(x[[i]])[2]) ) {
                        new_label <- concat( labels[i] , "[" , j , "]" )
                        x_new[[ new_label ]] <- x[[i]][,j,]
                    }#j
                flag_change <- TRUE
            } else {
                # single parameter or vector of parameters, so leave unchanged
                x_new[[ labels[i] ]] <- x[[i]]
            }
        }#i
        if ( flag_change ) {
            x <- x_new
            n <- length(x)
            labels <- names(x)
        }
    }
    plot.new()
    linch <- if (!is.null(labels)) 
        max(strwidth(labels, "inch"), na.rm = TRUE)
    else 0
    if (is.null(glabels)) {
        ginch <- 0
        goffset <- 0
    }
    else {
        ginch <- max(strwidth(glabels, "inch"), na.rm = TRUE)
        goffset <- 0.4
    }
    if (!(is.null(labels) && is.null(glabels))) {
        nmai <- par("mai")
        nmai[2L] <- nmai[4L] + max(linch + goffset, ginch) + 
            0.1
        par(mai = nmai)
    }
    if (is.null(groups)) {
        o <- 1L:n
        y <- o
        ylim <- c(0, n + 1)
    }
    else {
        # sub-groups, so need more rows
        o <- sort.list(as.numeric(groups), decreasing = TRUE)
        x <- x[o]
        groups <- groups[o]
        color <- rep_len(color, length(groups))[o]
        lcolor <- rep_len(lcolor, length(groups))[o]
        offset <- cumsum(c(0, diff(as.numeric(groups)) != 0))
        y <- 1L:n + 2 * offset
        ylim <- range(0, y + 2)
    }
    plot.window(xlim = xlim, ylim = ylim, log = "")
    lheight <- par("csi")
    if (!is.null(labels)) {
        linch <- max(strwidth(labels, "inch"), na.rm = TRUE)
        loffset <- (linch + 0.1)/lheight
        labs <- labels[o]
        mtext(labs, side = 2, line = loffset, at = y, adj = 0, 
            col = "black", las = 2, cex = cex, ...)
    }
    #abline(h = y, lty = "dotted", col = lcolor)
    #points(x, y, pch = pch, col = color, bg = bg, cex = pt.cex/cex)

    # draw zero line?
    if (!is.null(draw.zero)) {
        do.call(draw.zero,list())
    }

    # draw densities at each y offset
    for ( i in 1:n ) {
        nd <- length(dim(x[[i]]))
        if ( nd==1 ) {
            # single parameter
            if ( !single_real(x[[i]]) ) {
                a <- density( x[[i]] , adjust=adjust )
                a$y <- a$y/max(a$y) * height + y[i] - 0.3
                polygon( a$x , a$y , col=color , border=border )
            }
        }#1
        if ( nd==2 ) {
            # vector of parameters
            m <- dim(x[[i]])[2]
            x_label <- rep(NA,m)
            # first get all densities, so can normalize heights together
            a <- list()
            d_max <- 0
            for ( j in 1:m ) {
                a[[j]] <- density( x[[i]][,j] , adjust=adjust )
                if ( !single_real(x[[i]][,j]) ) {
                    y_max <- max(a[[j]]$y)
                    d_max <- ifelse( y_max > d_max , y_max , d_max )
                }
            }
            color <- rep_len(color, m)
            for ( j in 1:m ) {
                if ( !single_real(x[[i]][,j]) ) {
                    a[[j]]$y <- a[[j]]$y/d_max * height + y[i] - 0.3
                    polygon( a[[j]]$x , a[[j]]$y , col=col.alpha(color[j],alpha) , border=border )
                    x_label[j] <- a[[j]]$x[ a[[j]]$y==max(a[[j]]$y) ][1]
                }
            }#j
            if ( label_vectors )
                for ( j in 1:m )
                    if ( !single_real(x[[i]][,j]) )
                        text( x_label[j] , y[i] , labels=as.character(j) , cex=0.6 )
        }#2
        if ( nd==3 ) {
            # matrix of parameters
            # show each row in matrix on a separate row in plot
        }#3
    }#i

    if (!is.null(groups)) {
        gpos <- rev(cumsum(rev(tapply(groups, groups, length)) + 
            2) - 1)
        ginch <- max(strwidth(glabels, "inch"), na.rm = TRUE)
        goffset <- (max(linch + 0.2, ginch, na.rm = TRUE) + 0.1)/lheight
        mtext(glabels, side = 2, line = goffset, at = gpos, adj = 0, 
            col = gcolor, las = 2, cex = cex, ...)
        if (!is.null(gdata)) {
            abline(h = gpos, lty = "dotted")
            points(gdata, gpos, pch = gpch, col = gcolor, bg = bg, 
                cex = pt.cex/cex, ...)
        }
    }
    axis(1)
    box()
    title(main = main, xlab = xlab, ylab = ylab, ...)
    invisible()
}

# TEST code moved to tests/rethinking_tests/denschart_tests.R
