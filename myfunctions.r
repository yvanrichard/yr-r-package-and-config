## package.skeleton(list=ls(), name="mypkg")


###############################################################################
###  VECTOR/MATRIX MANIPULATION
###############################################################################

myreplace <- function(what, lookup, verbose=T, warn=T) {
    ## replace values in a vector by corresponding ones using a lookup table
    ## ex: lookup <- c(
    ## oldval1, newval1,
    ## oldval2, newval2) 
    w <- what
    lkup <- matrix(lookup, ncol=2, byrow=T)
    c <- w %in% lkup[,1]
    if (warn) {
        cond <- !(lkup[,1] %in% what)
        if (sum(cond))
            warning(sum(cond),' value(s) in lookup vector not in original data')
    }
    w[c] <- lkup[match(w[c], lkup[,1]), 2]
    s <- sum(w != what, na.rm=T)
    if (verbose)
        cat('\nReplaced ', s, ' values out of ', length(w), ' (', round(100*s/length(w),2), '%).\n',
            sep='')
    return(w)
}


sample_df <- function(df, n=10) {
    df[sort(sample(1:nrow(df))[1:n]),]
}

## sample from a vector without knowing its length (useful in sapply functions)
sample_in <- function(x, n=100, ...) {
    if (length(dim(x)))
        stop('Not a vector') else
    return(x[sample(1:length(x), n, ...)])
}


movecolumns <- function(df, cols, where=1) {
    colids <- match(cols, colnames(df))
    others <- which(!(colnames(df) %in% cols))
    if (where==1)
        return(df[,c(colids, others)]) else {
            if (where==ncol(df))
                return(df[,c(others, colids)])  else {
                    return(df[,c(others[others < where],
                                 colids,
                                 others[others >= where])])
                }
        }
}


duplicated_rows <- function(df, cols=names(df)) {
    id <- apply(df[,cols,drop=F], 1, paste, collapse='_')
    isdup <- duplicated(df[,cols,drop=F])
    d <- df[id %in% id[isdup], , drop=F]
    id <- apply(d[, cols, drop=F], 1, paste, collapse = "_")
    return(d[order(id),, drop=F])
}

## Merge on row names (and get rid of Row.names column created)
merge0 <- function(x, y, ...) {
    mrg <- merge(x, y, by=0, ...)
    rownames(mrg) <- mrg$Row.names
    mrg$Row.names <- NULL
    return(mrg)
}

## Cbind using row names (even if some rows are missing in x or y)
cbind0 <- function(x, y) {
    row.names <- sort(unique(c(rownames(x), rownames(y))))
    z <- as.data.frame(cbind(x[row.names, , drop=F], y[row.names, , drop=F]))
    rownames(z) <- row.names
    return(z)
}



###############################################################################
###  TEXT MANIPULATION
###############################################################################

## Function to test if text is a number
is_num_txt <- function(x)
    return(suppressWarnings(!is.na(as.numeric(x))))

## Lower 1st letter (mainly for species)
lower1st <- function(x) {
    ##     x <- dat$name
    x1 <- strsplit(x, '')
    x <- sapply(x1, function(x) {
                   x[1] <- tolower(x[1]) 
                   return(paste(x, collapse=''))
               })
    return(x)
}

upper1st <- function (x, prelower = T) {
    if (is.factor(x)) 
        x0 <- levels(x)
    else x0 <- as.character(x)
    if (prelower) x0 <- tolower(x0)
    nc <- nchar(x0)
    nc[is.na(nc)] <- 0
    x0[nc > 1] <- sprintf("%s%s", toupper(substr(x0[nc > 1], 1, 1)),
                          substr(x0[nc > 1], 2, nchar(x0[nc > 1])))
    x0[nc == 1] <- toupper(x0[nc == 1])
    if (is.factor(x)) 
        levels(x) <- x0
    else x <- x0
    return(x)
}

## from help of chartr()
capwords <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s,1,1)), {
                         s <- substring(s,2); if(strict) tolower(s) else s},
                     sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

## Returns string of comma-separated values, with quotes (or not)
Pprint <- function(x, sep=',', quotes="\"") {
    if (!is.null(quotes)) {
        out <- paste(sprintf('%1$s%2$s%1$s', quotes, x), collapse=sep)
    } else {
        out <- paste(sprintf('%s', x), collapse=sep)
    }
    cat(out, sep='\n')
    return(out)
}

## Function to return a number in scientific notation
sn <- function(x,digits) {
    if (x==0) return("0")
    ord <- floor(log(abs(x),10))
    x <- x / 10^ord
    if (!missing(digits)) x <- format(x,digits=digits)
    if (ord==0) return(as.character(x))
    if (x==1)
        return(sprintf("$10^{%s}$", ord)) else 
    return(sprintf("$%s\\\\times 10^{%s}$", x, ord))
}

trim <- function(x) {
    x2 <- sapply(x, function(y) gsub('^ *','', y), USE.NAMES=F)
    x3 <- sapply(x2, function(y) gsub(' *$','', y), USE.NAMES=F)
    return(x3)
}
strtrim <- function(x) {
    x2 <- gsub('^[[:blank:]]*', '', x)
    x3 <- gsub('[[:blank:]]*$', '', x2)
    return(x3)
}

strclean <- clean.string <- function(x) {
    ## remove heading and trailing spaces, multiple spaces, multiple tabs, and consecutive tabs or spaces
    return(gsub('[\t ]{2,}', ' ', gsub('\t{2,}', '\t', gsub(' {2,}', ' ', gsub('^ *| *$', '', x)))))
}

sentence <- function(x) {
    ## Make a sentence from a vector of characters
    if (length(x) == 2) {
        sent <- paste(x, collapse=' and ')
    } else if (length(x) > 2) {
        sent <- paste0(paste(x[1:(length(x)-1)], collapse=', '), ', and ', x[length(x)])
    } else sent <- x
    return(sent)
}

numlitt <- function(x, cap=F) {
    ## Turn integers into their english word up to 12 included
    if (length(x) > 1)   stop('More than one value for numlitt')
    if (!is.numeric(x))  x <- as.numeric(x)
    if (!identical(round(x), as.numeric(x)))  stop('x is not in an integer form: ', x)
    if (!is.finite(x))   stop('x not finite')
    litt <- c('one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine',
              'ten', 'eleven', 'twelve')
    if (x == 0) {
        res <- 'zero'
    } else if (x <= length(litt)) {
        res <- litt[x]
    } else res <- as.character(x)
    if (cap)  res <- upper1st(res)
    return(res)
}



###############################################################################
###  PLOTS
###############################################################################

alpha <- function (colour, alpha) {
    ## alpha() function from ggplot2
    alpha[is.na(alpha)] <- 0
    col <- col2rgb(colour, TRUE)/255
    if (length(colour) != length(alpha)) {
        if (length(colour) > 1 && length(alpha) > 1) {
            stop("Only one of colour and alpha can be vectorised")
        }
        if (length(colour) > 1) {
            alpha <- rep(alpha, length.out = length(colour))
        }
        else if (length(alpha) > 1) {
            col <- col[, rep(1, length(alpha)), drop = FALSE]
        }
    }
    col[4, ] <- ifelse(col[4, ] == 1, alpha, col[4, ])
    new_col <- rgb(col[1, ], col[2, ], col[3, ], col[4, ])
    new_col[is.na(colour)] <- NA
    return(new_col)
}

alphagrad <- function(n, grad='exp', curv=20, inv=F) {
    if (curv<=1) stop('Choose a value above 1')
    if (grad=='exp') {
        alphas <- exp(seq(curv,1,length.out=n))
        alphas <- (alphas-min(alphas))/max(alphas-min(alphas))
    } else if (grad=='log') {
        alphas <- log(seq(curv,1,length.out=n))
        alphas <- (alphas-min(alphas))/max(alphas-min(alphas))
    } else alphas <- seq(1,0,length.out=n)
    if (inv)
        cols <- rev(alpha(rep('#FFFFFF',n),alphas)) else #
    cols <- alpha(rep('#FFFFFF',n),alphas)               #
    return(cols)
}

applyalphagrad <- function(colors, grad='exp', curv=15, inv=F, startalpha=0, endalpha=1) {
    n  = length(colors)
    if (curv<=1) stop('Choose a value above 1')
    if (grad=='exp') {
        alphas <- exp(seq(curv,1,length.out=n))
    } else if (grad=='log') {
        alphas <- log(seq(curv,1,length.out=n))
    } else 
        alphas <- seq(1,0,length.out=n)
    alphas <- scaleminmax(alphas, startalpha, endalpha)
    if (inv)
        cols <- rev(alpha(colors,alphas)) else
    cols <- alpha(colors,alphas)
    return(cols)
}



scaleminmax <- function(in.seq, out.min=0, out.max=1, in.ref.min=NA, in.ref.max=NA) {
    ## in.seq=sort(rnorm(10)); out.min=0.2; out.max=0.8
    if (!is.na(in.ref.min))
        in.seq <- c(in.ref.min, in.seq)
    if (!is.na(in.ref.max))
        in.seq <- c(in.ref.max, in.seq)
    in.max = max(in.seq, na.rm=T)   
    in.min = min(in.seq, na.rm=T)
    in.range = diff(range(c(in.min, in.max)))
    out.range = diff(range(c(out.min, out.max)))
    if (in.range>0)
        ratio = out.range/in.range else
    ratio = 1
    out.seq <- (in.seq-in.min)*ratio + out.min
    if (!is.na(in.ref.max))
        out.seq <- out.seq[-1]
    if (!is.na(in.ref.min))
        out.seq <- out.seq[-1]
    return(out.seq)
}

colorfrom <- function(var, colors=c('black','red','gold','darkgreen'), ncolors=100, alpha=1, rev=F, ...) {
    library(RColorBrewer)
    if (is.factor(var) | is.character(var)) {
        if (!is.factor(var))  var <- factor(var, levels=unique(var))
        colors <- rep(colors, length.out=nlevels(var))
        names(colors) <- levels(var)
        Cols <- colors[match(var, names(colors))]
    } else {
        if (rev) colors <- rev(colors)
        cols <- colorRampPalette(colors)(ncolors)
        Cols <- cols[round(scaleminmax(var, out.min=1, out.max=ncolors, ...))]
    }
    return(alpha(Cols, alpha))
}

interms <- function(y, min=0) {
    if (max(y)<5)
        float = T else
    float = F
    x <- log(y+1)
    x <- max(x)
    if (!float)
        return(round(c(min, signif(exp(x/4)-1, 1), signif(exp(x/2)-1,1), signif(exp(3*x/4)-1,1), exp(x)-1))) else
    return(c(min, signif(exp(x/4)-1, 1), signif(exp(x/2)-1,1), signif(exp(3*x/4)-1,1), round(exp(x)-1,3)))
}

getmfrow <- function(n, dim=c(7,9)) {
    rat <- dim[2]/dim[1]
    d <- expand.grid(y=1:n, x=1:n)
    d <- d[,c('x','y')]
    d$z <- d$x*d$y
    d <- do.call('rbind', by(d, d$x, function(x) {
                                x2 <- x[x$z >= n,]
                                return(x2[which.min(x2$y),])
                            }))
    d$r <- d$y/d$x
    rr <- d[which.min(abs(d$r-rat)),]
    return(c(rr$x, rr$y))
}

## Draw envelope, given the lower and upper limits
draw.env <- function(x=1:length(lcl), lcl, ucl, fill=gray(0.9), line=gray(0.75)) {
    ## x = 1:length(lcl)
    polygon(c(x,rev(x),x[1]),c(ucl, rev(lcl), ucl[1]), border=NA, col=fill)
    lines(x, ucl, col=line)
    lines(x, lcl, col=line)
}


## Tile several windows
tile <- function(n=2,            # number of screens
                 ratio=0.8,     # heigth as a proportion of width of each screen
                 screen.size=c(1920,1080), # total screen dimension
                 res=c(92,91),  # screen resolution in dots per inch
                 smar=c(50,50), # left and bottom margins of screen (for menu bars)
                 dec=c(10,30),   # window decoration size (px)
                 clean=T        # remove pre-existing graphics screen
                 ) {
    if (clean) {
        graphics.off()
        dn <- 2
    } else {
        if (!is.null(dev.list()))
            dn <- max(dev.list()) else dn <- 2
    }
    lay <- getmfrow(n, dim=c(1,ratio))

    W <- (screen.size[1]-smar[1]-lay[1]*dec[1]) / lay[1]
    H <- (screen.size[2]-smar[2]-(lay[2]-1)*dec[2]) / lay[2]
    if (!is.na(ratio))  H <- min(ratio*W, H)
    
    xposs <- smar[1] +  ((1:lay[1])-1) * W  + ((1:lay[1])-1)*dec[1]
    yposs <- ((1:lay[2])-1) * H + ((1:lay[2])-1)*dec[2]
    ni=0
    for (yi in 1:lay[2])
        for (xi in 1:lay[1]) {
            ni <- ni + 1
            if (ni <= n)
                x11(xpos=xposs[xi], ypos=yposs[yi], width=W/res[1], height=H/res[2])
        }
    dev.set(dn)
}

col3d <- function(x, y, z, space='rgb') {
    f <- switch(space, rgb=rgb, hcl=hcl, hsv=hsv) 
    return(f(scaleminmax(x), scaleminmax(y), scaleminmax(z)))
}



## Calculate density of y for each x, and plot them on same plot
## col='red'; alph=0.5; col.border=NULL; xmin=NULL; at=NULL; ylevels=NULL; ylab=NA; cap1st=T
plotdensapply <- function(y, by, col='red', alph=0.5, col.border=NULL, xmin=NULL, at=NULL,
                  ylevels=NULL, ylab=NA, cap1st=T, gridx=0, gridy=0,
                  gridcol=grey(0.8), ltxtcex=1, ...) {
    if (!is.factor(by))
        by <- factor(by, levels=sort(unique(by)))
    by <- factor(by, levels=rev(levels(by)))
    spp <- levels(by)
    nspp <- nlevels(by)
    dd <- tapply(y, by, density)
    minx <- min(sapply(dd, function(x) min(x$x)))
    maxx <- max(sapply(dd, function(x) max(x$x)))
    rngx <- extendrange(c(minx, maxx), f=0.04)
    plot(NA, xlim=switch(is.null(xmin)+1, c(xmin, rngx[2]), rngx), ylim=c(0,nspp+1),
         ylab=ylab, yaxs='i', xaxs='i', yaxt='n', ...)
    grid(gridx, gridy, gridcol)
    if (is.null(at))   at <- 1:nspp
    if (length(at) != nspp) stop('Length of "at" does not match that of levels of "by"')
    if (length(col)==1)  col <- rep(col, nspp)
    if (is.null(col.border))  col.border <- col
    col <- rev(col)
    col.border <- rev(col.border)
    for (di in 1:nspp) {
        d1 <- dd[[di]]
        d1$y2 <- d1$y/max(d1$y)
        polygon(c(d1$x, rev(d1$x)), .5*c(d1$y2,-rev(d1$y2))+at[di], col=alpha(col[di],alph),
                border=col.border[di], lwd=.5)
    }
    if (is.null(ylevels)) ylevels <- spp
    z <- sapply(tapply(ylevels, at, unique), function(x) paste(x, collapse=','))
    ylev2 <- z; at2 <- as.numeric(names(z))
    if (cap1st) ylev2 <- upper1st(ylev2)
    mtext(ylev2, 2, at=at2, las=1, line=0.5, cex=ltxtcex)
}



lighten <- function(col, c=.5) {
    rgbs <- col2rgb(col, alpha=T)
    r <- rgbs['red',]/255
    g <- rgbs['green',]/255
    b <- rgbs['blue',]/255
    a <- rgbs['alpha',]/255
    rd <- r + c*(1-r)
    gd <- g + c*(1-g)
    bd <- b + c*(1-b)
    newrgbs <- rgb(rd,gd,bd,a)
    names(newrgbs) <- names(col)
    return(newrgbs)
}

darken <- function(col, c=.3) {
    rgbs <- col2rgb(col, alpha=T)
    r <- rgbs['red',]/255
    g <- rgbs['green',]/255
    b <- rgbs['blue',]/255
    a <- rgbs['alpha',]/255
    rd <- r * (1-c)
    gd <- g * (1-c)
    bd <- b * (1-c)
    newrgbs <- rgb(rd,gd,bd,a)
    names(newrgbs) <- names(col)
    return(newrgbs)
}


placeleg <- function(X, Y, ...) {
    poss <- c('topleft','top','topright','left','center','right','bottomleft','bottom','bottomright')
    d <- NULL
    pos <- poss[1]
    for (pos in poss) {
        l <- legend(pos, ..., plot=F)
        d <- c(d, sum(X >= l$rect$left & X <= (l$rect$left + l$rect$w) &
                     Y >= (l$rect$top - l$rect$h) & Y <= l$rect$top))
    }
    pos <- poss[which.min(d)]
    legend(pos, ...)    
}

pal.ex <- function(cols, cex=1.3) {
    ncols <- length(cols)
    par(mfrow=c(2,2), mar=c(2,2,1,1))
    ## bar plot
    x <- table(round(runif(100, 0.5, ncols+0.5)))
    barplot(x, col=cols)
    ## scatter plot
    plot(rnorm(1000), rnorm(1000), col=cols, pch=20, cex=cex)
    ## lines
    nl <- max(10, ncols)
    x <- apply(matrix(rnorm(100*nl), nrow=nl), 1, cumsum)
    matplot(x, type='l', col=cols, lwd=cex)
    ## gradients
    ncols <- ncols * 2
    plot(NA, xlim=c(0,5), ylim=c(1, ncols+1), axes=F, xaxs='i', yaxs='i')
    cols1 <- rep(cols, each=2)
    for (i in 1:ncols)
        rect(0, i, 1, i+1, col=cols1[i], border=NA)
    for (i in 1:ncols)
        rect(4, i, 5, i+1, col=rev(cols1)[i], border=NA)
    cols2 <- c(cols, rev(cols))
    for (i in 1:ncols)
        rect(2, i, 3, i+1, col=cols2[i], border=NA)
}

color.gradient <- function(x, colors=c("red","yellow","green"), colsteps=100) {
    return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}

pickcolor <- function(brewer=T, echo=T) {
    darken <- function (col, c = 0.3) {
        rgbs <- col2rgb(col, alpha = T)
        r <- rgbs["red", ]/255
        g <- rgbs["green", ]/255
        b <- rgbs["blue", ]/255
        a <- rgbs["alpha", ]/255
        rd <- r * (1 - c)
        gd <- g * (1 - c)
        bd <- b * (1 - c)
        newrgbs <- rgb(rd, gd, bd, a)
        names(newrgbs) <- names(col)
        return(newrgbs)
    }
    if (!brewer) {
        x11(width=15.2, height=8.2)
        cs <- colors()
        n <- length(cs)
        xx <- c(0, (1:n %% 40)[-length(1:n)])
        yy <- cumsum(xx==0)
        yy <- (max(yy)+1)-yy
        d <- data.frame(x=xx, y=yy, col=cs, stringsAsFactors=F)
        plot(xx, yy, col=darken(cs,.2), bg=cs, pch=22, cex=5, axes=F, xlab='', ylab='',
             main='Pick your colours', xlim=range(xx), ylim=c(-1, max(yy)))
        xy <- locator(type='p', pch=4)
        xy <- sapply(xy, round)
        cols <- merge(xy, d, all.x=T, all.y=F)$col
    } else {
        x11(width=19, height=6.8)
        library(RColorBrewer)
        info <- brewer.pal.info
        np <- nrow(info)
        info$y <- 1:np
        d <- NULL
        for (i in 1:np) {
            mx <- info[i, 'maxcolors']
            cs <- brewer.pal(mx, rownames(info)[i])
            d <- rbind(d, data.frame(x=rep(i, mx),
                                     y=1:mx,
                                     col=brewer.pal(mx, rownames(info)[i]),
                                     stringsAsFactors=F))
        }
        d$xy <- paste(d$x, d$y, sep='-')
        plot(NA, xlim=c(min(d$x)-1, max(d$x)+1), ylim=c(min(d$y)-2, max(d$y)+1),
             main='Pick your colours', axes=F,
             xlab='', ylab='')
        points(d$x, d$y, col=darken(d$col, .2), bg=d$col, pch=22, cex=5)
        text(1:np, rep(c(-.6,0), length.out=np), rownames(info))
        xy <- locator(type='p', pch=4)
        xy <- sapply(xy, round, simplify=F)
        xy$xy <- paste(xy$x, xy$y, sep='-')
        xy <- as.data.frame(xy)
        cols <- merge(xy, d, all.x=T, all.y=F, by.x='xy', by.y='xy', sort=F)$col
    }
    dev.off()
    if (echo) {
        op <- options("useFancyQuotes")
        options("useFancyQuotes"=F)
        c <- paste(sQuote(cols), collapse=', ')
        cat('\n', c, '\n\n')
        options("useFancyQuotes"=op)
    }
    return(cols)
}



###############################################################################
###  MAPS / GIS
###############################################################################

## Points to FMA
getfma <- function(df, xcolumn, ycolumn) {
    library(rgdal)
    fmas <- readOGR('/dragonfly/gis/shapes/mfish','fma')
    df$x <- df[[xcolumn]]
    df$y <- df[[ycolumn]]

    coordinates(df) = ~x+y
    proj4string(df) <- CRS("+proj=longlat +datum=WGS84")

    return(overlay(df, fmas))
}


## NZ basemap
## corners=list(x=c(162,189), y=c(-54,-30))
nz <- function(type='contour', corners=list(x=c(162,189), y=c(-54,-30)), pshift=0.005, simple=F, title=NA) {
    if (!length(dev.list()))
        par(mar=c(0,0,0,0))

    require(maptools)
    gpclibPermit()           # enable licence for polygon computations

    minx <- min(corners$x)
    maxx <- max(corners$x)
    miny <- min(corners$y)
    maxy <- max(corners$y)
    xshift <- diff(range(corners$x))*pshift
    yshift <- -diff(range(corners$y))*pshift

    cols <- colorRampPalette(c("#AAAAFF", "#EEEEFF")) #
    ## Load NZ contour and bathymetry maps
    NZ <- Rgshhs('/dragonfly/gis/gshhs/gshhs_i.b', level=1, 
                 xlim=c(minx,maxx), ylim=c(miny,maxy))
    
    plot(NA, xlim=c(minx,maxx), ylim=c(miny,maxy), main=title)	

    if (!simple) {
        load('/dragonfly/gis/r/bathy.rdata')

        if (type=='raster') {
            image(bathy$lon,bathy$lat,bathy$height, add=T, col=cols(50), useRaster=T)
            plot(elide(NZ$SP, shift=c(xshift,yshift)), col=grey(0.1), border=grey(0.1), add=T)
            plot(NZ$SP, add=T, col=grey(0.7), border=grey(0.68))	
        }

        if (type=='contour') {
            plot(elide(NZ$SP, shift=c(xshift,yshift)), col=grey(0.1), border=grey(0.1), add=T)
            plot(NZ$SP, add=T, col=grey(0.7), border=grey(0.68))	
            contour(bathy$lon, bathy$lat, -bathy$height, levels=c(200,500,1000,2000,5000),
                    col=gray(log(seq(exp(0.5*5),exp(0.85*5),length.out=5))/5), add=TRUE, labcex=0.2, lwd=0.5, drawlabels=F)
        }
    } else
        plot(NZ$SP, col=grey(0.7), border=grey(0.68))	
}


make.raster.grid <- function(from.df, reso=NULL, xcol=NULL, ycol=NULL, projs="+init=epsg:4326") {
    ## from.df: data frame or SpatialPointsDataFrame object
    ## reso: resolution of grid, in coordinates unit
    ## ycol, xcol: name of the columns for latitude and longitude (or northing and easting)
    ## projs: projection string (WGS84: "+init=epsg:4326", NZTM: "+init=epsg:2193")
    library(raster)
    library(rgdal)
    if (!(all(class(from.df) == 'SpatialPointsDataFrame'))) {
        ## Transform from.df to spatial object
        if (is.null(ycol) | is.null(xcol))
            stop('Need a SpatialPointsDataFrame, or ycol & xcol specified')
        from.df <- from.df[!is.na(from.df[[ycol]]) & !is.na(from.df[[xcol]]), ]
        coordinates(from.df) <- eval(parse(text=sprintf('~%s+%s', xcol, ycol)))
        proj4string(from.df) <- CRS(projs)
    } else projs <- proj4string(from.df)
    ## Make raster base grid
    bb <- bbox(from.df)
    span <- diff(t(bb))
    if (is.null(reso))  reso <- max(span)/100 ## resolution of grid, in units of coordinates
    dims <- span/reso
    cellsize <- c(reso, reso)
    grid <- SpatialGrid(GridTopology(bb[,1], cellsize, ceiling(dims)))
    proj4string(grid) <- CRS(projs)
    return(grid)
}


## Convert data frame representing points to SpatialPoints
make.spdf <- function(df, xcol=NULL, ycol=NULL, projs="+init=epsg:4326", verbose=F) {
    c <- is.na(df[[ycol]]) & is.na(df[[xcol]])
    if (sum(c) & verbose==T)
        cat('Removed', sum(c), 'rows', 'out of', nrow(df), 'because of NA coordinates\n')
    df <- df[!c, ]
    coordinates(df) <- eval(parse(text=sprintf('~%s+%s', xcol, ycol)))
    proj4string(df) <- CRS(projs)
    return(df)
}

myRasterize <- function(df, valcol, fun, reso=NULL, xcol=NULL, ycol=NULL, projs="+init=epsg:4326", grid=NULL) {
    ## df: data frame or SpatialPointsDataFrame object to be rasterised
    ## valcol: column name of the value of interest (used in fun)
    ## fun: function returning the aggregate quantity of valcol (needs '...' to accept na.rm)
    ##      e.g. function(x,...) mean(x, ...)
    ## reso: resolution of grid, in coordinates unit
    ## ycol, xcol: name of the columns for latitude and longitude (or northing and easting)
    ## projs: projection string (WGS84: "+init=epsg:4326", NZTM: "+init=epsg:2193")
    library(raster)
    library(rgdal)
    if (!(all(class(df) == 'SpatialPointsDataFrame'))) { ## Transform df to spatial object
        if (is.null(ycol) | is.null(xcol))
            stop('Need a SpatialPointsDataFrame, or ycol & xcol specified')
        df <- df[!is.na(df[[ycol]]) & !is.na(df[[xcol]]), ]
        coordinates(df) <- eval(parse(text=sprintf('~%s+%s', xcol, ycol)))
        proj4string(df) <- CRS(projs)
    } else projs <- proj4string(df)
    if (is.null(reso) & !is.null(grid)) reso <- grid@grid@cellsize[1]
    if (is.null(grid)) grid <- make.raster.grid(df, reso, xcol, ycol, projs)
    rast <- rasterize(df, raster(grid), valcol, fun=fun)
    return(rast)
}

clicks2extent <- function(verbose=T) {
    xys <- locator(2)
    xys <- matrix(c(range(xys$x), range(xys$y)), nrow=2, byrow=T)
    ext <- extent(xys)
    if (verbose) dput(ext)
    return(ext)
}


myZoom <- function(plotfun, verbose=T) {
    ## x11()
    plotfun()
    ext <- clicks2extent(verbose)
    ## dev.off()
    plotfun(ext)
}

toqgis <- function(x, plot.nz=T, plot.eez=F) {
    library(rgdal)
    p <- proj4string(x)
    if (class(x)=='SpatialPolygons') {
        e <- SpatialPolygonsDataFrame(x, data.frame(v=rep(NA, length(x))))
        writeOGR(e, '/tmp', 'x_temp', 'ESRI Shapefile', overwrite_layer=T)
    }
    if (class(x)=='SpatialPolygonsDataFrame') {
        e <- x
        writeOGR(e, '/tmp', 'x_temp', 'ESRI Shapefile', overwrite_layer=T)
    }
    if (class(x)=='SpatialPoints') {
        e <- SpatialPointsDataFrame(x, data.frame(v=rep(NA, length(x))))
        writeOGR(e, '/tmp', 'x_temp', 'ESRI Shapefile', overwrite_layer=T)
    }
    if (class(x)=='SpatialPointsDataFrame') {
        e <- x
        writeOGR(e, '/tmp', 'x_temp', 'ESRI Shapefile', overwrite_layer=T)
    }
    if (class(x)=='SpatialLines') {
        e <- SpatialLinesDataFrame(x, data.frame(v=rep(NA, length(x))))
        writeOGR(e, '/tmp', 'x_temp', 'ESRI Shapefile', overwrite_layer=T)
    }
    if (class(x)=='SpatialLinesDataFrame') {
        e <- x
        writeOGR(e, '/tmp', 'x_temp', 'ESRI Shapefile', overwrite_layer=T)
    }
    if (class(x)=='RasterLayer') {
        e <- as(x, 'SpatialGridDataFrame')
        writeGDAL(e, '/tmp/x_temp.img', 'HFA')
    }
    if (plot.nz | plot.eez) {
        load('~/share/dragonfly/gis/nz_isl_eez.rdata')
        if (plot.nz) {
            nz <- spTransform(nz, CRS(p))
            writeOGR(nz, '/tmp', 'nz_temp', 'ESRI Shapefile', overwrite_layer=T)
        }
        if (plot.eez) {
            eez <- spTransform(eez, CRS(p))
            writeOGR(eez, '/tmp', 'eez_temp', 'ESRI Shapefile', overwrite_layer=T)
        }
    }
    ext <- bbox(x)
    cmd <- sprintf("qgis --nologo --noplugins --extent %f,%f,%f,%f %s /tmp/x_temp.%s %s",
                   ext[1,1], ext[2,1], ext[1,2], ext[2,2], 
                   ifelse(plot.nz,'/tmp/nz_temp.shp',''),
                   ifelse(grepl('^Spatial',class(x)) & !grepl('Gridl',class(x)), 'shp', 'img'),
                   ifelse(plot.eez,'/tmp/eez_temp.shp',''))
    system(cmd, wait=F)
}


sprect <- function(xmin, xmax, ymin, ymax, p4s="", ids=NULL, aslist=F) {
    n <- unique(c(length(xmin), length(xmax), length(ymin), length(ymax)))
    if (length(n) != 1)
        stop('xmin, xmax, ymin, ymax do not have the same length')
    if (is.null(ids))  ids <- as.character(1:n)
    
    rects <- sapply(1:n, function(i) {
                        SpatialPolygons(list(Polygons(list(
                            Polygon(cbind(x=c(xmin[i], xmin[i], xmax[i], xmax[i], xmin[i]),
                                          y=c(ymin[i], ymax[i], ymax[i], ymin[i], ymin[i])))), ids[i])),
                                        proj4string=CRS(p4s))
                    }, simplify=F)

    r <- rects[[1]]
    if (!aslist) {
        if (n > 1) {
            for (i in 2:n) {
                library(maptools)
                r <- spRbind(r, rects[[i]])
            }
        }
        return(r)
    } else return(rects)
}


## Static base map using ggmap, making sure that all x and y are within the map
get_basemap <- function(x=NULL, y=NULL, xcent=NULL, ycent=NULL, zoom=NULL,
                        maptype='roadmap', filename='basemap',
                        style=c(feature='all', element='labels', visibility='off'),
                        color = 'bw') {
    library(ggmap)
    if (is.null(xcent) & is.null(ycent)) {
        if (is.null(x) | is.null(y)) stop('Need x and y to calculate missing centre x and y')
        xcent <- mean(range(x), na.rm=T)
        ycent <- mean(range(y), na.rm=T)
    }
    if (is.null(zoom)) {
        if (is.null(x) | is.null(y)) stop('Need x and y to calculate zoom')
        zoom <- calc_zoom(extendrange(x), extendrange(y))
    }
    gm <- get_googlemap(c(xcent, ycent),
                        zoom = zoom, maptype = 'roadmap', filename = 'roadmap',
                        style=c(feature='all', element='labels', visibility='off'),
                        color = 'bw')
    bb <- unlist(attr(gm, 'bb'))
    while (any(x < bb[['ll.lon']] | x > bb[['ur.lon']] | y < bb[['ll.lat']] | y > bb[['ur.lat']])) {
        zoom <- zoom - 1
        gm <- get_googlemap(c(xcent, ycent),
                            zoom = zoom, maptype = maptype, filename = filename,
                            style = style, color = color)
        bb <- unlist(attr(gm, 'bb'))
    }
    cat(sprintf('Basemap with zoom = %i, X center = %f, Y center = %f\n', zoom, xcent, ycent))
    return(gm)
}


###############################################################################
###  CALCULATIONS
###############################################################################

## coefficient of variation
cv <- function(x) sd(x)/mean(x)

## round a number (or a vector of numbers), higher or lower depending on decimal part    
rnd_round <- function(K) {
    l <- floor(K)
    r <- K-l
    o <- rep(NA, length(K))
    rn <- runif(length(K))
    o[rn>r] <- l[rn>r]
    o[rn<=r] <- l[rn<=r]+1
    o <- ifelse(rn>r, l, l+1)
    return(o)
}

## round a number to nearest specified value
nearest <- function(x, val=0.2) {
    coeff <- 1/val    
    return(round(x*coeff)/coeff)
}

logit <- function(x) {
    return(log(x/(1-x)))
}

invlogit <- function(x) {
    return(1/(1+exp(-x)))
}


## My own function to estimate x to get a given y
## Limits are not strict, just to give an idea of the slope and where to start (lims[1])
optimise_y <- function(f, target, lims, incr=0.01, tol=0.00001, ...) {
    ## target=0.05; lims=c(0,3); incr=0.01; tol=0.00001
    xys <- NULL
    xi <- lims[1]
    ci <- lims
    yi <- f(xi, ...)
    slope <- sign(f(ci[2], ...) - f(ci[1], ...))
    dir = ifelse(sign(yi-target)==slope, -1, 1)
    samedir = 0
    steps = 0
    stillhunt = T
    first <- T
    ## 1- Hunt: start anywhere then move with increasing step until limit is passed
    while (stillhunt) {
        steps = steps + 1
        if (!first) {
            yi <-  f(xi, ...)
        } else first <- F
        xys = rbind(xys, c(xi, yi, dir, incr))
        if (dir < 0 & yi < target) {
            ci[1] = xi
            stillhunt = F
        }
        if (dir > 0 & yi > target) {
            ci[2] = xi
            stillhunt = F
        }
        if (dir > 0 & yi < target) {
            ci[1] = xi
        }
        if (dir < 0 & yi > target) {
            ci[2] = xi
        }
        if (stillhunt) {
            incr = incr * (sqrt(5)+1)/2
            xi = xi + dir * incr
        } else {
            incr = incr * (sqrt(5)-1)/2
            dir = -1 * dir
            xi = xi + dir * incr
        }
    }

    ## 2- Bisection: halve the step
    while (abs(yi - target) > tol  &  incr > 1e-6  &  incr != tol & steps<666) {
        steps = steps + 1
        yi <- f(xi, ...)
        xys <- rbind(xys, c(xi, yi, dir, incr))
        incr <- incr * (sqrt(5)-1)/2
        dir = -sign(yi-target)
        ci[ifelse(dir > 0, 1, 2)] <- xi
        xi = xi + dir * incr
    }
    
    xys = cbind(xys, xys[,2]-target)
    colnames(xys) <- c('x','y','dir','incr','dist')

    if (xys[steps,'dist'] > tol)
        warning('Final accuracy greater than specified tolerance')
    
    return(list(x=xys[steps,'x'], acc=xys[steps,'dist'], step=xys))
}

around <- function(x, idx, plusminus=5) {
    return(x[(idx-plusminus):(idx+plusminus),])
}


###############################################################################
###  DIAGNOSE
###############################################################################

## table() as data frame
tabl <- function(x, sort=T, include.na=T) {
    if (length(x)) {
        t <- as.data.frame(table(x, useNA=ifelse(include.na,'always','ifany')), stringsAsFactors=F)
        names(t) <- c('value','freq')
        if (sort)
            t <- t[order(t$freq, decreasing=T),]
        rownames(t) <- NULL
        return(t)
    } else
        stop('x is empty')
}

## customised table()
tab <- function(x) {
    return(sort(table(x, useNA='always'), decreasing=T))
}


## Function from raster package
extension <- function (filename, value = NULL, maxchar = 10) {
    if (!is.null(value)) {
        extension(filename) <- value
        return(filename)
    }
    lfn <- nchar(filename)
    ext <- list()
    for (f in 1:length(filename)) {
        extstart <- -1
        for (i in lfn[f]:2) {
            if (substr(filename[f], i, i) == ".") {
                extstart <- i
                break
            }
        }
        if (extstart > 0) {
            ext[f] <- substr(filename[f], extstart, lfn[f])
        }
        else {
            ext[f] <- ""
        }
    }
    ext <- unlist(ext)
    ext[nchar(ext) > maxchar] <- ""
    return(ext)
}

makeuniquefilename <- function(x) {
    if (length(x)>1) stop('Function takes only one file name')
    ext <- extension(x)
    basefile <- sub(ext, '', x)
    f <- x
    ii <- 0
    while(file.exists(f)) {
        f <- sprintf('%s%i%s', basefile, ii, ext)
        ii <- ii+1
    }
    return(f)
}

## Open data frame in oocalc
localc <- function(df, row.names=F, newl.at=NA, na.col.rm=T, openwith='localc', ...) {
    library(data.table)
    df <- as.data.table(df)
    f <- tempfile(fileext = '.csv')
    if (!is.na(newl.at)) { ## insert return line when field is too long
        nch <- lapply(df, function(x) max(na.omit(nchar(as.character(x)))))
        toolongcols <- names(nch[nch>newl.at])
        for (c in toolongcols) {
            df[[c]] <- as.character(df[[c]])
            toolongvals = nchar(df[[c]]) > newl.at
            df[[c]][toolongvals] <- sapply(df[[c]][toolongvals], function(x) {
                                            s = strsplit(x,'')[[1]]
                                            s1 = grep('[[:blank:]]',s)
                                            s0 = c(seq(1, nchar(x), newl.at), nchar(x))
                                            i = findInterval(s1, s0, rightmost.closed=T)
                                            s2 = s1[ (i[-length(i)]-i[-1]) == -1]
                                            s[s2] <- '\n'
                                            return(paste(s, collapse=''))
                                        })
        }
    }
    lstcols <- sapply(df, is.list)
    if (any(lstcols))
        df[, names(lstcols[lstcols==T]) := NULL]
    if (na.col.rm) {
        df <- df[, sapply(df, function(x) all(is.na(x))) == F, with=F]
    }
    fwrite(df, f, quote=T, dateTimeAs = 'write.csv', row.names=row.names, ...)
    ## res <- system(sprintf('localc %s', f), wait=F)
    res <- system(sprintf('%s %s', openwith, f), wait=F)
    ## res <- system(sprintf('gnumeric %s', f), wait=F)
    ## res <- system(sprintf('gnumeric %s', f), wait=F)
}

focalc <- function(df, row.names=T, newl.at=100, ...) {
    library(data.table)
    df <- as.data.table(df)
    f <- tempfile(fileext = '.csv')
    if (!is.na(newl.at)) { ## insert return line when field is too long
        nch <- lapply(df, function(x) max(na.omit(nchar(as.character(x)))))
        toolongcols <- names(nch[nch>newl.at])
        for (c in toolongcols) {
            df[[c]] <- as.character(df[[c]])
            toolongvals = nchar(df[[c]]) > newl.at
            df[[c]][toolongvals] <- sapply(df[[c]][toolongvals], function(x) {
                                            s = strsplit(x,'')[[1]]
                                            s1 = grep('[[:blank:]]',s)
                                            s0 = c(seq(1, nchar(x), newl.at), nchar(x))
                                            i = findInterval(s1, s0, rightmost.closed=T)
                                            s2 = s1[ (i[-length(i)]-i[-1]) == -1]
                                            s[s2] <- '\n'
                                            return(paste(s, collapse=''))
                                        })
        }
    }
    lstcols <- sapply(df, is.list)
    df[, names(lstcols[lstcols==T]) := NULL]
    fwrite(df, f, row.names=row.names, ...)
    res <- system(sprintf('planmaker18free %s', f), wait=F)
}


###############################################################################
###  UTILS
###############################################################################

## quit R without confirmation
Q <- function(){base::q('no')}

## Get size of memory objects
lsmem <- function() {
    z = sapply(ls(envir=.GlobalEnv), function(x) object.size(get(x, envir=.GlobalEnv)))
    z = data.frame(var=names(z), size=z)
    z = z[order(z$size, decreasing=T),]
    rownames(z) <- NULL
    return(z)
}

## test function
test <- function()
    cat('\ntest\n')


## load content of rdata file into new environment and list them with their summary
## rdata = '~/dragonfly/abundance/report/data-summary/data/postgrooming_report.RData'
peepinto <- function(rdata, more=F) {
    e1 <- new.env()
    load(rdata, envir=e1)
    z = sapply(ls(envir=e1), function(x) object.size(get(x, envir=e1)))
    z = data.frame(object=names(z), size=z, stringsAsFactors=F)
    z = z[order(z$size, decreasing=T),]
    z$dim <- NULL
    z$length <- NULL
    z$type <- NULL
    for (i in 1:nrow(z)) {
        o <- get(z$object[i], envir=e1)
        z$dim[i] <- list(dim(o))
        z$length[i] <- length(o)
        z$type[i] <- typeof(o)
    }
    if (more) {
        z$summ <- NULL
        for (i in 1:nrow(z)) {
            o <- get(z$object[i], envir=e1)
            z$summ[i] <- list(summary(o))
        }
    }
    rownames(z) <- z$object
    return(z[,-1])
}


## List variables contained in a .rdata file, with their structure
ls.str.into <- function(rdata) {
    e1 <- new.env()
    load(rdata, envir=e1)
    ls.str(envir=e1)
}

## List variables contained in a .r source file, with their structure
ls.source.into <- function(sourcefile) {
    e1 <- new.env()
    source(sourcefile, local=e1)
    ls.str(envir=e1)
}

## dim that works also on vectors
Dim <- function(dat) {
    dm <- dim(dat)
    if (is.null(dm))
        return(length(dat)) else
    return(dm)
}


## Progress bar. Draw first line and return indices when a symbol should be drawn.
## ex: if (i %in% ids) cat('.')
progressbar <- function(n, length=50) {
    if (length>n) length=n
    cat(sprintf('|%s|\n', paste(rep('-',length-2), collapse='')))
    s <- 1:n
    sp <- s/n * length
    target <- 1:length
    ids <- sapply(target, function(x) which.min(abs(sp-x)))
    return(ids)
}


## Summary of all computer processes
compprocsumm <- function(comps=c('robin','leon','titi','tieke','frank','jeremy'), user='yvan', getres=F) {
    res <- NULL
    cp=comps[1]
    for (cp in comps) {
        cmd <- sprintf('ssh -A %s@%s \"ps -eo \\\"%%c|%%C|%%x|%%z|%%U\\\" --no-heading | column -t\"', user, cp)
        r <- system(cmd, intern=T)
        if (length(r)) {
            l <- strsplit(gsub(' +', '', r), '\\|')
            r <- as.data.frame(do.call('rbind',l), stringsAsFactors=F)
            names(r) <- c('command','%cpu','time','%mem','user')
            r$computer <- cp
            r <- r[,c('computer','command','%cpu','time','%mem','user')]
            res <- rbind(res, r)
        }
    }

    res2 <- res
    for (c in 1:ncol(res))
        res2[,c] <- type.convert(res2[,c], as.is=T)

    res <- res2
    cat('\n\n\n***************  All processes sorted by %mem  (first 10)  ***************\n\n')
    print(head( res[order(res$'%mem', decreasing=T),] , 10))

    cat('\n\n\n***************  All processes sorted by %cpu  (first 10)  ***************\n\n')
    print(head( res[order(res$'%cpu', decreasing=T),] , 10))

    cat('\n\n\n***************  All processes sorted by cpu time  (first 10)  ***************\n\n')
    print(head( res[order(res$'time', decreasing=T),] , 10))

    if (getres) return(res)
} 


## Check for process (R by default) in all dragonfly computers
proc_in_dfly <- function(comm='R', comps=c('robin','leon','titi','tieke','frank','haast','tui','jeremy','taiko'), user='yvan', getres=F) {
    res <- NULL
    cp=comps[1]
    for (cp in comps) {
        cmd <- sprintf('ssh -A %s@%s \"ps -C %s -o \\\"%%c|%%C|%%x|%%z|%%U\\\" --no-heading | column -t\"', user, cp, comm)
        r <- system(cmd, intern=T)
        if (length(r)) {
            l <- strsplit(gsub(' +', '', r), '\\|')
            r <- as.data.frame(do.call('rbind',l), stringsAsFactors=F)
            names(r) <- c('command','%cpu','time','mem(Mb)','user')
            r$computer <- cp
            r <- r[,c('computer','command','%cpu','time','mem(Mb)','user')]
            res <- rbind(res, r)
        }
    }

    res$'mem(Mb)' <- round(as.numeric(res$'mem(Mb)')/1024, 2)
    print(res)
    if (getres) return(res)
} 

## Check log of screen in all dragonfly computers
check_screen_in_dfly <- function(fold, comps=c('robin','leon','titi','tieke','frank','jeremy','taiko','haast'), user='yvan') {
    cp <- comps[1]
    for (cp in comps) {
        cat('*****', cp,'\n')
        cmd <- sprintf('ssh -A %s@%s "cd %s; tail screenlog.0"', user, cp, fold)
        cat(cmd,'\n')
        system(cmd, wait=T)
        cat('\n\n')
    }
}

dfly_cmd <- function(cmd, comps=c('robin','leon','titi','tieke','frank','taiko','tui','kahu','haast'),
                     user='yvan', wait=T) {
    cp=comps[1]
    for (cp in comps) {
        cat('*****', cp, '\n')
        cmd2 <- sprintf('ssh -A %s@%s "%s"', user, cp, cmd)
        cat(cmd2,'\n\n')
        system(cmd2, wait=wait)
        cat('\n\n')
    }
}

runoncluster <- function(f, x, vars2export=NULL, libs2load=NULL,
                         clust=c(rep('robin',5), rep('leon', 5), rep('frank',3), rep('taiko',3))) {
    library(snow)
    cl <- makeSOCKcluster(clust)
    clusterExport(cl, list(c("f",vars2export)))
    if (!is.null(libs2load))
        for (i in length(libs2load))
            clusterEvalQ(cl, eval(parse(text=sprintf('library(%s)', libs2load[i]))))
    res <- clusterApply(cl, x, f)
    stopCluster(cl)
}

## return random factor for testing
rndfactor <- function(n=15, nlev=4)
    return(factor(sample(LETTERS[1:nlev], n, replace=T)))


## return string to be copied in script to setwd to current dir and copy string to clipboard
setwdhere <- function() {
    home <- Sys.getenv('HOME')
    here <- getwd()
    txt <- sprintf('setwd(\'%s\')',sub(home, '~', here))
    cat(sprintf('\n%s\n\n',txt))
                                        #system(sprintf('echo \"%s\" | xclip', txt))
    File = pipe("xclip -i", "w")
    cat(txt, file=File)
    close(File)
}


makeseq <- function(x) {
    if (class(x) != 'character') return(NA)
    x <- unlist(strsplit(x, ','))
    areranges <- grepl('-',x)
    return(sort(unique(c(as.numeric( x[!areranges]),
                         unlist(sapply(x[areranges],
                                       function(x) {
                                           x <- as.numeric(unlist(strsplit(x, '-')))
                                           return(seq(x[1],x[2]))
                                       }))))))
}


collapseseq <- function(x, with.attr=F) {
    if (!(class(x) %in% c('numeric','integer')))  return(NA)
    if (length(x)>1) {
        x <- sort(unique(x))            # just in case
        n <- length(x)
        d <- x[2:n]-x[1:(n-1)]
        d2 <- c(2,d)
        s <- cumsum(d2-1)
        r <- split(x, s)
        rs <- sapply(r, function(x) {
                         nx <- length(x)
                         if (nx > 1)
                             return(sprintf('%s-%s', x[1], x[nx])) else
                         return(x)
                     }, simplify=T)
        ns <- sapply(r, length)
        txt <- paste(rs, collapse=',')
        if (with.attr) {
            attr(txt,'elements') <- rs
            attr(txt,'ns') <- ns
        }
        return(txt)
    } else {
        txt <- as.character(x)
        if (with.attr) {
            attr(txt,'elements') <- as.character(x)
            attr(txt,'ns') <- 1
        }
        return(txt)
    }
}

## fold <- getwd()
get_in_out_from_scripts <- function(fold='.', returnlist=F, recursive=F) {
    fls <- dir(fold, pattern='*.r$', full.names=T, recursive=recursive)

    ## filter only text files
    istxt <- NULL
    for (i in 1:length(fls)) {
        a <- system(sprintf('file %s', fls[i]), intern=T)
        istxt <- c(istxt, ifelse(length(grep('text', a)), T, F))
    }
    fls <- fls[istxt]
    
    f=fls[1]
    outlist <- NULL
    for (f in fls) {
        if (!returnlist) {
            cat('\n')
            cat(f,':\n')
            cat(paste(rep('-', nchar(f)), collapse=''),'\n')
        }
        sc <- readLines(f)
        sc <- sc[!grepl('^ *#', sc) & sc!=''] #
          ## INPUTS
          ins <- NULL
        rcsv <- grep('read.csv', sc, val=T)
        if (length(rcsv)) {
            withsq <- grepl("\'.*\'", rcsv)
            ins <- c(ins, gsub(".*\'(.*)\'.*", '\\1', rcsv[withsq]))
            withdq <- grepl("\".*\"", rcsv)
            ins <- c(ins, gsub(".*\"(.*)\".*", '\\1', rcsv[withdq]))
        }
        lds <- grep('load\\(', sc, value=T)
        if (length(lds)) {
            withsq <- grepl("\'.*\'", lds)
            ins <- c(ins, gsub(".*\'(.*)\'.*", '\\1', lds[withsq]))
            withdq <- grepl("\".*\"", lds)
            ins <- c(ins, gsub(".*\"(.*)\".*", '\\1', lds[withdq]))
        }
        ## OUTPUTS
        outs <- NULL
        rcsv <- grep('write.csv', sc, val=T)
        if (length(rcsv)) {
            withsq <- grepl("\'.*\'", rcsv)
            outs <- c(outs, gsub(".*\'(.*)\'.*", '\\1', rcsv[withsq]))
            withdq <- grepl("\".*\"", rcsv)
            outs <- c(outs, gsub(".*\"(.*)\".*", '\\1', rcsv[withdq]))
        }
        lds <- grep('save\\(', sc, value=T)
        if (length(lds)) {
            withsq <- grepl("\'.*\'", lds)
            outs <- c(outs, gsub(".*\'(.*)\'.*", '\\1', lds[withsq]))
            withdq <- grepl("\".*\"", lds)
            outs <- c(outs, gsub(".*\"(.*)\".*", '\\1', lds[withdq]))
        }
        ## cat('\n')
        ## if (length(c(ins, outs)))
        outlist[[f]] <- list(ins=ins, outs=outs)
        if (!returnlist & length(c(ins, outs))) {
            cat('INPUTS:\n')
            print(ins)
            cat('OUTPUTS:\n')
            print(outs)
        } 
    }
    if (returnlist)
        return(outlist)
}


num2word <- function(x){ 
    x <- as.numeric(x)
    helper <- function(x){ 
        digits <- rev(strsplit(as.character(x), "")[[1]]) 
        nDigits <- length(digits) 
        if (nDigits == 1) as.vector(ones[digits]) 
        else if (nDigits == 2) 
            if (x <= 19) as.vector(teens[digits[1]]) 
            else trim(paste(tens[digits[2]], Recall(as.numeric(digits[1])))) 
        else if (nDigits == 3) trim(paste(ones[digits[3]], "hundred", 
                                          Recall(makeNumber(digits[2:1])))) 
        else { 
            nSuffix <- ((nDigits + 2) %/% 3) - 1 
            if (nSuffix > length(suffixes)) stop(paste(x, "is too large!")) 
            trim(paste(Recall(makeNumber(digits[ 
                                                nDigits:(3*nSuffix + 1)])), 
                       suffixes[nSuffix], 
                       Recall(makeNumber(digits[(3*nSuffix):1])))) 
        } 
    } 
    trim <- function(text){ 
        gsub("^\ ", "", gsub("\ *$", "", text)) 
    } 
    makeNumber <- function(...) as.numeric(paste(..., collapse="")) 
    opts <- options(scipen=100) 
    on.exit(options(opts)) 
    ones <- c("", "one", "two", "three", "four", "five", "six", "seven", 
              "eight", "nine") 
    names(ones) <- 0:9 
    teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", 
               "sixteen", " seventeen", "eighteen", "nineteen") 
    names(teens) <- 0:9 
    tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", 
              "eighty", 
              "ninety") 
    names(tens) <- 2:9 
    x <- round(x) 
    suffixes <- c("thousand", "million", "billion", "trillion") 
    if (length(x) > 1) return(sapply(x, helper)) 
    helper(x) 
} 



graph_r_scripts <- function(fold='.', recursive=T) {
    fold <- sub('^\\.', getwd(), fold)
    fold <- path.expand(fold)

    opt <- options("useFancyQuotes")
    options(useFancyQuotes = FALSE)

    inout <- get_in_out_from_scripts(fold, returnlist=T, recursive=recursive)

    scripts <- names(inout)
    ins <- sapply(inout, function(x) x$ins)
    outs <- sapply(inout, function(x) x$outs)
    
    ## Create dot file
    nodes <- dQuote(na.omit(c(unlist(ins), unlist(outs))))
    rnodes <- dQuote(scripts)

    ##     gf <- 'digraph G {
    ## rankdir=BT; nodesep=1; ranksep=1; ratio="compress"; size="1000,100"; spline=true;
    ## '
    gf <- 'digraph G {
nodesep=1; ranksep=1; ratio="compress"; size="100,100"; spline=true; 
'
    gf <- c(gf,
            sprintf('node [fontsize=16, height=.3, style=filled, fillcolor=grey, shape=rectangle] %s;',
                    paste(nodes,collapse=' ')))
    gf <- c(gf,
            sprintf('node [fontsize=16, height=.3, style=filled, fillcolor=yellow, shape=rectangle] %s;',
                    paste(rnodes,collapse=' ')))

    i=2
    for (i in 1:length(inout)) {
        op <- inout[[i]]
        if (!is.null(op$ins))
            gf <- c(gf, sprintf('{%s} -> %s;', paste(dQuote(op$ins),collapse='; '),
                                dQuote(names(inout[i])),collapse='\\n'))
        if (!is.null(op$outs))
            gf <- c(gf, sprintf('%s -> {%s};', dQuote(names(inout[i])),
                                paste(dQuote(op$outs),collapse='; ')))
    }
    gf <- c(gf, '}')

    dotfile <- sprintf('%s/graph-r-scripts.dot', fold)
    pdffile <- sub('\\.dot', '.pdf', dotfile)
    cat(gf, sep='\n', file=dotfile)
    options(useFancyQuotes = opt)
    uffile <- sub('\\.dot', '_uf.dot', dotfile)
    system(sprintf('unflatten %s -o %s', dotfile, uffile))
    system(sprintf('neato -Tpdf %s -o %s', uffile, pdffile))
    system(sprintf('xdot %s', uffile), wait=F)
    ## system(sprintf('xdg-open %s', pdffile), wait=F)
}


get_r_libraries <- function(fold = '.') {
    library(data.table)

    cmd1 <- sprintf('grep -r --include="*.r" --include="*.R" "library(" %s', fold)
    res1 <- suppressWarnings(system(cmd1, intern=T))
    cmd2 <- sprintf('grep -r --include="*.r" --include="*.R" "require(" %s', fold)
    res2 <- suppressWarnings(system(cmd2, intern=T))
    res <- c(res1, res2)
    res <- as.data.table(do.call(rbind, strsplit(res, ':')))
    setnames(res, c('file', 'library'))
    res <- res[!grepl('^[[:blank:]]*#', library)]
    res[, library := trimws(library)]
    res[, library := sub('.*library\\((.*)\\).*', '\\1', library)]
    res[, library := sub('.*require\\((.*)\\).*', '\\1', library)]
    summ <- res[, .N, library]
    summ[, lib := tolower(library)]
    setorder(summ, lib)
    print(summ[, -'lib', with=F])
    return(summ[, library])
}

## "%nin%" <- function(x,y) !(x %in% y)

## Check labels in LaTeX report that are not cited in text
## reportfile='~/dragonfly/sra-foundations/report/notes/report.tex'
check_cited_labels <- function(reportfile='report.tex', ignore=c('sec','subsec','eq','app','page'),
                               debug=F, save=T) {
    if (!grepl('^/|^~', reportfile))  reportfile <- sprintf('%s/%s',getwd(),reportfile)
    get_inputs <- function(file) {
        file <- sprintf('%s.tex', gsub('^[[:blank:]]*|[[:blank:]]*$', '', sub('\\.tex$', '', file)))
        txt <- readLines(file, warn=F)
        txt <- txt[!grepl('^[[:blank:]]*%', txt)]
        inputs <- grep('\\\\input\\{', txt, value=T)
        inputs <- gsub('.*\\\\input\\{([^}]+)\\}.*', '\\1', inputs)
        subinputs <- sapply(inputs, get_inputs)
        return(as.vector(c(names(subinputs), unlist(subinputs))))
    }
    inputs <- c(reportfile, unique(get_inputs(reportfile)))
    alltext <- NULL
    alllabels <- NULL
    ## f=inputs[1]
    cat('\nGetting list of labels...\n')
    for (f in inputs) {
        if (debug) cat(f, '\n')
        file <- f
        if (!grepl('^/|^~', file))  file <- sprintf('%s/%s',getwd(), file)
        file <- sprintf('%s.tex', sub('\\.tex$', '', file))
        txt <- readLines(file, warn=F)
        comments <- grepl('^ *%', txt) | txt==''
        txt <- txt[!comments]
        alltext[[f]] <- txt
        labels0 <- grep('\\\\label\\{', txt, val=T)
        labels <- gsub('.*\\\\label\\{([^}]+)\\}.*', '\\1', labels0)
        c <- sapply(strsplit(labels,':'), function(x) x[1]) %in% ignore
        labels <- labels[!c]
        alllabels[[f]] <- labels
    }
    alllabs <- do.call('rbind', sapply(names(alllabels), function(x) {
                                           if (length(alllabels[[x]]))
                                               data.frame(file=x, label=alllabels[[x]], stringsAsFactors=F)
                                       }, simplify=F))
    rownames(alllabs) <- NULL
    alltext <- unlist(alltext)
    cat('\nChecking use of labels...\n')
    library(parallel)
    ## l <- alllabels[2]
    uncited <- mclapply(1:nrow(alllabs), function(i) {
                            l <- alllabs$label[i]
                            if (debug) cat(l, '\n')
                            cited <- grep(sprintf('ref\\{ *%s *\\}', l), alltext, value=T)
                            if (!length(cited)) {
                                return(data.frame(file=alllabs$file[i], label=l))
                            }
                        }, mc.cores=6)
    uncited <- do.call('rbind', uncited)
    uncited <- unique(uncited)
    if (save) {
        cat('\nSaving results to uncited_labels.csv...\n')
        write.csv(uncited, 'uncited_labels.csv')
    }
    cat('\n\n----- Non-cited labels:\n\n')
    print(uncited)
}


##makefile <- '~/dragonfly/sra-foundations/modelling/bh-dd-k50/makefile'
## makefile <- '~/dragonfly/sra-foundations/report/notes/makefile'
graph_makefile <- function(makefile='makefile', rankdir='BT', nodesep=0.1, ranksep=0.2, ratio=0.8, margin=1,
                           ignore.clusters=F, ignore.utils=F, expand.vars=F, filter='dot') {

    opt <- options("useFancyQuotes")
    options(useFancyQuotes = FALSE)

    ##  Create dependency graph from makefile
    ##  Assumptions:
    ##  * no tabs in dependencies or actions names
    
    cola <- '"#FFC6AF"'                 # colour of actions
    colp <- '"#FFF8AF"'                 # colour of programs
    cold <- '"#97DDBF"'                 # colour of data
    colg <- '"#B89DDC"'                 # colour of graphical output
    colf <- '"#C2C2C2"'                 # colour of flags
    colu <- '"#E2E2E2"'                 # colour of unknown types
    colc <- 'grey95'                    # colour of clusters fills

    progtypes <- '\\.r\"|\\.py\"|\\.bug\"|\\.cmd\"|\\.sh\"'
    datatypes <- '\\.rdata\"|\\.csv\"|\\.Rdata\"'
    graphtypes <- '\\.pdf\"|\\.png\"'
    gettype <- function(z) {
        pp <- grep(progtypes, z, value=T)
        dd <- grep(datatypes, z, value=T)
        gg <- grep(graphtypes, z, value=T)
        ff <- z[!grepl('\\.', z) & !(z %in% c(pp, dd, gg))]
        uu <- z[!(z %in% c(pp, dd, ff, gg))]
        return(list(u = paste(uu, collapse=' '),
                    p = paste(pp, collapse=' '),
                    g = paste(gg, collapse=' '),
                    f = paste(ff, collapse=' '),
                    d = paste(dd, collapse=' ')))
    }
    
    mdir <- dirname(makefile)
    mk <- readLines(makefile, warn=F)

    if (any(grepl('^ifndef', mk))) {
        s <- grep('^ifndef', mk)
        e <- grep('^endif', mk)
        e <- e[e>s][1]
        mk <- mk[-(s:e)]
    }
    
    mk <- gsub('^ *','', mk)
    mk <- mk[grepl('^\t', mk) | !grepl('=', mk) | grepl('=[\']', mk)] # remove assignments of environmental variables
    mk <- mk[!grepl('^export ', mk)]
    ## Remove if statements
    ifstatements <- grep('\\bifeq\\b', mk)
    endifs <- grep('\\bendif\\b', mk)
    for (ifloc in ifstatements) {
        mk <- mk[-c(ifloc:min(endifs[endifs >= ifloc]))]
    }
    
    mk <- mk[!(grepl('^include', mk))]
    mk <- mk[!(grepl('\\.PHONY', mk))]

    ## Get clusters
    parts <- mk[(!grepl('#+.*:', mk) & grepl(':', mk) & !grepl('^\t', mk)) | grepl('<!.*!>', mk)] #
    ispart <- grepl('<!.*!>', parts)
    if (any(ispart) & !ignore.clusters) {
        partlabs <- ifelse(grepl('<!.*!>', parts), trim(gsub('.*<!(.*)!>.*','\\1',parts)), NA)
        ts <- cumsum(is.na(partlabs))
        ps <- list()
        wh <- which(!is.na(partlabs))
        for (i in 1:length(wh))
            ps[[partlabs[wh[i]]]] <- ts[(wh[i]+1):ifelse(i!=length(wh), wh[i+1]-1, length(ts))]
        ps[which(names(ps)=='')] <- NULL
        nps <- ts[is.na(partlabs) & !(ts %in% unlist(ps))]
        withclusters <- T
    } else withclusters <- F
    
    ## Remove special characters
    mk <- mk[!grepl('^#', mk)]          # remove comments
    mk <- sub('\t$', '', mk)
    mk <- sub('([^ ])\\\\', '\\1 \\\\', mk)
    mk <- mk[mk!='']
    c <- grepl(':', mk) & !grepl('^\t', mk) & !grepl('#.*:', mk) #
    mk[c] <- gsub('\t', ' ', mk[c])

    mk2 <- paste(mk, collapse='&&&&')

    ## remove consecutive blank lines
    mk3 <- gsub('&&&&&&&&*','&&&&',mk2)
    ## get rid of \\ to identify continuations of lines
    mk4 <- gsub('\\\\&&&&*\t*','',mk3)
    ## identify action
    mk5 <- gsub('&&&&\t', '\t', mk4)
    ## replace :: used sometimes in R with *
    mk5 <- gsub('::+', '**', mk5)
    ## split operations
    m <- strsplit(mk5,'&&&&')[[1]]
    ## identify target
    withaction <- grepl('\t', m)
    
    notarget <- !grepl(':',m)
    if (any(notarget)) {
        print(m[notarget])
        stop('Some targets missing')
    }

    sm <- strsplit(m, ':')

    all <- sapply(sm, function(x) {
        targs <- x[1]
        other <- sapply(x[-1], function(y) strsplit(y, '\t')[[1]], USE.NAMES=F, simplify=F)[[1]]
        if (!grepl('^ *$', other[1])) {                           # with deps
            deps <- gsub('  +', ' ', trim(other[1]))
            deps <- strsplit(deps, ' ')[[1]]
            acts <- other[-1]
            if (!length(other[-1]))
                acts <- NA
        } else  {
            deps <- NA
            acts <- other[-1]
        }
        return(list(targs=trim(targs), deps=trim(deps), acts=trim(acts)))
    }, simplify=F)

    all <- rapply(all, function(x) {
        x <- gsub('\'|\"', '', x)
        z <- sapply(x, function(k) {
            if (!is.na(k))
                paste(sapply(strsplit(k, '\\\\n'), function(y) {
                    if (!all(is.na(y)))
                        return(paste(strwrap(y, width=100, exdent=4), collapse='\\l')) else return(y)
                }), collapse='\\\\n') else return(k)
        }, USE.NAMES = F)
        ## if (!all(is.na(z))) return(paste(z, collapse = '\\\\n')) else return(z)
        return(z)
    }, how='replace')

    ## all <- rapply(all, function(x) gsub('\'|\"', '', x), '\\\\n', how='replace')
    
    if (withclusters)
        clusters <- sapply(ps, function(x) sapply(all[x], function(y) y$targs), simplify=F)

    ## Remove utils
    if (ignore.utils == T) {
        utilclust <- tail(clusters[grep('^util', names(clusters), ignore.case=T)], 1)
        clusters <- clusters[-which(names(clusters) == names(utilclust))]
        ps <- ps[-which(names(ps) == names(utilclust))]
        all <- all[-which(sapply(all, '[[', 'targs') %in% utilclust[[1]])]
    }
    ##=== Create dot file ===##

    all2 <- all
    ## Turn actions into single nodes
    all2 <- sapply(all2, function(x) {
        y <- x$acts
        y <- gsub('\"', '', y)
        y <- unlist(strsplit(y, ' *&& *'))
        if (!any(is.na(y)))
            y <- paste(y, collapse='\\n')
        x$acts <- y
        return(x)
    }, simplify=F)
    all2 <- rapply(all2, function(x) return(ifelse(is.na(x), NA, dQuote(x))), how='replace')
    
    gf <- sprintf('digraph G {
rankdir=%s; nodesep=%f; ranksep=%f; ratio=%f; margin=%f;
', rankdir, nodesep, ranksep, ratio, margin)

    ##--- Nodes ---##
    
    ## Clusters
    if (withclusters) {
        for (i in 1:length(ps)) {
            a <- all2[ps[[i]]]
            td <- unique(unlist(sapply(a, function(x) return(na.omit(c(x$targs, x$deps))))))
            ac <- unique(unlist(sapply(a, function(x) return(na.omit(x$acts)))))
            ## nch <- nchar(ac)
            ## ac[nch>80] <- sapply(ac[nch>80], function(x) {
            ##     paste(sapply(strsplit(x, '\\\\n')[[1]], function(y)
            ##         paste(strwrap(y, width=50, exdent=4), collapse='\\n')),
            ##         collapse = '\\n')
            ## })
            els <- gettype(td)
            gf <- c(gf, sprintf('subgraph cluster%1$i {
label=%2$s; style="rounded,filled"; color=gray50; fillcolor=%3$s; fontcolor=red; fontsize=20; nojustify=true;',
i, dQuote(names(ps)[i]), colc))
            gf <- c(gf, sprintf('node [fontsize=16, height=.3, style="rounded,filled", fillcolor=%1$s, shape=rectangle] %2$s;', colu, els$u))
            gf <- c(gf, sprintf('node [fontsize=16, height=.3, style="rounded,filled", fillcolor=%1$s, shape=rectangle] %2$s;', colp, els$p))
            gf <- c(gf, sprintf('node [fontsize=16, height=.3, style="rounded,filled", fillcolor=%1$s, shape=rectangle] %2$s;', cold, els$d))
            gf <- c(gf, sprintf('node [fontsize=16, height=.3, style="rounded,filled", fillcolor=%1$s, shape=rectangle] %2$s;', colg, els$g))
            gf <- c(gf, sprintf('node [fontsize=16, height=.3, style="rounded,filled", fillcolor=%1$s, shape=rectangle] %2$s;', colf, els$f))
            gf <- c(gf, sprintf('node [fontsize=16, height=.3, style="rounded,filled", fillcolor=%1$s, shape=rectangle] %2$s;\n}\n', cola, paste(ac, collapse=' ')))
        }
    }
    
    ## Nodes not inside clusters
    if (withclusters) {
        nodesinclust <- na.omit(unlist(all2[unlist(ps)]))
        a <- all2[nps]
    } else {
        a <- all2
        nodesinclust <- NULL
    }
    
    targsdeps <- unique(unlist(sapply(a, function(x) return(na.omit(c(x$targs, x$deps))))))
    targsdeps <- targsdeps[!(targsdeps %in% nodesinclust)]
    
    acts <- unique(unlist(sapply(a, function(x) return(na.omit(x$acts)))))
    acts <- acts[!(acts %in% nodesinclust)]

    td <- unique(unlist(sapply(a, function(x) return(na.omit(c(x$targs, x$deps))))))
    ac <- unique(unlist(sapply(a, function(x) return(na.omit(x$acts)))))
    els <- gettype(td)
    gf <- c(gf, sprintf('node [fontsize=16, height=.3, style="rounded,filled", fillcolor=%1$s, shape=rectangle] %2$s;', colu, els$u))
    gf <- c(gf, sprintf('node [fontsize=16, height=.3, style="rounded,filled", fillcolor=%1$s, shape=rectangle] %2$s;', colp, els$p))
    gf <- c(gf, sprintf('node [fontsize=16, height=.3, style="rounded,filled", fillcolor=%1$s, shape=rectangle] %2$s;', cold, els$d))
    gf <- c(gf, sprintf('node [fontsize=16, height=.3, style="rounded,filled", fillcolor=%1$s, shape=rectangle] %2$s;', colg, els$g))
    gf <- c(gf, sprintf('node [fontsize=16, height=.3, style="rounded,filled", fillcolor=%1$s, shape=rectangle] %2$s;', colf, els$f))
    gf <- c(gf, sprintf('node [fontsize=16, height=.3, style="rounded,filled", fillcolor=%1$s, shape=rectangle] %2$s;', cola, paste(ac, collapse=' ')))

    
    ##--- Edges ---##

    i=2
    for (i in 1:length(all)) {
        op <- all2[[i]]
        if (!is.na(op$acts)) {
            ## Deps -> Acts
            if (all(!is.na(op$deps))) {
                gf <- c(gf, sprintf('{%s} -> %s;', paste(op$deps,collapse='; '),
                                    paste(op$acts,collapse='\\n')))
            }
            ## Acts -> Targs
            gf <- c(gf, sprintf('%s -> %s;', paste(op$acts,collapse='\\n'), op$targs))
        }  else {
            ## Deps -> Targs
            gf <- c(gf, sprintf('{%s} -> %s;', paste(op$deps,collapse='; '), op$targs))
        }
    }
    gf <- c(gf, '}')

    dotfile <- sprintf('%s/graph-makefile.dot', mdir)
    pdffile <- sub('.dot', '.pdf', dotfile, fixed=T)
    cat(gf, sep='\n', file=dotfile)
    options(useFancyQuotes = opt)
    system(sprintf('%s -Tpdf %s -o %s', filter, dotfile, pdffile))
    ## system(sprintf('xdg-open %s', pdffile), wait=F)
    system(sprintf('xdot %s --filter %s', dotfile, filter), wait=F)

}


debug_make <- function(makefile='makefile') {

    dbg0 <- system(sprintf('make -f %s -dn', makefile), intern=T)
    dbg0 <- dbg0[(min(which(dbg0==''))+2):length(dbg0)]

    ni <- gsub('^[[:blank:]]+|[[:blank:]]+$', '', dbg0)

    ## Remove targets and indentations for more visibility
    dbg <- gsub("`[^']+'", "'X'", ni)
    print(tabl(dbg))

    cat('\n--- Non-existent files:\n')
    g <- gsub(".*`([^']+)'.*", '\\1', grep('File .* does not exist', dbg0, val=T))
    print(g)
    
    cat('\n--- Nothing to be done for:\n')
    g <- gsub(".*`([^']+)'.*", '\\1', grep('Nothing to be done for', dbg0, val=T))
    print(g)

    cat('\n--- Must remake targets:\n')
    g <- gsub(".*`([^']+)'.*", '\\1', grep('Must remake target', dbg0, val=T))
    print(g)

    cat("\n--- Prerequisite 'X' is newer than target 'Y':\n")
    g <- gsub(".*`([^']+)'.*`([^']+)'.*", "'\\1'\tnewer than\t'\\2'",
              grep('Prerequisite .* is newer than target .*', dbg0, val=T))
    cat(g, sep='\n')

}


## Function to display what's around a specific location in a vector or row in a data frame (or matrix)
neighb <- function(x, at, window=5, add.nas=T) {
    if (is.null(dim(x))) { # vector
        if (add.nas) {
            print(x[c((at-window):(at-1),NA,at,NA,(at+1):(at+window))])
        } else {
            print(x[(at-window):(at+window)])
        }
    } else { # data frame or matrix (more than 2 dimensions not considered)
        if (length(dim(x)) > 2)  stop('Dimensions of more than 2 not treated')
        if (add.nas) {
            print(x[c((at-window):(at-1),NA,at,NA,(at+1):(at+window)),])
        } else {
            print(x[(at-window):(at+window), ])
        }
    }
}

killallr <- function(user='yvan', comps=c('jeremy','tieke','frank','taiko','leon','robin'), proc='R') {
    ## Kill all R processes of specified user in all computers (current computer should specified last for obvious reasons
    for (cp in comps) {
        cat('*****', cp,'\n')
        cmd <- sprintf('ssh -A %1$s@%2$s "killall %3$s -u %1$s"', user, cp, proc)
        cat(cmd,'\n')
        system(cmd, wait=T)
        cat('\n\n')
    }
    cat('Done.\n\n')
}


includemk <- function(Mk='vars.mk', warn=T, save.parsed=T) {
    op <- options('useFancyQuotes')
    options('useFancyQuotes'=F)
    startdir <- dirname(Mk)
    ## Recursive function
    getsubincl <- function(fl) {
        basedir <- dirname(fl)
        mk <- readLines(fl)
        while (any(grepl('^include +', mk))) {
            incpos <- grep('^include +', mk)[1]
            subfl <- paste0(basedir, '/', sub('^include +', '', mk[incpos]))
            incmk <- getsubincl(subfl)
            mk <- c(mk[0:(incpos-1)], incmk, mk[(incpos+1):length(mk)])
        }
        return(mk)
    }
    mk <- getsubincl(Mk)
    ## Clean and parse
    mk <- mk[!grepl('^[[:blank:]]*#', mk)]
      mk <- mk[grep('=', mk)]
    mk <- gsub('\t*', '', mk)
    r <- rapply(strsplit(mk, '='), trim, how='replace')
    if (any(duplicated(sapply(r, '[', 1)))) {
        vars <- sapply(r, '[', 1)
        stop('Duplicated entries in makefile include: ', paste(vars[duplicated(vars)], collapse=', '))
    }
    rr <- list()
    for (i in 1:length(r))
        rr[[r[[i]][1]]] <- r[[i]][2]
    if (warn & any(names(rr) %in% ls(envir=.GlobalEnv)))
        warning(sprintf('Variables %s overwritten while parsing makefile',
                        paste(names(rr)[names(rr) %in% ls(envir=.GlobalEnv)],
                              collapse=', ')))
    if (save.parsed)  parsed <- mk
    for (i in 1:length(rr)) {
        c <- trim(strsplit(mk[i], '=')[[1]])
        c <- gsub(' *#.*$', '', c)
          if (length(c) != 2)
              stop(sprintf('Problem parsing makefile. Assignment #%i non-standard',i)) #
        if (grepl('\\$\\(.*\\)', c[2])) {
            vars <- c(sapply(
                regmatches(c[2],gregexpr(sprintf('\\$\\(([^\\)]+)\\)'),c[2])),
                function(x) gsub('\\$\\((.*)\\)', '\\1', x)))
            
            for (j in 1:length(vars)) {
                if (vars[j] %in% ls(envir=.GlobalEnv)) {
                    regmatches(c[2],regexec(sprintf('\\$\\([^\\)]+\\)'),c[2])) <- get(vars[j], envir=.GlobalEnv)
                } else stop(sprintf('Problem parsing makefile. Variable %s not declared?', vars[j]))
            }
        }
        if (save.parsed)  parsed[i] <- paste(c, collapse=' = ')
        val <- type.convert(c[2], as.is=T)
        assign(c[1], val, envir=.GlobalEnv)
    }
    options('useFancyQuotes'=op)
    if (save.parsed)  cat(parsed, file=paste0(Mk,'.parsed'), sep='\n')
}



takeout <- function(what, from)
    return(from[!(from %in% what)])


## Declare arguments of a function as their default to debug a function
getdefaultargs <- function(fun) {
    fls <- formals(fun)
    sy <- sapply(fls, is.symbol)
    sy <- names(sy)[sy & names(sy)!='...']
    if (length(sy))
        warning(sprintf('Variables not set:  %s', paste(sy, collapse=', ')))
    for (i in 1:length(fls)) {
        if (!is.symbol(fls[[i]])) {
            val <- eval(fls[[i]])
            assign(names(fls)[i], val, envir=.GlobalEnv)
        }
    }
}


## Function to convert R markdown file into pdf or html
## Requires: knitr, markdown, pandoc
convertRmd <- function(Rmds=file.path(getwd(), dir('.', pattern='\\.Rmd$|\\.rmd$')),
               to='pdf', open=TRUE, out=sub('Rmd$|rmd$',to,Rmds), use.markdownToHTML=FALSE,
               pandoc.type='', pandoc.extra, clean.md=TRUE, ...) { 
    require(knitr)
    require(markdown)

    to.poss <- c('pdf','html')
    if (!(to %in% to.poss))
        stop(sprintf('Output format not recognised. Possible values are %s',
                     paste(to.poss, collapse=', ')))
    i=1
    for (i in 1:length(Rmds)) {
        Rmd <- Rmds[i]
        bn <- sub('\\.Rmd|\\.rmd', '', Rmd)
        fp <- dirname(bn)
        md <- file.path(fp, knit(Rmd))  # creates md

        res <- sub('md$', to, md)

        if (!(to == 'html' & use.markdownToHTML == T)) {
            cmd <- sprintf('pandoc %s %s %s -o %s %s',
                           ifelse(pandoc.type!='', sprintf('-t %s', pandoc.type), ''),
                           ifelse(to=='pdf', '', '-s'),
                           md, res, pandoc.extra='')
            system(cmd)
        } else
            markdownToHTML(md, res, options=c('use_xhml'))
        
        if (open)                       # interactive() &
            browseURL(res)
        ## system(sprintf('xdg-open %s', res), wait=F)
        if (clean.md)
            file.remove(md)
    }
}


as.mdtable <- function(df, col.names=colnames(df), row.names=rownames(df), signif=4) {
    res <- NULL
    numcols <- apply(df, 2, class) %in% 'numeric'
    df[,numcols] <- apply(df[,numcols], 2, function(x) signif(x, signif))
    rows <- apply(df, 1, function(x) paste(x, collapse=' | '))
    res <- c(res, rows)
    ## add row names
    if (!is.null(row.names))
        res <- sprintf('%s | %s', row.names, res)
    ## add header
    if (!is.null(col.names)) {
        if (!is.null(row.names)) {
            frst <- paste(c(' ', col.names), collapse=' | ')
            scnd <- paste(rep('-', max(nchar(res)) + 3 + max(nchar(row.names))), collapse='')
        } else {
            frst <- paste(col.names, collapse=' | ')
            scnd <- paste(rep('-', max(nchar(res))), collapse='')
        }
        res <- c(frst, scnd, res)
    }
    return(paste(res, collapse='\n'))
}


insert.column <- function (df, pos, stringsAsFactors=F, ...) {
    if (pos <= 1) 
        df <- cbind(..., df, stringsAsFactors=stringsAsFactors)
    else if (pos >= ncol(df)) 
        df <- cbind(df, stringsAsFactors=stringsAsFactors, ...)
    else df <- cbind(df[, 1:(pos - 1), drop = F], ..., df[, pos:ncol(df), 
                                                          drop = F], stringsAsFactors=stringsAsFactors)
    return(df)
}


## Indicates changes in value (used in seqaxis())
ischange <- function(x, first=T) {
    c(first,!x[1:(length(x)-1)]==x[2:length(x)])
}

## Calculate the x locations of grouped items
## The attribute 'midpoints' return the x locations for group names
seqaxis <- function(mainlvls, sublvls, torem=NA, xsep=1, names=c('sub','main','x')) {
    d <- data.frame(sub=rep(sublvls, length(mainlvls)),
                    main=rep(mainlvls, each=length(sublvls)), stringsAsFactors=F)
    s <- cumsum(d$sub == sublvls[1] & d$main != mainlvls[1])
    if (!is.na(torem)) {
        torem <- gsub('_sub_','d$sub',torem)
        torem <- gsub('_main_','d$main',torem)
        keep <- !eval(parse(text=torem)) 
        d <- d[keep,]
        s <- s[keep]
    }
    d$x <- 1:nrow(d) + s*(xsep-1)
    mids <- d$x[ischange(s)]+(tapply(s,s,length)-1)/2
    names(mids) <- mainlvls
    attr(d, 'midpoints') <- mids
    colnames(d) <- names
    return(d)
}

mean_ci <- function(x) return(c(mean=mean(x),
                                lcl=quantile(x, 0.025, names=F),
                                ucl=quantile(x, 0.975, names=F)))
meanci <- function(x) return(c(mean=mean(x),
                       lcl=quantile(x, 0.025, names=F),
                       ucl=quantile(x, 0.975, names=F)))

get_stats <- function(x, na.rm=T) {
    return(c(mean=mean(x, na.rm=na.rm), med=median(x, na.rm=na.rm),
             lcl=quantile(x, 0.025, names=F, na.rm=na.rm),
             ucl=quantile(x, 0.975, names=F, na.rm=na.rm)))
}


NAanalyse <- function(df, plotx=NULL, nx=10, ncol=4, mar=c(3,3,1,1)) {
    library(data.table)
    if (!'data.table' %in% class(df))
        df <- data.table(df)
    sumna <- unlist(df[, lapply(.SD, function(x) sum(is.na(x)))])
    sna <- sort(sumna[sumna>0], decreasing=T)
    cat('\n===', length(sna), 'columns with some NAs (out of', ncol(df), ',',
        round(100*length(sna)/ncol(df), 1), '%)\n')
    cat('\n-- Number of records that are NA:\n')
    print(sna)
    cat('\n-- Percentage of NAs:\n')
    propna <- sort(round(100*sumna/nrow(df), 1), decreasing=T)
    print(propna[propna>0])
    cat('\n===', sum(propna==0),'columns without NAs:\n')
    print(names(propna[propna==0]))
    naom <- na.omit(df)
    cat('\n===', nrow(naom), 'rows without NAs (out of', nrow(df),',',
        round(100*nrow(naom)/nrow(df),1), '%)\n\n')

    if (!is.null(plotx)) {
        par(mfrow=c(ceiling(sum(sumna>0)/ncol), ncol), mar=mar)
        for (v in names(df)) {
            if (any(is.na(df[[v]]))) {
                d1 <- unique(data.table(x=df[[plotx]], v=df[[v]]))
                d1 <- d1[order(d1$x),]
                d2 <- d1[, .(nas=sum(is.na(x))), v]
                xx <- myreplace(pretty(1:nrow(d2), nx), c(0, 1), verbose=F)
                xx <- xx[xx<length(d2)]
                xxl <- names(d2)[xx]
                plot(d2, type='h', col=ifelse(d2>0, 'red', 'black'),
                     axes=F, xlab=NA, ylab=NA)
                axis(1, at=xx, labels=xxl)
                axis(2, at=c(0, max(d2, na.rm=T)), las=1)
                mtext(v, 3, line=-1.5, cex=0.6)
            }
        }
    }
}

maketags <- function(x)
    rtags(ofile='TAGS', recursive=T)


modifiedfiles <- function(fold='~/dragonfly', days=7, type='f', extra='-lgo') {
    out <- system(sprintf('find %s -type %s -mtime -%i |xargs -r ls %s |column -t', fold, type, days, extra), intern=T)
    out <- out[!grepl('\\.git', out)]
}

is.git.tracked <- function(f) {
    s <- suppressWarnings(system(sprintf('git ls-files %s', f), intern=T, ignore.stderr=T))
    if (!length(s))
        return(F) else return(T)
}


check.latex.deps.flat <- function(report_path = './report.tex',
                     paths.ignore=c('^/usr/|^/var/lib|^/etc/tex|sweave/|^/dragonfly|/share/|submitted/|/dev/'),
                     ext.ignore = c('sty', 'def', 'lbx', 'fd', 'tfm', 'cfg', 'bbx', 'cbx', 'cnf',
                                    'clo', 'fmt', 'cls', 'map', 'ldf', 'dbx'),
                     only=c('/'), recursive=F, save_deps=T, use.xelatex=T, ignore.rnw=F) {
    library(data.table)
    
    extension <- function(x) {
        y <- x
        c <- grepl('\\.', y)
        y[c] <- sub('.*\\.([^.]*)$', '\\1', y[c])
        y[!c & !is.na(y)] <- ''
        return(y)
    }
    
    alldeps <- NULL
    prevdir <- getwd()
    fold <- normalizePath(dirname(report_path))
    setwd(fold)

    ## ** Flatten report
    system(sprintf('latexpand %s > the_flattened_report.tex', basename(report_path)))

    ## ** File dependencies in Sweave files
    if (!ignore.rnw) {
        rnw <- dir('.', '*.rnw$|*.Rnw$', recursive=recursive)
        rnw <- rnw[!grepl(paths.ignore, rnw)]
        basedir <- getwd()
        r1=rnw[2]
        for (r1 in rnw) {
            cat('\n************  ', r1, '  ************\n')
            rdir <- dirname(r1)
            setwd(rdir)
            r2 <- basename(r1)
            r <- readLines(r2, warn=F)
            c1 <- r[grepl('\\bload\\(', r) & !grepl('^[[:blank:]]*#', r)]
              fs1 <- sub('load\\([\'\"]+(.*)[\'\"]+.*', '\\1', c1)
            c2 <- r[grepl('\\bread\\.csv\\(', r) & !grepl('^[[:blank:]]*#', r)]
              fs2 <- sub('.*read.csv\\([\'\"]+(.*)[\'\"]+.*', '\\1', c2)
            fs <- c(fs1, fs2)
            ## Replace global variables in .mk files by their value
            c <- grepl('load\\([a-zA-Z]+', fs)
            alldeps1 <- sub('.*load\\((.*).*\\).*', '\\1', fs[c])
            s <- unlist(sapply(dir('.', '*.mk.parsed'), function(mk) readLines(mk), simplify=F))
            if (!is.null(s)) {
                s1 <- do.call('rbind', strsplit(s, '[[:blank:]]*=[[:blank:]]*'))
                s2 <- sapply(alldeps1, function(x) s1[which(s1[,1] %in% x),2], simplify=F)
                fs[c] <- ifelse(sapply(s2, length), sapply(s2, '[', 1), names(s2))
            }
            cat(paste(fs, collapse='\n'),'\n')
            fs <- normalizePath(fs)
            ## fs <- fs[!(extension(fs) %in% ext.ignore)]
            if (length(fs)) {
                alldeps <- rbind(alldeps, data.frame(infile = r1, dep = fs, stringsAsFactors = F))
            } else alldeps <- rbind(alldeps, data.frame(infile = r1, dep = NA, stringsAsFactors = F))
            setwd(basedir)
            cat('\n')
        }
    }
    ## File dependencies in tex files
    basedir <- getwd()
    tex <- 'the_flattened_report.tex'
    cat('\n************  ', tex, '  ************\n')
    t2 <- basename(tex)
    bt <- sub('\\.tex', '', t2)
    tmp <- readLines(t2, warn=F)
    if (any(grepl('begin\\{document\\}', tmp))) {
        if (!use.xelatex) {
            s <- system(sprintf('pdflatex -recorder -interaction=nonstopmode %s', bt), intern=T)
        } else {
            s <- system(sprintf('xelatex -recorder -interaction=nonstopmode %s', bt), intern=T)
        }
        f <- sprintf('%s.fls', bt)
        if (file.exists(f)) {
            fls <- readLines(f)
            fls <- sapply(strsplit(fls, ' '), function(x) x[2])
            fls <- sub('^\\./', '', fls)
            fls <- unique(fls)
            fls <- fls[!grepl(sprintf('^%s', bt), fls)]
            fls <- fls[!(fls %in% normalizePath(fold))]
            fs <- normalizePath(fls)
            if (length(fs)) {
                alldeps <- rbind(alldeps, data.table(infile = tex, dep = fs))
            } else alldeps <- rbind(alldeps, data.table(infile = tex, dep = NA))
            cat(paste(fs[!(extension(fs) %in% ext.ignore) & !grepl(paths.ignore, fs)], collapse='\n'))
            cat('\n')
        } else cat('fls file inexistent. There is a problem with this file...\n')
    } else cat('Not a master file. Skip...\n')
    setwd(basedir)
    
    cat('\n\n')

    alldeps <- alldeps[!grepl('^[[:blank:]]*\\%', dep)]
    alldeps[, dep := strtrim(dep)]
    ## Apply ignore rules
    alldeps[, ignored := ifelse( extension(dep) %in% ext.ignore  | grepl(paths.ignore, dep) | is.na(dep), T, F)]
    ## Detect dependencies that are not file names
    alldeps[, valid := NA]
    alldeps[ignored == FALSE, valid := ifelse(grepl('[<%"\'\\(\\) ,=]+', dep), F, T)]

    ## Check if the dependencies are git-tracked
    alldeps[, git_tracked := NA]
    if (any(alldeps$valid %in% T))
        alldeps[valid %in% T, git_tracked := sapply(dep, is.git.tracked)]

    print(alldeps)

    if (!is.null(alldeps) & any(alldeps$git_tracked %in% F)) {
        cat('************====  Files not tracked by GIT:  ====************\n')
        uniqdeps <- unique(alldeps[git_tracked %in% F, dep])
        cat(paste(uniqdeps, collapse='\n'))
        cat('\n\nYou may want to type:\ngit add ')
        cat(paste(uniqdeps, collapse='  '))
        cat('\n\n')
    } else cat('\nNo untracked dependencies\n')
    if (save_deps)
        write.csv(alldeps, 'tex-dependencies.csv', row.names=F)
    setwd(prevdir)
    if (any(alldeps$valid %in% F, na.rm=T)) {
        cat('\n--- Non-processed dependencies:\n')
        cat(paste(unique(alldeps$dep[alldeps$valid %in% F]), collapse='\n'))
        cat('\n')
    }
}


check.latex.deps <- function(fold='.',
                     paths.ignore=c('^/usr/|^/var/lib|^/etc/tex|sweave/|^/dragonfly|/share/|submitted/|/dev/'),
                     ext.ignore = c('sty', 'def', 'lbx', 'fd', 'tfm', 'cfg', 'bbx', 'cbx', 'cnf',
                                    'clo', 'fmt', 'cls', 'map', 'ldf', 'dbx'),
                     only=c('/'), recursive=T, save_deps=T, use.xelatex=T, ignore.rnw=F) {
    library(data.table)
    extension <- function(x) {
        y <- x
        c <- grepl('\\.', y)
        y[c] <- sub('.*\\.([^.]*)$', '\\1', y[c])
        y[!c & !is.na(y)] <- ''
        return(y)
    }
    
    alldeps <- NULL
    prevdir <- getwd()
    setwd(fold)
    ## File dependencies in Sweave files
    if (!ignore.rnw) {
        rnw <- dir('.', '*.rnw$|*.Rnw$', recursive=recursive)
        basedir <- getwd()
        r1=rnw[2]
        for (r1 in rnw) {
            cat('\n************  ', r1, '  ************\n')
            rdir <- dirname(r1)
            setwd(rdir)
            r2 <- basename(r1)
            r <- readLines(r2, warn=F)
            c1 <- r[grepl('\\bload\\(', r) & !grepl('^[[:blank:]]*#', r)]
              fs1 <- sub('load\\([\'\"]+(.*)[\'\"]+.*', '\\1', c1)
            c2 <- r[grepl('\\bread\\.csv\\(', r) & !grepl('^[[:blank:]]*#', r)]
              fs2 <- sub('.*read.csv\\([\'\"]+(.*)[\'\"]+.*', '\\1', c2)
            c3 <- r[grepl('\\bfread\\(', r) & !grepl('^[[:blank:]]*#', r)]
              fs3 <- sub('.*fread\\([\'\"]+(.*)[\'\"]+.*', '\\1', c3)
            c4 <- r[grepl('\\breadRDS\\(', r) & !grepl('^[[:blank:]]*#', r)]
              fs4 <- sub('.*readRDS\\([\'\"]+(.*)[\'\"]+.*', '\\1', c3)
            fs <- c(fs1, fs2, fs3, fs4)
            ## Replace global variables in .mk files by their value
            c <- grepl('load\\([a-zA-Z]+', fs)
            alldeps1 <- sub('.*load\\((.*).*\\).*', '\\1', fs[c])
            s <- unlist(sapply(dir('.', '*.mk.parsed'), function(mk) readLines(mk), simplify=F))
            if (!is.null(s)) {
                s1 <- do.call('rbind', strsplit(s, '[[:blank:]]*=[[:blank:]]*'))
                s2 <- sapply(alldeps1, function(x) s1[which(s1[,1] %in% x),2], simplify=F)
                fs[c] <- ifelse(sapply(s2, length), sapply(s2, '[', 1), names(s2))
            }
            cat(paste(fs, collapse='\n'),'\n')
            fs <- normalizePath(fs)
            ## fs <- fs[!(extension(fs) %in% ext.ignore)]
            if (length(fs)) {
                alldeps <- rbind(alldeps, data.frame(infile = r1, dep = fs, stringsAsFactors = F))
            } else alldeps <- rbind(alldeps, data.frame(infile = r1, dep = NA, stringsAsFactors = F))
            setwd(basedir)
            cat('\n')
        }
    }
    ## File dependencies in tex files
    tex <- dir('.', '*.tex$', recursive=recursive)
    tex <- tex[!(tex %in% 'aebr.tex')]
    basedir <- getwd()
    t=tex[3]
    for (t in tex) {
        cat('\n************  ', t, '  ************\n')
        tdir <- dirname(t)
        setwd(tdir)
        t2 <- basename(t)
        bt <- sub('\\.tex', '', t2)
        tmp <- readLines(t2, warn=F)
        if (any(grepl('begin\\{document\\}', tmp))) {
            if (!use.xelatex) {
                s <- system(sprintf('pdflatex -recorder -interaction=nonstopmode %s', bt), intern=T)
            } else {
                s <- system(sprintf('xelatex -recorder -interaction=nonstopmode %s', bt), intern=T)
            }
            f <- sprintf('%s.fls', bt)
            if (file.exists(f)) {
                fls <- readLines(f)
                fls <- sapply(strsplit(fls, ' '), function(x) x[2])
                fls <- sub('^\\./', '', fls)
                fls <- unique(fls)
                fls <- fls[!grepl(sprintf('^%s', bt), fls)]
                fls <- fls[!(fls %in% normalizePath(fold))]
                fs <- normalizePath(fls)
                if (length(fs)) {
                    alldeps <- rbind(alldeps, data.table(infile = t, dep = fs))
                } else alldeps <- rbind(alldeps, data.table(infile = t, dep = NA))
                cat(paste(fs[!(extension(fs) %in% ext.ignore) & !grepl(paths.ignore, fs)], collapse='\n'))
                cat('\n')
            } else cat('fls file inexistent. There is a problem with this file...\n')
        } else cat('Not a master file. Skip...\n')
        setwd(basedir)
    }
    
    cat('\n\n')
    alldeps <- alldeps[!grepl('^[[:blank:]]*\\%', dep)]
    alldeps[, dep := strtrim(dep)]
    ## Apply ignore rules
    alldeps[, ignored := ifelse( extension(dep) %in% ext.ignore  | grepl(paths.ignore, dep) | is.na(dep), T, F)]
    ## Detect dependencies that are not file names
    alldeps[, valid := NA]
    alldeps[ignored == FALSE, valid := ifelse(grepl('[<%"\'\\(\\) ,=]+', dep), F, T)]

    ## Check if the dependencies are git-tracked
    alldeps[, git_tracked := NA]
    if (any(alldeps$valid %in% T))
        alldeps[valid %in% T, git_tracked := sapply(dep, is.git.tracked)]

    if (!is.null(alldeps) & any(alldeps$git_tracked %in% F)) {
        cat('************====  Files not tracked by GIT:  ====************\n')
        uniqdeps <- unique(alldeps[git_tracked %in% F, dep])
        cat(paste(uniqdeps, collapse='\n'))
        cat('\n\nYou may want to type:\ngit add ')
        cat(paste(uniqdeps, collapse='  '))
        cat('\n\n')
    } else cat('\nNo untracked dependencies\n')
    if (save_deps)
        write.csv(alldeps, 'tex-dependencies.csv', row.names=F)
    setwd(prevdir)
    if (any(alldeps$valid %in% F, na.rm=T)) {
        cat('\n--- Non-processed dependencies:\n')
        cat(paste(unique(alldeps$dep[alldeps$valid %in% F]), collapse='\n'))
        cat('\n')
    }

    invisible(alldeps)
}



check.latex.sources <- function(fold = normalizePath('.'),
                        paths.ignore=c('^/usr/|^/var/lib|^/etc/tex|sweave/|^/dragonfly|/share/'),
                        ext.ignore = c('sty', 'def', 'lbx', 'fd', 'tfm', 'cfg', 'bbx', 'cbx', 'cnf',
                                       'clo', 'fmt', 'cls', 'map', 'ldf', 'dbx'),
                        only=c('/'), recursive=T, save_deps=T, use.xelatex=T, ignore.rnw=F,
                        source.types = c('r')) {

    extension <- function(x) {
        y <- x
        c <- grepl('\\.', y)
        y[c] <- sub('.*\\.([^.]*)$', '\\1', y[c])
        y[!c & !is.na(y)] <- ''
        return(y)
    }
    
    alldeps <- NULL
    prevdir <- getwd()
    setwd(fold)
    ## File dependencies in Sweave files
    if (!ignore.rnw) {
        rnw <- dir('.', '*.rnw$|*.Rnw$', recursive=recursive)
        basedir <- getwd()
        r1=rnw[2]
        for (r1 in rnw) {
            cat('\n************  ', r1, '  ************\n')
            rdir <- dirname(r1)
            setwd(rdir)
            r2 <- basename(r1)
            r <- readLines(r2)
            c1 <- r[grepl('\\bload\\(', r) & !grepl('^[[:blank:]]*#', r)]
              fs1 <- sub('load\\([\'\"]+(.*)[\'\"]+.*', '\\1', c1)
            c2 <- r[grepl('\\bread\\.csv\\(', r) & !grepl('^[[:blank:]]*#', r)]
              fs2 <- sub('.*read.csv\\([\'\"]+(.*)[\'\"]+.*', '\\1', c2)
            c3 <- r[grepl('\\bfread\\(', r) & !grepl('^[[:blank:]]*#', r)]
              fs3 <- sub('.*fread\\([\'\"]+(.*)[\'\"]+.*', '\\1', c2)
            c4 <- r[grepl('\\breadRDS\\(', r) & !grepl('^[[:blank:]]*#', r)]
              fs4 <- sub('.*readRDS\\([\'\"]+(.*)[\'\"]+.*', '\\1', c2)
            fs <- c(fs1, fs2, fs3, fs4)
            ## Replace global variables in .mk files by their value
            c <- grepl('load\\([a-zA-Z]+', fs)
            alldeps1 <- sub('.*load\\((.*).*\\).*', '\\1', fs[c])
            s <- unlist(sapply(dir('.', '*.mk.parsed'), function(mk) readLines(mk), simplify=F))
            if (!is.null(s)) {
                s1 <- do.call('rbind', strsplit(s, '[[:blank:]]*=[[:blank:]]*'))
                s2 <- sapply(alldeps1, function(x) s1[which(s1[,1] %in% x),2], simplify=F)
                fs[c] <- ifelse(sapply(s2, length), sapply(s2, '[', 1), names(s2))
            }
            cat(paste(fs, collapse='\n'),'\n')
            fs <- normalizePath(fs)
            ## fs <- fs[!(extension(fs) %in% ext.ignore)]
            if (length(fs)) {
                alldeps <- rbind(alldeps, data.frame(infile = r1, dep = fs, stringsAsFactors = F))
            } else alldeps <- rbind(alldeps, data.frame(infile = r1, dep = NA, stringsAsFactors = F))
            setwd(basedir)
            cat('\n')
        }
    }
    ## File dependencies in tex files
    tex <- dir('.', '*.tex$', recursive=recursive)
    tex <- tex[!(tex %in% 'aebr.tex')]
    basedir <- getwd()
    t=tex[3]
    for (t in tex) {
        cat('\n************  ', t, '  ************\n')
        tdir <- dirname(t)
        setwd(tdir)
        t2 <- basename(t)
        bt <- sub('\\.tex', '', t2)
        tmp <- readLines(t2)
        if (any(grepl('begin\\{document\\}', tmp))) {
            if (!use.xelatex) {
                s <- system(sprintf('pdflatex -recorder -interaction=nonstopmode %s', bt), intern=T)
            } else {
                s <- system(sprintf('xelatex -recorder -interaction=nonstopmode %s', bt), intern=T)
            }
            f <- sprintf('%s.fls', bt)
            if (file.exists(f)) {
                fls <- readLines(f)
                fls <- sapply(strsplit(fls, ' '), function(x) x[2])
                fls <- sub('^\\./', '', fls)
                fls <- unique(fls)
                fls <- fls[!grepl(sprintf('^%s', bt), fls)]
                fls <- fls[!(fls %in% normalizePath(fold))]
                fs <- normalizePath(fls)
                if (length(fs)) {
                    alldeps <- rbind(alldeps, data.frame(infile = t, dep = fs, stringsAsFactors = F))
                } else alldeps <- rbind(alldeps, data.frame(infile = t, dep = NA, stringsAsFactors = F))
                cat(paste(fs[!(extension(fs) %in% ext.ignore) & !grepl(paths.ignore, fs)], collapse='\n'))
                cat('\n')
            } else cat('fls file inexistent. There is a problem with this file...\n')
        } else cat('Not a master file. Skip...\n')
        setwd(basedir)
    }
    
    cat('\n\n')
    alldeps <- alldeps[!grepl('^[[:blank:]]*\\%', alldeps$dep), ]
    alldeps$dep <- strtrim(alldeps$dep)
    ## Apply ignore rules
    alldeps$ignored <- ifelse( extension(alldeps$dep) %in% ext.ignore  | grepl(paths.ignore, alldeps$dep) |
                                is.na(alldeps$dep), T, F)
    alldeps <- subset(alldeps, ignored == F)
    sources <- sapply(1:nrow(alldeps), function(i) {
                          cat(i, '\n')
                          d <- alldeps[i, ]
                          f <- basename(d$dep)
                          match <- system(sprintf('grep -r --include="*.r" %s .', f), intern = T)
                          if (length(match)) {
                              m <- do.call('rbind', sapply(strsplit(match, ':'), function(x) {
                                                               as.data.frame(rbind(x), stringsAsFactors=F)
                                                           }, simplify=F))
                              names(m) <- c('source', 'call')
                              m <- cbind(infile = d$infile, dep = sub(normalizePath(fold), '', d$dep), m)
                              m$source_mk <- sapply(m$source, function(sc) {
                                                       fr <- basename(sc)
                                                       matchr <- system(sprintf('grep -r --include="makefile" %s .', fr),
                                                                       intern = T)
                                                       if (length(matchr)) {
                                                           mr <- paste( unique(sapply(strsplit(matchr, ':'), '[', 1)),
                                                                      collapse='; ')
                                                       } else mr <- NA
                                                       return(mr)
                                                   })
                          } else {
                              m <- cbind(infile = d$infile, dep = sub(normalizePath(fold), '', d$dep), source = NA,
                                        call = NA, source_mk = NA)
                          }
                          return(m)
                      }, simplify=F)
    src <- do.call('rbind', sources)
    rownames(src) <- NULL
    src$loadsave <- ifelse(is.na(src$source), NA, ifelse(grepl('save\\(', src$call), 'save',
                                                        ifelse(grepl('load\\(', src$call), 'load', '?')))
    if (save_deps) {
        out <- sprintf('%s/latex-deps-in-r-files.csv', fold)
        cat('Dependencies saved in', out, '\n')
        write.csv(src, out, row.names=F)
    }
    return(src)
}


### Check order of labels and refs in LaTeX
check.latex.labels.order <- function(texbase = './report.tex', with.inputs = T, ignores = c('sec', 'eq', 'app')) {
    
    fold <- normalizePath(dirname(texbase))
    base <- basename(texbase)
    get.refs <- function(tex) {
        reflines <- grep('ref\\{', tex, val=T)
        r=reflines[1]
        refs <- as.vector(unlist(sapply(reflines, function(r) {
            matches <- gregexpr( 'ref\\{', r )[[1]]
            sapply(matches, function(m) {
                sub('ref\\{([^\\}]+)\\}.*', '\\1', substr(r, m, nchar(r)))
            })
        })))
        return(refs[!duplicated(refs)])
    }
    get.labels <- function(tex) {
        lablines <- grep('label\\{', tex, val=T)
        r=lablines[1]
        labs <- as.vector(unlist(sapply(lablines, function(r) {
            matches <- gregexpr( 'label\\{', r )[[1]]
            sapply(matches, function(m) {
                sub('label\\{([^\\}]+)\\}.*', '\\1', substr(r, m, nchar(r)))
            })
        })))
        return(labs)
    }

    ## Get inputs
    tex <- readLines(texbase)
    inputs <- c(normalizePath(texbase),
               paste0(fold, '/', gsub('.*\\{(.*)\\}.*', '\\1', grep('\\input\\{', tex, val=T)), '.tex'))

    ## Get refs
    allrefs <- do.call('rbind', sapply(inputs, function(inp) {
        tex <- readLines(inp)
        tex <- tex[!grepl('^[[:blank:]]*%', tex)]
        refs <- get.refs(tex)
        if (length(refs)) {
            return(data.frame(file=sub('\\.tex', '', basename(inp)), ref=refs, stringsAsFactors = F))
        }
    }, simplify=F, USE.NAMES = F))

    ## Get labs
    alllabs <- do.call('rbind', sapply(inputs, function(inp) {
        tex <- readLines(inp)
        tex <- tex[!grepl('^[[:blank:]]*%', tex)]
        labs <- get.labels(tex)
        if (length(labs)) {
            return(data.frame(file=sub('\\.tex', '', basename(inp)), lab=labs, stringsAsFactors = F))
        }
    }, simplify=F, USE.NAMES = F))

    if (any(duplicated(alllabs$lab))) {
        warning('Duplicated labels:  ', paste( alllabs$lab[duplicated(alllabs$lab)], collapse =', '))
    }

    ## Ignore some types
    for (i in ignores) {
        alllabs <- alllabs[!grepl(sprintf('^%s:', i), alllabs$lab), ]
        allrefs <- allrefs[!grepl(sprintf('^%s:', i), allrefs$ref), ]
        rownames(alllabs) <- NULL
        rownames(allrefs) <- NULL
    }

    cat('\n=== Labels not cited:\n')
    print(setdiff(alllabs$lab, allrefs$ref))

    alllabs$ref.pos <- match(alllabs$lab, allrefs$ref)
    alllabs$ref.file <- allrefs$file[match(alllabs$lab, allrefs$ref)]
    allrefs$lab.pos <- match(allrefs$ref, alllabs$lab)
    allrefs$lab.file <- alllabs$file[match(allrefs$ref, alllabs$lab)]

    return( list( labels = alllabs,  refs = allrefs ) )
}


check.latex.sources.in.makefiles <- function(fold='.', paths.ignore=c('^/usr/|^/var/lib|^/etc/tex|sweave/|^/dragonfly|/share/'),
                                     ext.ignore = c('sty', 'def', 'lbx', 'fd', 'tfm', 'cfg', 'bbx', 'cbx', 'cnf',
                                                    'clo', 'fmt', 'cls', 'map', 'ldf', 'dbx'),
                                     only=c('/'), recursive=T, save_deps=T, use.xelatex=T, ignore.rnw=F,
                                     source.types = c('r')) {

    extension <- function(x) {
        y <- x
        c <- grepl('\\.', y)
        y[c] <- sub('.*\\.([^.]*)$', '\\1', y[c])
        y[!c & !is.na(y)] <- ''
        return(y)
    }
    
    alldeps <- NULL
    prevdir <- getwd()
    setwd(fold)
    ## File dependencies in Sweave files
    if (!ignore.rnw) {
        rnw <- dir('.', '*.rnw$|*.Rnw$', recursive=recursive)
        basedir <- getwd()
        r1=rnw[2]
        for (r1 in rnw) {
            cat('\n************  ', r1, '  ************\n')
            rdir <- dirname(r1)
            setwd(rdir)
            r2 <- basename(r1)
            r <- readLines(r2)
            c1 <- r[grepl('\\bload\\(', r) & !grepl('^[[:blank:]]*#', r)]
              fs1 <- sub('load\\([\'\"]+(.*)[\'\"]+.*', '\\1', c1)
            c2 <- r[grepl('\\bread\\.csv\\(', r) & !grepl('^[[:blank:]]*#', r)]
              fs2 <- sub('.*read.csv\\([\'\"]+(.*)[\'\"]+.*', '\\1', c2)
            fs <- c(fs1, fs2)
            ## Replace global variables in .mk files by their value
            c <- grepl('load\\([a-zA-Z]+', fs)
            alldeps1 <- sub('.*load\\((.*).*\\).*', '\\1', fs[c])
            s <- unlist(sapply(dir('.', '*.mk.parsed'), function(mk) readLines(mk), simplify=F))
            if (!is.null(s)) {
                s1 <- do.call('rbind', strsplit(s, '[[:blank:]]*=[[:blank:]]*'))
                s2 <- sapply(alldeps1, function(x) s1[which(s1[,1] %in% x),2], simplify=F)
                fs[c] <- ifelse(sapply(s2, length), sapply(s2, '[', 1), names(s2))
            }
            cat(paste(fs, collapse='\n'),'\n')
            fs <- normalizePath(fs)
            ## fs <- fs[!(extension(fs) %in% ext.ignore)]
            if (length(fs)) {
                alldeps <- rbind(alldeps, data.frame(infile = r1, dep = fs, stringsAsFactors = F))
            } else alldeps <- rbind(alldeps, data.frame(infile = r1, dep = NA, stringsAsFactors = F))
            setwd(basedir)
            cat('\n')
        }
    }
    ## File dependencies in tex files
    tex <- dir('.', '*.tex$', recursive=recursive)
    tex <- tex[!(tex %in% 'aebr.tex')]
    basedir <- getwd()
    t=tex[3]
    for (t in tex) {
        cat('\n************  ', t, '  ************\n')
        tdir <- dirname(t)
        setwd(tdir)
        t2 <- basename(t)
        bt <- sub('\\.tex', '', t2)
        tmp <- readLines(t2)
        if (any(grepl('begin\\{document\\}', tmp))) {
            if (!use.xelatex) {
                s <- system(sprintf('pdflatex -recorder -interaction=nonstopmode %s', bt), intern=T)
            } else {
                s <- system(sprintf('xelatex -recorder -interaction=nonstopmode %s', bt), intern=T)
            }
            f <- sprintf('%s.fls', bt)
            if (file.exists(f)) {
                fls <- readLines(f)
                fls <- sapply(strsplit(fls, ' '), function(x) x[2])
                fls <- sub('^\\./', '', fls)
                fls <- unique(fls)
                fls <- fls[!grepl(sprintf('^%s', bt), fls)]
                fls <- fls[!(fls %in% normalizePath(fold))]
                fs <- normalizePath(fls)
                if (length(fs)) {
                    alldeps <- rbind(alldeps, data.frame(infile = t, dep = fs, stringsAsFactors = F))
                } else alldeps <- rbind(alldeps, data.frame(infile = t, dep = NA, stringsAsFactors = F))
                cat(paste(fs[!(extension(fs) %in% ext.ignore) & !grepl(paths.ignore, fs)], collapse='\n'))
                cat('\n')
            } else cat('fls file inexistent. There is a problem with this file...\n')
        } else cat('Not a master file. Skip...\n')
        setwd(basedir)
    }
    
    cat('\n\n')
    alldeps <- alldeps[!grepl('^[[:blank:]]*\\%', alldeps$dep), ]
    alldeps$dep <- strtrim(alldeps$dep)
    ## Apply ignore rules
    alldeps$ignored <- ifelse( extension(alldeps$dep) %in% ext.ignore  | grepl(paths.ignore, alldeps$dep) |
                                is.na(alldeps$dep), T, F)
    alldeps <- subset(alldeps, ignored == F)
    sources <- sapply(1:nrow(alldeps), function(i) {
                          cat(i, '\n')
                          d <- alldeps[i, ]
                          f <- basename(d$dep)
                          match <- system(sprintf('grep -ri --include="makefile" %s .', f), intern = T)
                          if (length(match)) {
                              m <- do.call('rbind', sapply(strsplit(match, ':'), function(x) {
                                                               as.data.frame(rbind(x), stringsAsFactors=F)
                                                           }, simplify=F))
                              names(m) <- c('source', 'call')
                              m <- cbind(infile = d$infile, dep = sub(normalizePath(fold), '', d$dep), m)
                          } else {
                              m <- cbind(infile = d$infile, dep = sub(normalizePath(fold), '', d$dep), source = NA,
                                         call = NA)
                          }
                          return(m)
                      }, simplify=F)
    src <- do.call('rbind', sources)
    rownames(src) <- NULL
    if (save_deps) {
        cat('Dependencies saved in latex-deps-in-makefiles.csv\n')
        write.csv(src, 'latex-deps-in-makefiles.csv', row.names=F)
    }
    return(src)
}


## Stangle including also \Sexpr expressions
mystangle <- function(file, outfile=sub('(\\.[^.]+)$', '_stangled.R', file)) {
    rnw <- readLines(file)

    get.sexprs <- function(start, end) {
        poss <- start:end
        sexprs <- grep('Sexpr\\{', rnw[poss], val=F)
        if (length(sexprs)) {
            sxppos <- poss[sexprs]
            cmds <- sapply(1:length(sxppos), function(i) {
                               rnw1 <- rnw[sxppos[i]]
                               sxpstarts <- gregexpr('Sexpr', rnw1)[[1]]
                               if (length(sxpstarts) == 1) { ## single \Sexpr in line
                                   cmd <- sub('.*Sexpr *\\{([^}]+)\\}.*', '\\1', rnw1)
                               } else { ## multiple \Sexpr in line
                                   cmd <- sapply(1:length(sxpstarts), function(j) {
                                                     pos <- sxpstarts[j]
                                                     x <- substr(rnw1, pos, ifelse(j==length(sxpstarts), nchar(rnw1), sxpstarts[j+1]))
                                                     return(sub('.*Sexpr *\\{([^}]+)\\}.*', '\\1', x))
                                                 })
                               }
                               cmd <- paste(c(paste0('## l. ', sxppos[i]), cmd), collapse='\n')
                           })
            return(c('\n\n#####  Sexpr expressions', cmds, '\n\n'))
        } else return()
    }

    all.cmds <- NULL
    cmd.blocks.pos <- grep('^<<', rnw)
    end.blocks.pos <- grep('^@', rnw)
    i=1
    for (i in 1:length(cmd.blocks.pos)) {
        start <- cmd.blocks.pos[i]
        end <- end.blocks.pos[end.blocks.pos > start][1]
        blockname <- sub('<<(.*)>>=', '\\1', rnw[start])
        ## Get code blocks
        all.cmds <- c(all.cmds, '\n\n',
                      paste0('######  Block "', blockname, '" -- lines ', start, '-', end, '  ######\n'),
                      rnw[(start+1):(end-1)])
        ## Add following \Sexpr expressions until next code block (or end of file)
        all.cmds <- c(all.cmds,
                      get.sexprs(end, ifelse((i+1) <= length(cmd.blocks.pos),
                                             cmd.blocks.pos[i+1], length(rnw))))
    }
    cat('Results saved to', outfile,'\n')
    ## Save results
    writeLines(all.cmds, outfile)
}

sqlquery <- function(query, db='oreo-2013v1', host='titi', port=5433) {
    library(RPostgreSQL)
    drv <- dbDriver("PostgreSQL")
    conn <- dbConnect(drv, dbname=db, host=host, port=port)
    tryCatch(dat <- dbGetQuery(conn, query), finally={dbDisconnect(conn);dbUnloadDriver(drv)})
    dbDisconnect(conn); dbUnloadDriver(drv)
    return(dat)   
}

sql_all_but_geom <- function(tablename='all_captures', compl_txt='', schema='public',
                             dbname='oreo-2014v1', host='titi', port=5433, verbose=F) {
    ## Does a SELECT * FROM, but excluding geometry columns
    ## compl_txt: Extras if needed (for WHERE, ORDER BY, etc.)
    if (grepl('\\.', tablename)) {
        schema=strsplit(tablename, '\\.')[[1]][1]
        tablename1=strsplit(tablename, '\\.')[[1]][2]
    } else {
        schema='public'
        tablename1=tablename
    }
    library(RPostgreSQL)
    drv <- dbDriver("PostgreSQL")
    conn <- dbConnect(drv, dbname=dbname, host=host, port=port)
    ## Get column names (removing geometry columns)
    cols <- rev(unlist(dbGetQuery(conn, sprintf("
SELECT c.column_name FROM information_schema.columns As c
            WHERE table_name = '%s' AND table_schema = '%s' AND udt_name <> 'geometry'
", gsub('[\"\']', '', tablename1), schema)))); names(cols) <- NULL
    ## SQL statement to select all columns but geometry ones
    selcols <- paste(cols, collapse=', ')
    sqlquery <- sprintf("SELECT %s from %s %s", selcols, tablename, compl_txt)
    if (verbose) cat('\n', sqlquery, '\n')
    sqlres <- dbGetQuery(conn, sqlquery)
    dbDisconnect(conn); dbUnloadDriver(drv)
    return(sqlres)
}

## Close all opened connections
close.db.conn <- function() {
    library(RPostgreSQL)
    sapply(dbListConnections(dbDriver("PostgreSQL")), dbDisconnect)
}


## View TeX document as PDF from a tex file (not self-sufficient)
preview_tex <- function(texfile, dir='.') {

    if (dir != '.') {dir.create(dir); file.copy(texfile, sprintf('%s/', dir))}
    tex <- c('
\\documentclass[a4paper,11pt]{article}
\\usepackage{amsmath}
\\usepackage{rotating}
\\usepackage{Sweave}
\\usepackage{lineno}
\\usepackage{setspace}
\\usepackage{dcolumn}
\\usepackage{placeins}
\\usepackage[utf8]{inputenc}
\\newcolumntype{d}[0]{D{.}{.}{-1}}
\\usepackage[margin=2.5cm]{geometry}
\\usepackage{graphicx}
\\usepackage{array}
\\usepackage{multirow}
\\usepackage{rotating}
\\usepackage{engord}
\\usepackage{textcomp}
\\usepackage[perpage,para,symbol*]{footmisc}
\\usepackage[T1]{fontenc}
\\usepackage{times}
\\usepackage{mathptmx} %% Times maths font
\\DeclareMathSizes{11}{11}{8}{6}
\\usepackage{tocloft}
\\usepackage[usenames, dvipsnames]{color}
\\usepackage{colortbl}
\\usepackage{booktabs}
\\usepackage{longtable}
\\usepackage{textcomp}
\\usepackage{hhline}
\\usepackage[font={small,bf},captionskip=0pt,
            nearskip=0pt,farskip=0pt,position=top,
            justification=justified,singlelinecheck=false]{subfig}
\\usepackage[perpage,para,symbol*]{footmisc}
\\usepackage{hyperref}

\\usepackage[bf,sf,pagestyles]{titlesec}

\\newcommand{\\plaintitle}{Temp output}
\\newcommand{\\reporttitle}{Temp output}
\\newcommand{\\pdftitle}{Temp output}
\\newcommand{\\pdfauthors}{authors}
\\newcommand{\\reportno}{??}

\\usepackage{type1cm}
\\usepackage{eso-pic}

%%\\input{/dragonfly/latex/mfish/aebr.tex}

\\usepackage[textsize=scriptsize]{todonotes}

\\begin{document}
',
             sprintf('\\input{%s}', sub('.tex', '', texfile)),
             '\\end{document}\n')
    ## tex <- paste(tex, collapse='\n')
    writeLines(tex, sprintf('%s/tex_output.tex', dir), sep='\n')
    system(sprintf('cd %s && xelatex tex_output && xdg-open tex_output.pdf', dir))
}

whichseason <- function(d, starts, names) {
    library(lubridate)
    np <- length(starts)
    if (class(d) != 'Date') d <- as.Date(d)
    if (class(starts) != 'Date') starts <- as.Date(starts)

    startsyday <- c(lubridate::yday(starts), lubridate::yday(starts[1])+366)
    minyday <- startsyday[1]
    startsyday0 <- startsyday - minyday
    startsyday0 <- ifelse(startsyday0<0, startsyday0+366, startsyday0)

    dyday <- lubridate::yday(d)
    dyday0 <- lubridate::yday(d) - minyday
    dyday0 <- ifelse(dyday0<0, dyday0+366, dyday0)
    
    pers <- cut(dyday0, breaks=startsyday0, right=F, labels=names)
    return(pers)
}



quantsmooth <- function(x, y, ndivs=20, ...) {
    qq <- sort(quantile(x, seq(0, 1, length.out=ndivs+1)))
    qqmids <- (qq[2:length(qq)] + qq[1:(length(qq)-1)]) / 2
    varcut <- qqmids[findInterval(x, qq, rightmost.closed=T, all.inside=T)]
    varcutf <- factor(varcut, levels=qqmids)
    qqy <- as.numeric(tapply(y, varcutf, mean))
    df <- data.frame(midpt=qqmids, y=qqy)
    attr(df, 'n') <- as.numeric(tapply(y, varcut, length))
    attr(df, 'supsmu') <- supsmu(df$midpt, df$y)
    return(df)
}


corr_plot <- function(df, with.diag.lines=F, use.dens.cols=T) {

    library(corrgram)

    ## Customised function to add correlation coefficient
    panel.tri <- function (x, y, corr = NULL, col.regions, ...) {
        if (is.null(corr)) 
            corr <- cor(x, y, use = "pair")
        ncol <- 14
        pal <- col.regions(ncol)
        col.ind <- as.numeric(cut(corr, breaks = seq(from = -1, to = 1, 
                                                     length = ncol + 1), include.lowest = TRUE))
        usr <- par("usr")
        rect(usr[1], usr[3], usr[2], usr[4], col = pal[col.ind], 
             border = NA)
        if (!is.na(corr)) {
            if (with.diag.lines) 
                rect(usr[1], usr[3], usr[2], usr[4], density = 5,
                     angle = ifelse(corr > 0, 45, 135), col = "white")
            text(mean(usr[1:2]), mean(usr[3:4]), round(corr,3),
                 col=ifelse(abs(corr)>0.55, 'white', 'black'))
        }
        box(col = "lightgray")
    }

    panel.diag <- function (x, corr = NULL, ...) {
        if (!is.null(corr)) 
            return()
        ## From panel.density
        dd = density(x, na.rm = TRUE)
        xr = range(dd$x)
        yr = range(dd$y)
        par(usr = c(min(xr), max(xr), min(yr), max(yr) * 1.1))
        plot.xy(xy.coords(dd$x, dd$y), type = "l", col = grey(0.7), ...)
        box(col = "lightgray")
        ## Modified from panel.minmax
        minx <- round(min(x, na.rm = TRUE), 2)
        maxx <- round(max(x, na.rm = TRUE), 2)
        usr <- par("usr")
        text(usr[1], usr[3], minx, cex = 0.8, adj = c(-0.1, -0.1))
        text(usr[2], usr[4], maxx, cex = 0.8, adj = c(1.1, 1.1))
    }

    panel.pts <- function (x, y, corr = NULL, col.regions, ...) {
        if (!is.null(corr)) 
            return()
        if (use.dens.cols)  cols <- densCols(x, y)  else  cols <- '#00000044' #
        plot.xy(xy.coords(x, y), type = "p", col=cols, ...)
        box(col = "lightgray")
    }

    corrgram(df, order=FALSE, lower.panel=panel.tri,
             upper.panel=panel.pts, text.panel=panel.txt,
             diag.panel=panel.diag, main="", pch='.')

}


## Function to extract the references used in report (tex, Rnw, rnw files) from the main bibliography file
getbibrefs <- function(texdir='.', overwrite=T, outbib='bib.bib',
                       bibfile='~/dragonfly/bibliography/mfish.bib') {
    texdir <- normalizePath(texdir)
    texfiles <- grep('\\.tex$|\\.[Rr]nw$', dir(texdir), val=T)
    bib <- readLines(bibfile)
    refstarts <- grep('^[[:blank:]]*@', bib)
    bibtags <- sort(unique(as.character(sort(unlist(sapply(texfiles, function(tex) {
                                                               t <- readLines(paste0(texdir, '/', tex))
                                                               t <- t[!grepl('^[[:blank:]]*%', t)]  ## remove comments
                                                               t <- gsub('\\[[^]]*\\]', '', gsub('[[:blank:]]+', ' ', paste(t, collapse=' ')))
                                                               locs <- gregexpr('\\\\cite[pt]*', t)[[1]]
                                                               if (!any(locs == -1)) {
                                                                   return(grep('_', sort(unique(gsub(' +', '', unlist(sapply(locs, function(i) {
                                                                                                                                 strsplit(sub('\\\\cite[pt]*\\{([^\\}]+)\\}.*', '\\1', substr(t, i, nchar(t))), ',')
                                                                                                                             }, simplify=F))))), val=T))
                                                               } else return(NULL)
                                                           }, simplify=F))))))
    cat(length(bibtags), 'references used in total.\n')
    subbib <- sapply(bibtags, function(tag) {
                         refstart <- grep(sprintf('\\<%s\\>', tag), bib)[1]
                         if (!is.na(refstart)) {
                             return(bib[refstart:(min(refstarts[refstarts > refstart])-1)])
                         } else cat('WARNING!! Could not find ref: ', dQuote(tag), '\n')
                     })
    if (!file.exists(outbib) | overwrite) {
        cat(paste(unlist(subbib), collapse='\n'), file='bib.bib')
        cat('References exported to ', dQuote(outbib), '.\n', sep='')
    } else cat('File', outbib, 'exists and overwrite=FALSE\n')
}


##  Load environment variables contained in *.env files
setupenv <- function(verb = F, envs = sort(dir('.', '*.env$')), warn=T) {
    if (!length(envs) & warn)  warning('No environment file found')
    ## Function to recursively replace environment variables
    parseval <- function(x) {
        done <- any(gregexpr('\\$\\{[^\\)]+\\}', x)[[1]] == -1)
        while (!done) {
            locs <- gregexpr('\\$\\{[^\\}]+\\}', x)[[1]]
            var <- substr(x, locs[1]+2, locs[1] + attr(locs, 'match.length')[1]-2)
            repl <- get(var, .GlobalEnv)
            if (repl == '')  {
                cat('\n')
                stop('Environment variable ', var, ' not found in ', x)
            }
            x <- sub(sprintf('\\$\\{%s\\}', var), repl, x)
            done <- any(gregexpr('\\$\\([^\\)]+\\)', x)[[1]] == -1)
        }
        return(x)
    }
    ## Loop through all envs files
    for (f in envs) {
        if (verb)  cat('\n=== Loading ', f, '\n')
        env <- readLines(f)
        env <- gsub('[[:blank:]]*=[[:blank:]]*', '=', gsub('[[:blank:]]+', '', gsub('#.*$', '', env)))
        env <- env[env != '']
        env <- do.call('rbind', strsplit(env, '='))
        ## env <- gsub('\\$\\(([^)]+)\\)', '${\\1}', env)
        for (i in 1:nrow(env)) {
            repl <- parseval(env[i, 2])
            if (verb) {
                cat(env[i,1], ' ')
                if (env[i,1] %in% ls(.GlobalEnv))
                    prev <- get(env[i,1])
                if (prev != repl)
                    cat('\tOverwriting different value !!!\n\tWas:', prev, '\n\tNow:', repl,'\n')
                if (prev == repl)  cat('\tOverwriting same value')
            }
            eval( parse( text = sprintf('%s = "%s"', env[i,1], repl)), envir=.GlobalEnv)
            if (verb)  cat('\tok\n')
        }
        if (verb)  cat('\n')
    }
}

pgExport <- function(shp, tablename='tmpshp', dbname='mygis', dbuser = 'dba', verb=T) {
    if (!grepl('^Spatial[PL]', class(shp)))  stop('Object to export is not spatial')
    if (!is.null(attr(suppressWarnings(system(sprintf("psql -lqt | cut -d \\| -f 1 | grep -w %s", dbname),
                                              intern=T)), 'status'))) {
        if (verb)  cat('\nCreating mygis database...\n')
        system(sprintf('createdb %s', dbname))
        system(sprintf("psql -c 'create extension postgis' %s -U %s", dbname, dbuser))
        system(sprintf("psql -c 'create extension postgis_topology' %s -U %s", dbname, dbuser))
    }
    library(rgdal)
    if (verb)  cat(sprintf('\nExporting to %s database...\n', dbname))
    if (!grepl('DataFrame', class(shp)))  shp <- as(shp, paste0(class(shp), 'DataFrame'))
    writeOGR(shp, '/tmp', tablename, 'ESRI Shapefile', overwrite_layer = T)
    system(sprintf("psql %s -U %s -c 'drop table if exists %s'", dbname, dbuser, tablename))
    system(sprintf('shp2pgsql /tmp/%s.shp | psql %s -U %s', tablename, dbname, dbuser))
    proj <- proj4string(shp)
    if (grepl('epsg:[0-9]+ ', proj)) {
        proj4 <- sub('.*epsg:([0-9]+) .*', '\\1', proj)
        system(sprintf("psql %s -U %s -c 'update %s set geom = st_setsrid(geom, %s)'",
                       dbname, dbuser, tablename, proj4))
    } else warning('No epsg found in input object, projection not set in Postgres')
}

pgUnion <- function(inname='tmpshp', outname='out_union', incol='geom', outcol='geom', dbname='mygis', dbuser='dba') {
    system(sprintf("psql %1$s -U %6$s -c 'drop table if exists %2$s;
                    create table %2$s as select st_multi(st_union(%3$s)) as %4$s from %5$s'",
                   dbname, outname, incol, outcol, inname, dbuser))
    return(outname)
}

pgBuffer <- function(inname, width, outname='out_buffer', incol='geom', outcol='geom', dbname='mygis', dbuser='dba') {
    system(sprintf("psql %1$s -U %7$s -c 'drop table if exists %2$s;
                    create table %2$s as select st_buffer(%3$s, %4$s) as %5$s from %6$s'",
                   dbname, outname, incol, width, outcol, inname, dbuser))
    return(outname)
}

pgCmd <- function(from, select, outname, dbname='mygis', dbuser = 'dba') {
    system(sprintf("psql %1$s -U %5$s -c 'drop table if exists %2$s;
                    create table %2$s as select %3$s from %4$s'",
                   dbname, outname, select, from, dbuser))
    return(outname)
}

pgReturn <- function(tablename='tmpshp', outfile='out_shp.shp', dbname='mygis', dbuser='dba') {
    library(rgdal)
    system(sprintf('ogr2ogr -f "ESRI Shapefile" -overwrite %s PG:"dbname=%s user=%s" -sql "select * from %s"',
                   outfile, dbname, dbuser, tablename))
    shp <- readOGR(dirname(outfile), sub('\\..*$', '', basename(outfile)))
    return(shp)
}

zoom <- function(plotfun, new=T) {
    if (new | !length(dev.list())) {
        x11()
        plotfun()
    }
    xys <- locator(2)
    plotfun(xlim=c(min(xys$x), max(xys$x)),
            ylim=c(min(xys$y), max(xys$y)))
}

zoom1 <- function(spobj, new=T, ...) {
    x11()
    plot(spobj, ...)
    xys <- locator(2)
    plot(spobj, xlim=c(min(xys$x), max(xys$x)),
         ylim=c(min(xys$y), max(xys$y)), ...)
    cat(paste0('xlim=c(', min(xys$x),',',max(xys$x), '), ylim=c(', min(xys$y), ',', max(xys$y), ')\n'))
}


str2org <- function(what, tmpfile = tempfile('str_output_'), open = T) {
## Return the output of str() as an org file (open in emacs) for better visibility and collapsible levels
    orgfile <- paste0(tmpfile, '.org')
    tryCatch({
        capture.output(str(what, list.len = 199), file=tmpfile)
        }, error = function(e) e, finally = warning('Some error occurred'))
    strout <- readLines(tmpfile)
    strout <- gsub('^([[:blank:].]*)\\.\\.\\$', '\\1.. .. $', strout)
    strout <- gsub('^([[:blank:].]*)\\.\\.\\-', '\\1.. .. -', strout)
    strout <- gsub('^[[:blank:]]*\\$', '.. $', strout)
    strout <- gsub('^([[:blank:].]*)\\[', '\\1.. [', strout)
    while (length(grep('\\.\\. \\.', strout))) {
        strout <- gsub('[[:blank:]*]*\\.\\.([[:blank:]@$-]*[^.])', '* \\1', strout)
    }
    while( length(grep('\\*[[:blank:]]+\\*', strout)) ) {
        strout <- gsub('\\*[[:blank:]]+\\*', '**', strout)
    }
    strout <- gsub('\\*[[:blank:]]+([$-])', '* \\1', strout)
    strout <- c(strout, "* org conf", "#+STARTUP: indent", "#+STARTUP: showstars", "#+STARTUP: showall",
               "#+INFOJS_OPT: view:info toc:true view:content tdepth:2",
               "#+SETUPFILE: ~/.emacs.d/github_projects/org-html-themes/setup/theme-readtheorg.setup")
    writeLines(strout, orgfile, sep = '\n')
    if (open)
        system(sprintf('emacsclient -n %s', orgfile), intern = F, wait = F)
}

files2org <- function(fold = '.', outfile = tempfile('file_list_', fileext = '.org'), remove.base = T, open = T,
              details = T,
              ignore = c('.*~', '.*/\\.git.*'), #c('dead.letter', 'ed_music', 'fin_music', 'Podcasts'),
              title = 'File list') {
    library(data.table)
    ## fs0 <- dir(fold, recursive = T, full.names = T, include.dirs = T)

    fs1 <- system(sprintf("find -P %s %s",
                         fold,
                         paste(sprintf("\\( ! -regex '%s' \\)", ignore), collapse = ' ')),
                 intern=T)
    invalidsymlinks <- system(sprintf("find %s -type l -xtype l", fold), intern=T)
    fs1 <- fs1[ !(fs1 %in% invalidsymlinks) ]
    fs <- normalizePath(fs1)
    finfos <- file.info(fs)
    infos <- ifelse(finfos$isdir,
                   sprintf('mod: %s', finfos$mtime),
                   sprintf('mod: %s - %s bytes', finfos$mtime, finfos$size))
    if (remove.base)
        fs <- gsub(normalizePath(fold), '', fs)
    fs <- gsub('^/', '', fs)
    s <- strsplit(fs, '/')
    mx <- max(sapply(s, length))
    ## Remove dups
    isdup <- duplicated(s)
    s <- s[which(!isdup)]
    infos <- infos[!isdup]
    finfos <- finfos[!isdup, ]
    ## Collapse
    s2 <- do.call('rbind', sapply(s, function(x) c(x, rep('', mx - length(x))), simplify=F))
    ## Sort
    s[sapply(s, length) == 0] <- ''
    so <- sapply(1:length(s), function(i) {
                    x <- s[[i]]
                    if (finfos$isdir[i]) {
                        x[length(x)] <- paste0(x[length(x)], '/')
                    }
                    return(x)
                }, simplify=F)
    so <- do.call('rbind', sapply(so, function(x) c(x, rep('', mx - length(x))), simplify=F))
    for (i in 1:ncol(so)) {
        c <- grepl('/$', so[,i])
        so[c, i] <- paste0('0000', so[c, i])
        c <- grepl('^\\.', so[,i])
        so[c, i] <- paste0('00', so[c, i])
    }
    ord <- do.call(order, data.frame(so))
    s2 <- s2[ord, ]
    infos <- infos[ord]
    finfos <- finfos[ord, ]
    ## Format for org
    if (mx > 1) {
        for (i in 1:mx) {
            c <- c(F, s2[2:nrow(s2), i] == s2[1:(nrow(s2)-1), i])  &  c(F, s2[1:(nrow(s2)-1),i] != '')
            s2[c, i] <- '*'
        }
    }
    s3 <- cbind('*', s2)
    s4 <- apply(s3, 1, function(x) paste(x, collapse='/'))
    s4 <- gsub('//+', '/', s4)
    s4 <- gsub('/$', '', s4)
    s4 <- gsub('\\*/', '*', s4)
    s5 <- gsub('^([*]*)', '\\1 ', s4)
    ## s5 <- gsub('^\\**[[:blank:]]*$', '', s5)
    s5 <- ifelse(finfos$isdir, paste0(s5, '/'), s5)
    if (details) {
        s6 <- sprintf('%s \t/%s/', s5, infos)
    } else  s6 <- s5
    desc <- c(sprintf('Created: %s', Sys.time()),
             sprintf('Base: %s', sQuote(normalizePath(fold))),
             sprintf('From computer: %s', sQuote(system('hostname', intern=T))))
    s6 <- c(paste(desc, collapse='\n\n'), '', s6)
    strout <- c(s6, "* org conf", "#+STARTUP:\tindent", "#+STARTUP:\tshowstars", "#+STARTUP:\tshowall",
               "#+INFOJS_OPT:\tview:info toc:true view:content tdepth:2",
               ## "#+SETUPFILE:\t~/.emacs.d/github_projects/org-html-themes/setup/theme-readtheorg.setup",
               sprintf("#+TITLE:\t%s", title),
               sprintf("#+DATE:\t%s", Sys.Date()),
               sprintf("#+DESCRIPTION:\t%s", paste(desc, collapse = ' - ')))
    writeLines(strout, outfile, sep = '\n')
    cat('File list written to', normalizePath(outfile), '\n')
    if (open)
        system(sprintf('emacsclient -n %s', outfile), intern = F, wait = F)
}



replicateFileStruct <- function(fold = '.', outdir = '~/test', overwrite = F, remove.base = T, 
                        ignore = c('dead\\.letter', 'ed_music', 'fin_music', 'Podcasts')) {
###  Replicates a folder stucture (recursively) by creating dummies
    op <- options('useFancyQuotes')
    options('useFancyQuotes'=F)
    library(data.table)
    if (!file.exists(outdir))
        dir.create(outdir, recursive = T)
    ## Fetch file list
    fs <- normalizePath(dir(fold, recursive = T, full.names = T, include.dirs = T))
    finfos <- file.info(fs)
    ## Ignore files
    toign <- NULL
    for (ign in ignore) {
        toign <- c(toign, grep(ign, fs))
    }
    fs <- fs[!((1:length(fs)) %in% toign)]
    finfos <- finfos[!((1:nrow(finfos)) %in% toign), ]
    if (remove.base) {
        fs <- gsub(normalizePath(fold), '', fs)
        fs <- gsub('^/', '', fs)
    }
    fs <- paste0(outdir, '/', fs)

    ## Create directories
    dirs <- fs[finfos$isdir == T]
    dir=dirs[1]
    for (dir in dirs) {
        dir.create(dir, recursive = T, showWarnings = F)
    }
    ## Remove folders from file list
    fs <- fs[!finfos$isdir]
    ## Create files
    for (f in fs) {
        if (!file.exists(f))
            r <- file.create(f, showWarnings = F)
    }
    options('useFancyQuotes'=op)
}


## Display occurrences of a term in a graph (using tags), with tooltip about context in code
## TODO: open file at correct line using "emacsclient +4 <file>" when clicked on occurrence to open at line 4.
plottag <- function(word = 'observer_species', tagfile='/slow/yvan/dragonfly/oreo/tags', agent = 'dot', tooltip_len = 10) {
    op <- options('useFancyQuotes')
    options('useFancyQuotes'=F)
    tags <- read.table(tagfile, sep ='\t', as.is=T)
    names(tags) <- c('tag', 'file', 'pos')
    t <- tags[tags$tag == word, ]
    dot <- c('graph {',
            'ratio=auto;', 'ranksep = 0.2;', 'rankdir = LR;',
            sprintf('node [fontsize=16, height=.3, style="rounded,filled", fillcolor="#FFAAAA", shape=rectangle] %s', dQuote(word)))
    conns <- NULL
    files <- unique(t$file)
    files2 <- paste0(dirname(tagfile), '/', files)
    for (fi in 1:length(files)) {
        cat(fi,'\n')
        f <- files[fi]
        t1 <- t[t$file == f,]
        filecontent <- readLines(files2[fi])
        fc <- filecontent[!grepl('^[[:blank:]]*--', filecontent)]
        t1$valid <- !grepl('^[[:blank:]]*--', filecontent[as.numeric(t1$pos)])
        creations <- which(grepl('\\bcreate.* as', fc, ignore.case = T) &
                          !grepl('^[[:blank:]]+--', fc, ignore.case = T))
        creat <- gsub(' +as', '', gsub('^[[:blank:]]*create +', '', fc[creations], ignore.case = T),
                     ignore.case=T)
        t1$clocs <- sapply(as.numeric(t1$pos), function(l) {
            max(creations[creations <= l])
        }, simplify=T)
        t1$labs <- sprintf('%s - l.%s',
                          gsub(' +as', '', gsub('^[[:blank:]]*create +', '', fc[t1$clocs], ignore.case = T),
                               ignore.case=T), t1$pos)
        
        if (any(valid)) {

            dot <- c(dot, 
                    sprintf('subgraph cluster%1$i {
label=%2$s; style="rounded,filled"; color=gray50; fillcolor=%3$s; fontcolor=black; fontsize=20;',
fi, dQuote(f), 'gray90'))
            
            t1$tooltip <- NA
            for (i in which(t1$valid)) {
                l <- as.numeric(t1$pos[i])
                t1$tooltip[i] <- gsub('"', '\'',
                                     paste(gsub('^[[:blank:]]+', '&#9;&#9;&#9;&#9;',
                                                filecontent[max(1, l-tooltip_len):min(length(filecontent),
                                                                                      l+tooltip_len)]),
                                           collapse='&#10;'))
                conns <- c(conns, sprintf('%s -- %s;', dQuote(word),
                                         dQuote(t1$labs[i])))
                dot <- c(dot, sprintf('node [style=filled tooltip="%s" URL="%s"] %s;',
                                     t1$tooltip[i], files2[fi], dQuote(t1$labs[i])))
            }
            dot <- c(dot, '}')
        }
    }

    dot <- c(dot, conns, '}')
    tmpfile <- tempfile(fileext='.dot')
    outfile <- tempfile(fileext='.svg')
    writeLines(dot, tmpfile)
    system(sprintf('%s -Tsvg -o %s %s', agent, outfile, tmpfile))
    system(sprintf('google-chrome %s', outfile))
    ## system(sprintf('emacsclient -n %s', tmpfile), wait = F)
    options('useFancyQuotes'=op)

    
}


###  Find examples of "topic" on GitHub
## Adapted from http://rud.is/b/2015/06/29/get-by-with-a-little-r-help-from-your-friends-at-github/
##  so that it can be used not only for R but any extension also
ghelp <- function(topic, ext='R', in_cran=ifelse(ext=='R', TRUE, FALSE)) {
 
  require(htmltools) # for getting HTML to the viewer
  require(rvest)     # for scraping & munging HTML
 
  # github search URL base
  base_ext_url <- sprintf("https://github.com/search?utf8=%%%%E2%%%%9C%%%%93&q=%%s+extension%%%%3A%s", ext)
  ext_url <- sprintf(base_ext_url, topic)
 
  # if searching with user:cran (the default) add that to the URL  
  if (in_cran) ext_url <- paste(ext_url, "+user%3Acran", sep="", collapse="")
 
  # at the time of writing, "rvest" and "xml2" are undergoing some changes, so
  # accommodate those of us who are on the bleeding edge of the hadleyverse
  # either way, we are just extracting out the results <div> for viewing in 
  # the viewer pane (it works in plain ol' R, too)
  if (packageVersion("rvest") < "0.2.0.9000") { 
    require(XML)
    pg <- html(ext_url)
    res_div <- paste(capture.output(html_node(pg, "div#code_search_results")), collapse="")
  } else {
    require(xml2)
    pg <- read_html(ext_url)
    res_div <- as.character(html_nodes(pg, "div#code_search_results"))
  }
 
  # clean up the HTML a bit   
  res_div <- gsub('How are these search results\\? <a href="/contact">Tell us!</a>', '', res_div)
  # include a link to the results at the top of the viewer
  res_div <- gsub('href="/', 'href="http://github.com/', res_div)
  # build the viewer page, getting CSS from github-proper and hiding some cruft
  for_view <- sprintf('<html><head><link crossorigin="anonymous" href="https://assets-cdn.github.com/assets/github/index-4157068649cead58a7dd42dc9c0f2dc5b01bcc77921bc077b357e48be23aa237.css" media="all" rel="stylesheet" /><style>body{padding:20px}</style></head><body><a href="%s">Show on GitHub</a><hr noshade size=1/>%s</body></html>', ext_url, res_div)
  # this makes it show in the viewer (or browser if you're using plain R)
  html_print(HTML(for_view))
 
}


webtab <- function(d, rownames = T, fontsize = '10px', rounding = 5, serverside=F) {
    dd <- copy(d)
    for (x in names(dd)) {
        if (is.numeric(dd[[x]])) {
            dd[, eval(x) := round(get(x), rounding)]
        }
    }
    library(DT)
    ## headstyle <- '<span style="color:DarkSlateBlue;font-size:small">%s</span>'
    ## rowstyle <- '<span style="color:DarkSlateBlue;font-size:small">%s</span>'
    dd <- as.data.frame(dd)
    ## if ('data.table' %in% class(dd)) {
    ##     dd <- copy(dd)
    ##     setnames(dd, names(dd), sprintf(style, names(dd)))
    ## } else
    ## names(dd) <- sprintf(headstyle, names(dd))
    ## rownames(dd) <- sprintf(rowstyle, rownames(dd))
    datatable(dd, escape = F,
              extensions = c('Buttons'),
              options = list(autoWidth = FALSE, paging = FALSE,
                             searching = TRUE, info   = TRUE,
                             dom = 'Bfrtip', buttons = I('colvis')),
              rownames = rownames,
              filter = 'top',
              class = 'stripe order-column hover compact'
              ) # %>% formatStyle(1:ncol(dd), `font-size` = fontsize)
}


## Keep only changing part of a vector of strings
simplify_str <- function(x) {
    y <- strsplit(x, '')
    maxchar <- max(unlist(lapply(y, function(z) length(z))))
    y <- do.call("rbind", lapply(y, function(z) c(z, rep(NA, maxchar - length(z)))) )
    keep <- apply(y, 2, function(z) length(unique(z)) > 1)
    good <- apply(y[, keep, drop=F], 1, function(z) paste(na.omit(z), collapse=''))
    removed <- unique(apply(y[, !keep, drop=F], 1, function(z) paste(na.omit(z), collapse='')))
    attr(good, 'removed') <- removed
    return(good)
}


plotDensity <- function(x, normalise=F, col='#44004444', border='#440044AA', add=F, ...) {
    d <- density(x)
    if (normalise)
        d$y <- d$y / max(d$y)
    if (add == F) {
        plot(NA, xlim = range(d$x), ylim = c(0, max(d$y)*1.04), yaxs = 'i', ...)
    }
    polygon(x = c(d$x, tail(d$x,1), d$x[1]),
            y = c(d$y, 0, 0),
            col = col, border = border)
}


taxonomytree <- function(spp, clean.names = T, verbose = T, debug = F) {
    ## Returns a data table with the same number of rows as the length of spp
    ## with each row showing the tree of the species (e.g. kingdom, class, order, family, species)
    library(taxize)
    library(data.table)

    if (clean.names) {
        sppp <- gsub('_', ' ', as.character(spp))
        sppp <- sub('\\bsubsp.*$', '', sppp) ## Remove subspecies part
        sppp <- gsub('[0-9]', '', sppp) ## Remove digits
        sppp <- gsub('\\(.*\\)', '', sppp) ## Remove things in parentheses (often the authority ref)
        sppp <- gsub('\\bsp\\.', '', sppp) ## Remove " sp. "
        sppp <- gsub('\\b[a-zA-Z]\\b', '', sppp) ## Remove single letters
        sppp <- gsub('^[[:blank:]]+|[[:blank:]]+$', '', sppp)  ## Trim spaces
    } else sppp <- spp

    if (verbose)  cat('Getting classification...\n')
    cl_ncbi <- classification(sppp, db='ncbi')

    if (debug)
        str(cl_ncbi)
    
    ## Complete with ITIS db
    if (verbose)  cat('Completing with ITIS db...\n')
    for (c in sppp[sapply(cl_ncbi, class) != 'data.frame']) {
        cat(c, '\n')
        cl_ncbi[[c]] <- classification(c, db='itis')[[1]]
    }

    if (verbose)  cat('Re-organising results...\n')
    l <- lapply(cl_ncbi, function(x) {
        if (class(x) == 'data.frame') {
            y <- x[x$rank != 'no rank',]
            dt <- as.data.table(t(y$name))
            setnames(dt, names(dt), tolower(y$rank))
        } else {
            dt <- data.table(kingdom = NA, order = NA, family = NA, genus = NA, species = NA)
        }
        return(dt)
    })
    l2 <- rbindlist(l, fill=T)

    ## Clean up
    pnas <- sapply(names(l2), function(x) sum(is.na(l2[[x]])/nrow(l2)))
    cols2keep <- names(pnas[pnas < 0.2])

    l3 <- l2[, rev(cols2keep), with=F]
    if (nrow(l3)) {
        l4 <- data.table(orig_spp = spp, l3)

        if (verbose & any(is.na(l4$class)))
            warning('Some species may not have any classification: ',
                    paste(l4$orig_spp[is.na(l4$class)], collapse=', '), '\n')

        return(l4)
    } else {
        return(NULL)
    }
}


shinyCorr <- function(df, alpha = 0.5, gradient.cols = gplots::rich.colors(50), ...) {
    ## Shiny app for interactive scatterplots of a data frame
    require(shiny)
    require(ggplot2)
    shinyApp(
        
        ui = fluidPage(
                 sidebarLayout(
                     sidebarPanel(width = 3,
                                  uiOutput("xvar"),
                                  uiOutput("xtrans"),
                                  hr(),
                                  uiOutput("yvar"),
                                  uiOutput("ytrans"),
                                  hr(),
                                  uiOutput("groupvar"),
                                  hr(),
                                  uiOutput("panelvar"),
                                  uiOutput("panelscale")),
                     mainPanel(width = 9,
                               plotOutput("theplot", height = '800px'))
                 )
             ), 

        server = function(input, output) {

            output$xvar <- renderUI({
                selectInput("xvar", label = "X variable", choices = names(df),
                            selected = names(df)[1])
            })
            output$xtrans <- renderUI({
                selectInput("xtrans", label = "X transformation", choices = c('None', 'log10', 'sqrt'),
                            selected = 'None')
            })
            output$yvar <- renderUI({
                selectInput("yvar", label = "Y variable", choices = names(df),
                            selected = names(df)[2])
            })
            output$ytrans <- renderUI({
                selectInput("ytrans", label = "Y transformation", choices = c('None', 'log10', 'sqrt'),
                            selected = 'None')
            })
            output$groupvar <- renderUI({
                selectInput("groupvar", label = "Colour-by variable", choices = c(names(df), '<None>'),
                            selected = '<None>')
            })
            output$panelvar <- renderUI({
                selectInput("panelvar", label = "Panel variable", choices = c(names(df), '<None>'),
                            selected = '<None>')
            })
            output$panelscale <- renderUI({
                selectInput("panelscale", label = "Panel scaling",
                            choices = c('Free X & Y' = 'free',
                                        'Same X' = "free_y",
                                        'Same Y' = "free_x",
                                        'Same X & Y' = 'fixed'),
                            selected = 'free')
            })

            plotdata <- reactive({
                if (!is.null(input$xvar)) 
                    df$tmpX <- df[[input$xvar]]
                if (!is.null(input$yvar))
                    df$tmpY <- df[[input$yvar]]
                if (!is.null(input$groupvar))
                    if (input$groupvar != '<None>')
                        df$tmpGroup <- df[[input$groupvar]]  else  df$tmpGroup <- NA
                if (!is.null(input$panelvar))
                    if (input$panelvar != '<None>')
                        df$tmpPanel <- df[[input$panelvar]]  else  df$tmpPanel <- NA
                return(df)
            })
            
            output$theplot <- renderPlot({
                dat <- plotdata()
                if ('tmpX' %in% names(dat) & 'tmpY' %in% names(dat) & 'tmpGroup' %in% names(dat) &
                    'tmpPanel' %in% names(dat)) {
                    if (input$groupvar != '<None>') {
                        g <- ggplot(dat, aes(x = tmpX, y = tmpY, group = tmpGroup, colour = tmpGroup))
                    } else g <- ggplot(dat, aes(x = tmpX, y = tmpY))

                    g <- g + geom_point(alpha = alpha)

                    if (input$panelvar != '<None>')
                        g <- g + facet_wrap(~ tmpPanel, scales = input$panelscale)

                    if (input$xtrans == 'log10')
                        g <- g + scale_x_log10()
                    if (input$ytrans == 'log10')
                        g <- g + scale_y_log10()

                    if (input$xtrans == 'sqrt')
                        g <- g + scale_x_sqrt()
                    if (input$ytrans == 'sqrt')
                        g <- g + scale_y_sqrt()

                    if (class(dat[[input$groupvar]]) %in% c('numeric', 'integer'))
                        g <- g + scale_colour_gradientn(name = input$groupvar,
                                                       colours = gradient.cols)
                    
                    g <- g + labs(x = input$xvar, y = input$yvar)
                    
                    return(g)
                }
            }
            )
        }
    )
}


shinyMCMC <- function(mcmc, ...) {
    ## Shiny app for interactive scatterplots of a data frame
    require(shiny)
    require(ggplot2)
    require(mcmcplots)
    require(coda)
    require(DT)
    require(data.table)
    shinyApp(
########
## UI ##
########
        ui = fluidPage(
                 titlePanel("MCMC diagnostics"),
                 tabsetPanel(
                     tabPanel("Predictions",
                              sidebarLayout(
                                  sidebarPanel(width = 3,
                                               uiOutput("par")),
                                  mainPanel(width = 9,
                                            plotOutput("theplot", height = '800px'))
                              )
                              ),
                     tabPanel("Correlations",
                              plotOutput("corrplot", height = '800px')
                              ),
                     tabPanel("Posterior summaries",
                              dataTableOutput("postsumm", width = '100%')
                              )
                 )), 
############
## Server ##
############
        server = function(input, output) {

            output$par <- renderUI({
                selectInput("par", label = "Parameter", choices = varnames(mcmc),
                            selected = varnames(mcmc)[1])
            })
            
            output$theplot <- renderPlot({
                mcmcplot1(mcmc[, input$par, drop=F])
            })
            output$corrplot <- renderPlot({
                autocorr.plot(mcmc)
            })
            output$postsumm <- DT::renderDataTable({
                mcdt <- data.table(do.call('rbind', mcmc))
                mcdt[, sample := 1:nrow(mcdt)]
                mcdtn <- melt(mcdt, id.vars = 'sample')
                mcsumm <- mcdtn[, .(mean=mean(value), med=median(value),
                                   lcl=quantile(value, 0.025, names=F),
                                   ucl=quantile(value, 0.975, names=F)), variable]
                DT::datatable(mcsumm, escape = F,
                              options = list(autoWidth = FALSE, paging = TRUE,
                                             searching = TRUE, info   = TRUE,
                                             pageLength = 30, iDisplayLength = 30),
                              class = 'stripe nowrap hover compact',
                              filter = 'top',
                              rownames = T)
                })
            })
}

rollup <- function(x, j, by, level=FALSE) {
    ### SQL rollup function for data.table. Useful to add margins.
    ## x <- data.table(group=sample(letters[1:2],100,replace=TRUE),
    ##                year=sample(2010:2012,100,replace=TRUE),
    ##                month=sample(1:12,100,replace=TRUE),
    ##                v=runif(100))
    ## r <- rollup(x, .(vmean=mean(v), vsum=sum(v)), by = c("group","year","month"), level=FALSE)
    ## dcast(melt(r, measure.vars=c('vmean', 'vsum')), group + year ~ month, fun.aggregate=sum)
    library(data.table)
    stopifnot(is.data.table(x), is.character(by), length(by) >= 2L, is.logical(level))
    j = substitute(j)
    aggrs = rbindlist(c(
                lapply(1:(length(by)-1L), function(i) x[, eval(j), c(by[1:i])][, (by[-(1:i)]) := NA]), # subtotals
                list(x[, eval(j), c(by)]), # leafs aggregations
                list(x[, eval(j)][, c(by) := NA]) # grand total
            ), use.names = TRUE, fill = FALSE)
    if(level) aggrs[, c("level") := sum(sapply(.SD, is.na)), 1:nrow(aggrs), .SDcols = by]
    setcolorder(aggrs, neworder = c(by, names(aggrs)[!names(aggrs) %in% by]))
    setorderv(aggrs, cols = by, order=1L, na.last=TRUE)
    return(aggrs[])
}

corr <- function(dat, depvar, corrtype = 'spearman') {
    library(data.table)
    corrvars <- setdiff(names(dat)[sapply(names(dat), function(v) class(dat[[v]])) == 'numeric'], depvar)
    corrs <- rbindlist(lapply(corrvars, function(v) {
        if (sd(dat[[v]]) > 0) {
            corr <- cor(dat[[depvar]], dat[[v]], method = corrtype)
        } else corr <- NA
        data.table(depvar = depvar, corrvar = v, corr = corr, corrtype = corrtype)
    }))
    return(corrs[order(-abs(corr))])
}

corrplot1var <- function(dat, depvar, alpha = 0.3, psample = 1, colby = NULL) {
    library(ggplot2)
    library(data.table)
    dat <- as.data.table(dat)
    corrvars <- setdiff(names(dat)[sapply(names(dat), function(v) class(dat[[v]])) %in% c('numeric','integer')], depvar)
    nvars <- length(corrvars)
    if (!is.null(colby)) {
        c <- rbindlist(lapply(corrvars, function(v) {
            d <- data.table(depvar = depvar, y = dat[[depvar]], corrvar = v, x = dat[[v]], colby = dat[[colby]])
            return(d[sample(1:nrow(d), round(psample * nrow(d)))])
        }))
        g <- ggplot(c, aes(x = x, y = y, group = corrvar)) + 
          facet_wrap(~ corrvar, scales = 'free')
        if (!is.null(colby))
            g <- g + geom_point(aes(colour = colby), alpha = alpha)
    } else {
        cc <- rbindlist(lapply(corrvars, function(v) {
            d <- data.table(depvar = depvar, y = dat[[depvar]], corrvar = v, x = dat[[v]])
            return(d[sample(1:nrow(d), round(psample * nrow(d)))])
        }))
        g <- ggplot(cc, aes(x = x, y = y, group = corrvar)) +
            geom_smooth() +
          facet_wrap(~ corrvar, scales = 'free') +
          geom_point(alpha = alpha)
    }
    return(g)
}

rmall <- function() {
     rm(list=ls())
}


estBetaParams <- function(mu=NULL, var=NULL, alpha=NULL, beta=NULL) {
    ## ** mu & var -> alpha & beta
    if (is.null(alpha) & is.null(beta)) {
        ## Estimate alpha and beta of beta distribution from mean and variance
        ## from http://stats.stackexchange.com/questions/12232/calculating-the-parameters-of-a-beta-distribution-using-the-mean-and-variance
        alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
        beta <- alpha * (1 / mu - 1)
        return(list(alpha = alpha, beta = beta))
    } else if (is.null(mu) & is.null(var)) {
    ## ** alpha & beta -> mu & var
        mu <- alpha / (alpha + beta)
        var <- (alpha * beta) / ((alpha * beta)^2 * (alpha + beta + 1))
        return(list(mu = mu, var = var))
    } else stop('Either `alpha` and `beta` or `mu` and `var` should be NULL')
}


checkgitprojects <- function() {
    dirs <- system('ls -d ~/dragonfly/*/', intern=T)
    base <- getwd() #'~/dragonfly'
    ## d=dirs[40]
    for (d in dirs) {
        setwd(d)
        status <- suppressWarnings(system('git status', intern=T, ignore.stderr = T))
        if (!length(attr(status, 'status')) & !length(c(grep('nothing to commit', status), grep('Not a git repository', status)))) {
            cat('\n######################################################################\n')
            cat('#### ', d, '\n')
            cat('######################################################################\n')
            cat(status, sep='\n')
            cat('\n')
        }
    }
    setwd(base)
}



get_fishing_group <- function(method=NA, fishery=NA, target=NA, vessel=NA, class=NA, start_date=NA) {
    ifelse(fishery == 'FLAT', 20,     ## Flatfish trawl
    ifelse(fishery == 'INST' | (fishery %in% c('LINT', 'MIDT', 'HAKT', 'HOKT') & class == 'S'), 1, ## Inshore trawl
    ifelse(class == 'S' & fishery == 'BNSB', 4, ## Small vessel bluenose bottom longline
    ifelse(class == 'S' & fishery %in% c('HAPB', 'MINB'), 5, ## Small vessel bottom longline targetting hapuka and minor species
    ifelse(class == 'S' & fishery %in% c('LINB'), 23,    ## Small vessel bottom longline targetting ling
    ifelse(class == 'S' & fishery %in% c('SNAB'), 6, ## Small vessel snapper
    ifelse(class == 'L' & method == 'BLL', 9, ## Autoline
    ifelse((class == 'L' | (is.na(class) & fishery == 'STNS')) & method == 'SLL', 10, ## Large vessel surface longline
    ifelse(class == 'S' & method == 'SLL' & fishery != 'SWOS', 11, ## Small vessel surface longline targetting tuna and other minor species
    ifelse(class == 'S' & method == 'SLL' & fishery == 'SWOS', 22, ## Small vessel surface longline targetting swordfish
    ifelse(class == 'L' & fishery %in% c('LINT', 'MIDT', 'HAKT', 'HOKT') & (vessel %in% c(15169,12487,12906,12903,15289,20687,6620,11036,15042,15398,13006,6061,15585,11644,8591,20876,5995,6610,15014,15500,6618,6138,15039,13313,5530,5933,6154,21482) | (vessel == 12368 & as.Date(start_date) >= as.Date('2001-01-01'))), 12, ## Meal capable middle-depths trawl
    ifelse(class == 'L' & fishery %in% c('LINT', 'MIDT', 'HAKT', 'HOKT') & (vessel %in% c(3725, 15532, 8609,8601,333,6129,11138,5250,1193,1195,8800,327,13106,360,3763,9259,12600,11338,5262,5247,8700,20809,359,804) | (vessel == 6096 & as.Date(start_date) < as.Date('2007-01-01'))), 13, ## Fresher middle depths
    ifelse(class == 'L' & fishery %in% c('LINT', 'MIDT', 'HAKT', 'HOKT') & (vessel %in% c(3704, 5558, 8040, 21356, 20610,20884,20864,6984,5663,13521,6944,5458,6473,6645,5981,8804,5906,5921,5803,13009,1356,12599,6730,6489,3351,20766,5605,1282,15256,9921) | (vessel == 12368 & as.Date(start_date) < as.Date('2001-01-01')) | (vessel == 6096 & as.Date(start_date) >= as.Date('2007-01-01'))), 2, ## Freezer middle depths
    ifelse((is.na(class) | class == 'L') & fishery %in% c('LINT', 'MIDT', 'HAKT', 'HOKT'), 3, ## Unclassified middle-depths
    ifelse(fishery == 'SBWT', 15, ## Southern blue whiting
    ifelse(fishery == 'SCIT', 16, ## Scampi
    ifelse(fishery == 'MACT', 17, ## Mackerel
    ifelse(fishery == 'SQUT', 18, ## Squid
    ifelse(fishery == 'DPWT', 19, ## Deepwater
    ifelse(method == 'SN', 21, ## Set net
    0))))))))))))))))))))
}


## angle <- data$angle
## step <- data$step
## by.bins <- depth
## transformation = function(x) log(x + 1)
angle_step_dens <- function(angle, step, angle_bins=12, step_bins=20, range_step = range(step),
                    transformation = I, col.trans = function(x) log(x + 0.05), by.bins=NULL) {
   
    nbins <- length(unique(by.bins))
    nw <- ceiling(sqrt(nbins))
    nr <- ceiling(nbins / nw)
    
    if (!is.null(by.bins) & length(unique(by.bins)) > 30)  stop('Too many `by.bins` values. Try with less bins')
    
    angle <- angle + pi/2  ## rotate to have zero angle on top

    xy <- function(step, angle) {
        cbind(step * Re(exp((0+1i) * angle)), step * Im(exp((0+1i) * angle)))
    }

    allxy <- xy(transformation(step), angle)
    lims <- apply(allxy, 2, range)

    plt <- function(angle1, step1, main='') {
        smoothScatter(xy(transformation(step1), angle1),
                      colramp=function(n) rev(terrain.colors(n)), transformation = function(x) col.trans(x),
                      axes=F, postPlotHook=NULL, xlab='', ylab='', asp=1,
                      xlim = lims[,1], ylim = lims[,2])
        axis(2, transformation(seq(0, range_step[2], length.out=10)))
        text(mean(par('usr')[1:2]), par('usr')[3], expression(pi), pos=3)
        text(mean(par('usr')[1:2]), par('usr')[4], expression(0), pos=1)
        text(par('usr')[2], mean(par('usr')[3:4]), expression(-pi/2), pos=2)
        text(par('usr')[1], mean(par('usr')[3:4]), expression(pi/2), pos=4)
        points(0, 0, pch = '+')
        text(mean(par('usr')[1:2]), par('usr')[4], main, adj=c(0.5, 1.5))
    }

    oldpar <- par()
    if (!is.null(by.bins)) {
        par(mfrow = c(nr, nw), mar=c(0,0,0,0))
        for (i in 1:nbins) {
            c <- by.bins %in% unique(by.bins)[i]
            angle1 <- angle[c]
            step1 <- step[c]
            plt(angle1, step1, main = unique(by.bins)[i])
        }
    } else {
        par(mar=c(0,0,0,0))
        plt(angle, step)
    }
    ## suppressWarnings(par(oldpar))
}

pchs <- function() {
    pch <- 1:25
    x <- (pch-1) %% 5 + 1
    y <- cumsum(x == 1)
    y <- max(y) - y + 1
    x11(width = 4, height = 4, xpos = 700, ypos = 300)
    par(mar = c(0,0,0,1))
    plot(x, y, pch = pch, col="#1F78B4", bg="#99D8C9", cex = 2, bty = 'n')
    text(x, y, pch, col='black', cex = 0.9, pos = 4, offset = 0.8, xpd=T)
    xy <- locator(type='p', pch=4, col = 'red')
    dev.off()
    return()
}


read.mcmc.dt <- function(fold) {
### Read MCMC results with data.table without using coda, in normalised form
    library(data.table)
    chain.files <- sort(dir(fold, 'CODAchain', full.names=T))
    ## MC chains
    mc <- rbindlist(lapply(1:length(chain.files), function(i) {
        cat('Reading chain', i, '\n')
        v <- fread(chain.files[i])
        cbind(ch=i, idx=1:nrow(v), v)
    }))
    if (!length(index.file))  stop('Index file not found')
    ## Get index
    index.file <- sort(dir(fold, 'CODAindex', full.names=T))
    index <- fread(index.file)
    index[, variable := sub('\\[[0-9]+\\]', '', V1)]
    index[, variable := factor(variable, levels=unique(variable))]
    index[, V1 := factor(V1, levels = unique(V1))]
    inds <- tstrsplit(index[, ifelse(grepl('\\[', V1), sub('.*\\[(.*)\\].*', '\\1', V1), NA)], ',', type.convert=T, names=F)
    names(inds) <- paste0('ind', 1:length(inds))
    index <- cbind(index, as.data.table(inds))
    setkey(index, variable, V1)
    ind <- index[, .(idx = V2:V3), keyby=.(variable, V1)][index]
    setnames(ind, 'V1', 'par')
    mc <- merge(mc, ind[, -c('V2', 'V3'), with=F], by = 'idx', sort=F, all.x=T)
    setnames(mc, c('V1', 'V2'), c('iter', 'value'))    
    setorder(mc, variable, par, ch, idx)
    mc[, sample := 1:.N, par]
    return(mc)
}



find_common_seq <- function(v1, v2, size, na.rm=T, cores=6) {
    ## Find all sequences of size `size` common between vectors v1 and v2
    library(data.table)
    library(parallel)
    if (!identical(class(v1), class(v2)))
        stop('v1 and v2 are of different class')
    ids1 <- data.table(v = v1, id_ori = 1:length(v1))
    if (na.rm == T)
        ids1 <- na.omit(ids1)
    ids1[, id_new := 1:.N]
    ids2 <- data.table(v = v2, id_ori = 1:length(v2))
    if (na.rm == T)
        ids2 <- na.omit(ids2)
    ids2[, id_new := 1:.N]
    v1 <- ids1[, v]
    v2 <- ids2[, v]
    rbindlist(mclapply(1:(length(v1)-size), function(i) {
        x1 <- v1[i:(i+size-1)]
        rbindlist(lapply(1:(length(v2)-size+1), function(j) {
            x2 <- v2[j:(j+size-1)]
            if (identical(x1, x2)) {
                return(data.table(seq = paste(x1, collapse = ','),
                                  i = ids1[id_new == i, id_ori],
                                  j = ids2[id_new == j, id_ori]))
            }
        }))
    }, mc.cores=cores))
}

find_common_seq1 <- function(v, size, na.rm=T, cores=6) {
    ## Find all repeated sequences of size `size` within vector v
    library(data.table)
    ids <- data.table(v = v, id_ori = 1:length(v))
    if (na.rm == T)
        ids <- na.omit(ids)
    ids[, id_new := 1:.N]
    v <- ids[, v]
    rbindlist(mclapply(1:(length(v)-size), function(i) {
        x1 <- v[i:(i+size-1)]
        rbindlist(lapply((i+1):(length(v)-size+1), function(j) {
            if (j != i) {
                x2 <- v[j:(j+size-1)]
                if (identical(x1, x2)) {
                    return(data.table(seq = paste(x1, collapse = ','),
                                      i = ids[id_new == i, id_ori],
                                      j = ids[id_new == j, id_ori]))
                }
            }
        }))
    }, mc.cores=cores))
}

shinypal <- function(...) {
    ## Shiny app to choose colours
    require(shiny)
    library(ggplot2)
    require(data.table)
    require(shinyjs)
    ## require(rclipboard)
    require(clipr)
    bezierCurve <- function(x, y, n=10) {
        bez <- function(x, y, t) {
            outx <- 0
            outy <- 0
            n <- length(x)-1
            for (i in 0:n) {
                outx <- outx + choose(n, i)*((1-t)^(n-i))*t^i*x[i+1]
                outy <- outy + choose(n, i)*((1-t)^(n-i))*t^i*y[i+1]
            }
            return (list(x=outx, y=outy))
        }
        outx <- NULL
        outy <- NULL
        i <- 1
        for (t in seq(0, 1, length.out=n)) {
            b <- bez(x, y, t)
            outx[i] <- b$x
            outy[i] <- b$y
            i <- i+1
        }
        return (data.table(x=outx, y=outy))
    }
    shinyApp(

        ## * UI
        ui = fluidPage(
            useShinyjs(),
            ## rclipboardSetup(),
            sidebarLayout(
                sidebarPanel(width = 2
                            ,sliderInput("luminance", label = "Luminance",
                                         min = 0, max = 100, value = 75, step = 1)
                            ,sliderInput("chroma", label = "Chroma factor",
                                         min = 0, max = 150, value = 100, step = 1)
                            ,checkboxInput('fixup', 'Fixup colours', value=F)
                            ,radioButtons('mode', 'Click action', choices = c("Pick individual colours" = "choose_colors",
                                                                              "Draw Bezier curve" = "beziers"))
                            ,sliderInput("n_cols", label = "No. colours",
                                         min = 1, max = 20, value = 5, step = 1)
                            ,verbatimTextOutput('pal_string_rgb')
                            ,verbatimTextOutput('pal_string_hcl')
                            ## ,uiOutput("clip")
                            ,actionButton("clip", "Copy")
                            ,actionButton("clear_pal", "Clear palette")
                             ## ,verbatimTextOutput('test')
                             ),
                mainPanel(width = 10,
                          fluidRow(column(8, plotOutput("theplot", click = "plot_click", height = '700px')),
                                   column(2, plotOutput("palette")),
                                   column(2, plotOutput("explot")))
                          )
            )
        ),
        ## input <- list(chroma = 100, luminance = 75, fixup = F, mode = 'choose_colors')

        ## * Server
        server = function(input, output) {

            reacvals <- reactiveValues(xys = NULL,
                                      points = NULL,
                                      palette = NULL,
                                      bezierpoints = NULL,
                                      gradient = NULL)

            observeEvent(input$plot_click, {
                    reacvals$xys <<- c(reacvals$xys, list(input$plot_click[c('x', 'y')]))
                    whc <- wheel_colors()
                    pal <- rbindlist(reacvals$xys)
                    pal[, col := sapply(1:nrow(pal), function(i) {
                        whc[whc[, which.min((x - pal[i, x])^2 + abs(y - pal[i, y])^2)], z]
                    })]
                    reacvals$points <<- pal
                ## } else {
                ##     reacvals$bezierpoints <- c(reacvals$bezierpoints, list(input$plot_click[c('x', 'y')]))
                ## }
            })

            observe({
                if (input$mode != "choose_colors") {
                    if (!is.null(reacvals$points) & !is.null(input$n_cols)) {
                        bxys <- reacvals$points[, bezierCurve(x, y, input$n_cols)]
                        whc <- wheel_colors()
                        ## whc <- cbind(whc, whc[whc[, which.min((x - bxys[i, x])^2 + abs(y - bxys[i, y])^2)],
                        ##                      .(h,c,l,z)])
                        bxys <- rbindlist(lapply(1:nrow(bxys), function(i) {
                            whc[whc[, which.min((x - bxys[i, x])^2 + abs(y - bxys[i, y])^2)]]
                        }))
                        bxys[, col := z]
                        ## bxys[, col := sapply(1:nrow(bxys), function(i) {
                        ##     whc[whc[, which.min((x - bxys[i, x])^2 + abs(y - bxys[i, y])^2)], z]
                        ## })]
                        reacvals$gradient <<- bxys
                    }
                } else {
                    if (!is.null(reacvals$points)) {
                        reacvals$palette <<- reacvals$points
                    }
                }
            })
            
            observeEvent(input$clear_pal, {
                reacvals$points <<- NULL
                reacvals$gradient <<- NULL
                reacvals$xys <<- NULL
                reacvals$palette <<- NULL
            })

            observeEvent(input$mode, {
                if (input$mode != 'choose_colors') {
                    show('n_cols')
                } else hide('n_cols')

                reacvals$points <<- NULL
                reacvals$gradient <<- NULL
                reacvals$xys <<- NULL
                reacvals$palette <<- NULL
            })
            
            output$theplot <- renderPlot({
                wheel <- wheel_colors()
                if (!is.null(wheel)) {
                    par(mar = c(0,0,0,0))
                    wheel[, plot(x, y, col = z, pch = 19, bty = 'n', axes = F, xlab = NA, ylab = NA, asp=1)]
                    if (!is.null(reacvals$points)) {
                        reacvals$points[, text(x, y, col = 'black')]
                    }
                    if (!is.null(reacvals$gradient)) {
                        reacvals$gradient[, points(x, y, col = 'black', type = 'l', lwd = 1.5)]
                    }
                }
            }) #, width = 700, height = 700)

            output$palette <- renderPlot({
                if (input$mode == 'choose_colors') {
                    pal <- reacvals$palette
                } else {
                    pal <- reacvals$gradient
                }
                if (!is.null(pal)) {
                    pal[, colr := factor(col, levels = unique(col))]
                    cols <- as.character(pal$colr)
                    names(cols) <- as.character(pal$colr)
                    ggplot(pal, aes(xmin = -10, xmax = 10, ymin = -10, ymax = 10, fill = colr)) +
                        geom_rect() +
                        geom_text(x = 0, y = 0, aes(label = colr)) +
                        scale_x_continuous(expand = c(0,0)) +
                        scale_y_continuous(expand = c(0,0)) +
                        scale_fill_manual(values = cols) +
                        facet_wrap(~ colr, ncol=1) +
                        theme_void() +
                        theme(legend.position='none',
                              panel.spacing = unit(0, 'mm'),
                              strip.text = element_blank(),
                              plot.margin=unit(c(0,0,-1,-1), 'mm'))
                }
            })

            output$explot <- renderPlot({
                if (input$mode == 'choose_colors') {
                    pal <- reacvals$palette
                } else {
                    pal <- reacvals$gradient
                }
                if (!is.null(pal)) {
                    d1 <- data.table(x = rnorm(100), y = rnorm(100), z = sample(1:nrow(pal), 100, replace=T))
                    d2 <- data.table(x = unlist(sapply(1:nrow(pal), function(i) runif(1, 5, 100))))
                    d3 <- data.table(x = apply(matrix(rnorm(100 * nrow(pal)), nrow = nrow(pal)), 1, cumsum))
                    par(mfrow=c(3, 1), mar = c(0,0,0,0), bty='n')
                    d1[, plot(x, y, col = pal$col[z], pch = 19, bty='n', axes=F)]
                    d2[, barplot(x, col = pal$col, bty='n', axes=F)]
                    matplot(d3, type = "l", col = pal$col, lwd = 1.7, axes=F, bty='n', xlab=NA, ylab=NA)
                }
            })
            
            wheel_colors <- reactive({
                if (!is.null(input$chroma) & !is.null(input$luminance) & !is.null(input$fixup)) {
                    r  <- seq(0,1,length=201)
                    th <- seq(0,2*pi, length=201)
                    gg  <- data.table(expand.grid(r=r,th=th))
                    gg[, `:=`(x = r*sin(th),
                              y = r*cos(th),
                              h = 360*th/(2*pi),
                              c = input$chroma*r,
                              l = input$luminance)]
                    gg[, z := hcl(h, c, l, fixup=input$fixup)]
                    return(gg)
                }
            })

            output$pal_string_rgb <- renderText({
                txt <- NULL
                if (input$mode == 'choose_colors') {
                    if (!is.null(reacvals$palette)) {
                        txt <- sprintf('"%s"', paste(reacvals$palette$col, collapse="\",\""))
                    }
                } else {
                    if (!is.null(reacvals$gradient)) {
                        txt <- sprintf('"%s"', paste(reacvals$gradient$col, collapse="\",\""))
                    }
                }
                ## clipr::write_clip(txt)
                return(txt)
            })
            output$pal_string_hcl <- renderText({
                txt <- NULL
                if (input$mode == 'choose_colors') {
                    if (!is.null(reacvals$palette)) {
                        txt <- sprintf('"%s"', paste(reacvals$palette[, sprintf('%s,%s,%s', h,c,l)], collapse="\",\""))
                    }
                } else {
                    if (!is.null(reacvals$gradient)) {
                        txt <- sprintf('"%s"', paste(reacvals$gradient[, sprintf('hcl(h=%s,c=%s,l=%s)', h,c,l)], collapse=","))
                    }
                }
                ## clipr::write_clip(txt)
                return(txt)
            })            

            ## ## Add clipboard buttons
            ## output$clip <- renderUI({
            ##     rclipButton("clipbtn", "Copy", input$pal_string, icon("clipboard"))
            ## })
            ## Workaround for execution within RStudio
            observeEvent(input$clip,
                         clipr::write_clip(as.character(input$pal_string), object_type='character')
                         )
            
            output$test <- renderPrint({
                if (input$mode == 'choose_colors') {
                    print(reacvals$palette)
                } else {
                    print(reacvals$gradient)
                }
            })
            
        })
}


find_root_folder <- function (from = getwd()) {
    fold <- from
    atroot <- file.exists(sprintf("%s/.git", fold))
    while (!atroot & fold != "/") {
        fold <- dirname(fold)
        atroot <- file.exists(sprintf("%s/.git", fold))
    }
    if (atroot) {
        return(fold)
    } else return(character(0))
}

settoroot <- function() {
    root <- find_root_folder()
    if (length(root)) {
        setwd(root)
        cat('Working directory is now', root, '\n')
    } else {
        warning('Project root not found')
    }
}


replace_nas <- function(DT) {
    ## ** Replace NAs in numeric columns of a data.table with 0 
    for (j in seq_len(ncol(DT))) {
        if (is.numeric(DT[[j]]))
            set(DT, which(is.na(DT[[j]])), j, 0)
    }
}



## ## Function to fill in missing pixels with mean of non-missing cells in the 4-cell neighbourhood
## library(inline)
## src1 <- '
##     Rcpp::NumericMatrix Am(A);
##     int na = Rcpp::as<int>(missval);
##     NumericMatrix Bm = Am;
##     int nrows = Am.nrow();
##     int ncolumns = Am.ncol();
##     int n = 0;
##     float sum = 0;
##     for (int i = 1; i < (ncolumns-1); i++) {
##         for (int j = 1; j < (nrows-1); j++) {
##             if (Am(j,i)==na && (Am(j+1,i) != na || Am(j-1,i) != na || Am(j,i+1) != na || Am(j,i-1) != na)) {
##                n = 0;
##                sum = 0;
##                if (Am(j+1,i) != na) {
##                  n = n+1;
##                  sum = sum + Am(j+1,i);
##                }
##                if (Am(j-1,i) != na) {
##                  n = n+1;
##                  sum = sum + Am(j-1,i);
##                }
##                if (Am(j,i+1) != na) {
##                  n = n+1;
##                  sum = sum + Am(j,i+1);
##                }
##                if (Am(j,i-1) != na) {
##                  n = n+1;
##                  sum = sum + Am(j,i-1);
##                }
##                Bm(j,i) = sum/n;
##             }
##         }
##     }
##     return Bm;
## '
## fillincoastline_forward <- cxxfunction(signature(A = "numeric", missval = "integer"),
##                                       body = src1, plugin="Rcpp")

## src2 <- '
##     Rcpp::NumericMatrix Am(A);
##     int na = Rcpp::as<int>(missval);
##     NumericMatrix Bm = Am;
##     int nrows = Am.nrow();
##     int ncolumns = Am.ncol();
##     int n = 0;
##     float sum = 0;
##     for (int j = (nrows-1); j > 1; j--) {
##         for (int i = 1; i < (ncolumns-1); i++) {
##             if (Am(j,i)==na && (Am(j+1,i) != na || Am(j-1,i) != na || Am(j,i+1) != na || Am(j,i-1) != na)) {
##                n = 0;
##                sum = 0;
##                if (Am(j+1,i) != na) {
##                  n = n+1;
##                  sum = sum + Am(j+1,i);
##                }
##                if (Am(j-1,i) != na) {
##                  n = n+1;
##                  sum = sum + Am(j-1,i);
##                }
##                if (Am(j,i+1) != na) {
##                  n = n+1;
##                  sum = sum + Am(j,i+1);
##                }
##                if (Am(j,i-1) != na) {
##                  n = n+1;
##                  sum = sum + Am(j,i-1);
##                }
##                Bm(j,i) = sum/n;
##             }
##         }
##     }
##     return Bm;
## '
## fillincoastline_backward <- cxxfunction(signature(A = "numeric", missval = "integer"),
##                                        body = src2, plugin="Rcpp")


## clean_raster <- function(r, missval=-9999, naval=NA, show.progress=F) {
##     r1 <- r2 <- r
##     ## ** Forward
##     r1[r1[] %in% naval] <- missval
##     prevvals <- rep(0, length(r1[]))
##     while (!identical(prevvals, r1[])) {
##         if (show.progress) cat('.')
##         prevvals <- r1[]
##         m <- fillincoastline_forward(as.matrix(r1), missval)
##         r1 <- setValues(r1, m)
##     }
##     if (show.progress) cat('\n')
##     r1[r1[] == missval] <- naval
##     ## ** Backward
##     r2[r2[] %in% naval] <- missval
##     prevvals <- rep(0, length(r2[]))
##     while (!identical(prevvals, r2[])) {
##         if (show.progress) cat('.')
##         prevvals <- r2[]
##         m <- fillincoastline_backward(as.matrix(r2), missval)
##         r2 <- setValues(r2, m)
##     }
##     if (show.progress) cat('\n')
##     r2[r2[] == missval] <- naval
##     ## ** Mean
##     return(r1 + r2 / 2)
## }



approximate_left_join <- function(left, right, on.col = 'datetime', take='nearest', nomatch=NA,
                          id.col.left = NULL, id.col.right = NULL) {
    ## * Find nearest, closest lower, or closest higher value
    library(data.table)
    if (!is.data.table(left))  left  <- as.data.table(left)
    if (!is.data.table(right)) right <- as.data.table(right)
    ## ** Prepare
    if (!(take %in% c('nearest', 'lower', 'higher')))
        stop('`take` needs to be one of `nearest`, `lower`, `higher`')
    roll <- switch(take, lower = Inf, higher = -Inf, 'nearest')
    if (is.null(id.col.left)) {
        left[,  id.left  := seq_len(.N)]
    } else left[, id.left := get(id.col.left)]
    if (is.null(id.col.right)) {
        right[,  id.right  := seq_len(.N)]
    } else right[, id.right := get(id.col.right)]
    left[, oncol := get(on.col)]
    right[, oncol := get(on.col)]
    j <- right[left, on = 'oncol', roll = roll, nomatch=nomatch]
    ## ** Join
    j[right, value.joined := i.oncol, on = 'id.right']
    ## ** Cleanup
    left[ , id.left  := NULL]
    right[, id.right := NULL]
    j <- j[, .(id.left, oncol, id.right, value.joined)]
    setnames(j, 'id.left', ifelse(is.null(id.col.left), 'id'))
    setnames(j, 'id.right', ifelse(is.null(id.col.right), 'id.joined'))
    setnames(j, c('oncol', 'value.joined'),
             c(on.col, paste0(on.col, '_joined')))
    return(j)
}



distance2target <- function(rast, target_value, na_value=-99999, use_input_nodata = T) {
    ## * Raster: Calculate distance to cells of target value
    rast[is.na(rast)] <- na_value
    tmp <- tempfile()
    writeRaster(rast, tmp, 'GTiff')
    system(sprintf('gdal_proximity.py %1$s.tif %1$s_out.tif -values %2$s -nodata %3$f -use_input_nodata %4$s',
                   tmp, target_value, na_value, ifelse(use_input_nodata, 'YES', 'NO')))
    r <- raster(sprintf('%s_out.tif', tmp))
    r[r[] == na_value] <- NA
    return(r)
}


longest_common_substring <- function(a, b) {
    library(stringi)
    if (!is.na(a) & !is.na(b)) {
        nc <- nchar(b)
        is <- CJ(seq_len(nc), seq_len(nc))[V2 >= V1]
        sb <- stri_sub(b, is$V1, is$V2)
        sstr <- na.omit(stri_extract_all_coll(a, sb, simplify=TRUE))
        res <- sstr[which.max(nchar(sstr))]
        if (length(res)) {
            return(res)
        } else return(NA)
    } else return(NA)
}


convert_files_to_txt <- function(filepaths, mc.cores = 6) {
    conv.fun <- c(rtf  = "unrtf --text --nopict \"%s\" | tail -n+5 > \"%s\"",
                  docx = "unzip -p \"%s\" word/document.xml | sed -e 's/<\\/w:p>/\\n/g; s/<[^>]\\{1,\\}>//g; s/[^[:print:]\\n]\\{1,\\}//g' > \"%s\"",
                  pdf  = "pdftotext \"%s\" \"%s\"",
                  odt  = "unoconv -f txt --stdout \"%s\" > \"%s\"")
    ext <- tolower(sub('.*\\.([a-zA-Z0-9-]+)$', '\\1', filepaths))
    ext[ext %in% 'pdf-1'] <- 'pdf'
    outfiles <- paste0(filepaths, '.txt')
    res <- mclapply(seq_along(filepaths), function(i) {
        f <- filepaths[i]
        convf <- conv.fun[ext[i]]
        if (!is.na(convf)) {
            cmd <- sprintf(convf, f, outfiles[i])
            tryCatch(system(cmd, intern=T),
                     warning = function(w) {
                         cat('Warning with ', f, '\n')
                     },
                     error = function(e) {
                         cat('Error with ', f, '\n')
                     })
        } else {
            warning(sprintf('No converter found for %s. Skipped.', f))
        }
    }, mc.cores = mc.cores)
}


get_latex_paths <- function(dir=getwd(), extensions=c('rnw', 'tex', 'Rnw'),
                            exclude=c('diff.tex', 'cover.tex'), no.time=T) {
    cmd <- sprintf('find %s -maxdepth 1 \\( %s \\) | xargs grep -E "%s"',
                   dir,
                   paste(sprintf('-name "*.%s"', extensions), collapse=' -o '),
                   '^[^#%].*[-a-zA-Z0-9_]+/[-a-zA-Z0-9_]+')
    res <- system(cmd, intern=T)
    split <- strsplit(res, ':')
    res <- data.table(source = sapply(split, '[', 1),
                      content = sapply(split, function(x) paste(x[2:length(x)], collapse='')))
    res <- res[!(basename(source) %in% exclude)]
    res[, content := sub('.*[^-a-zA-Z0-9._/]+([-a-zA-Z0-9._/]+/[-a-zA-Z0-9._/]+)[^-a-zA-Z0-9._/]*.*', '\\1', content)]
    res[, mdate := file.mtime(content)]
    setorder(res, mdate)
    if (no.time) res[, mdate := format(mdate, '%Y-%m-%d')]
    return(res)
    
}


dfly_pal <- function(what=NULL) {
    pal <- c(blue   = '#43A1C9',
             grey   = '#565659',
             cream  = '#E5DECC',
             green  = '#50AD85',
             orange = '#EB7A59',
             red    = '#CF4547',
             purple = '#5B3456',
             mint   = '#9DC4A9')

    if (is.null(what)) {
        return(pal)
    }
    if (is.character(what)) {
        if (what %in% names(pal)) {
            return(pal[what])
        } else stop('Colour not in palette')
    } else if (is.numeric(what)) {
        return(pal[what])
    }
}

mean_colour <- function(twocols=c('#fff3e1', '#fcb045')) {
    if (length(twocols) != 2) stop('A vector of two colors is needed')
    return(colorRampPalette(twocols)(3)[2])
}


push_msg <- function(title = 'Automatic msg', body = 'Test', url = NULL,
                     devices = 0, debug=F, verbose=F) {
    library(RPushbullet)
    if (is.null(getOption("rpushbullet.key"))) {
        stop('Pushbullet key not set. Please run `pbSetup()`.')
    }
    if (is.null(url)) {
        pbPost(type='note', title=title, body=body, verbose=verbose, debug=debug,
               recipients = devices)
    } else if (grepl('//', url)) {
        pbPost(type='link', title=title, body=body, url=url, verbose=verbose, debug=debug,
               recipients = devices)
    } else {
        pbPost(type='file', url=url, verbose=verbose, debug=debug,
               recipients = devices)
    }
}


dt_to_multiline <- function(dt, coords, byvars, crs=sf::NA_crs_) {
    ## * Convert a data.table to SF lines, by group
    library(data.table)
    library(sf)
    st_as_sf(as.data.table(st_as_sf(dt, coords = coords))[
      , .(geometry = list(st_cast(do.call(c, geometry), 'LINESTRING')))
      , byvars], crs=crs)
}

st_rectangle <- function(xmin, ymin, xmax, ymax, crs=4326) {
    library(sf)
    rect <- st_polygon(list(matrix(c(xmin, ymin, xmin, ymax, xmax, ymax, xmax, ymin, xmin, ymin), ncol=2, byrow=T)))
    rect <- st_geometry(rect)
    st_crs(rect) <- crs
    return(rect)
}

getroot <- function(target='.git') {
    path <- getwd()
    atroot <- file.exists(file.path(path, target))
    i <- 0
    while(!atroot & path != '/') {
        i <- i + 1
        path <- dirname(path)
        atroot <- file.exists(file.path(path, target))
    }
    if (!atroot & path == '/') {
        warning('Root path not found')
        return(character(0))
    } else return(normalizePath(path))
}


ensuresomematching <- function(x, outfun=warning) {
    condtxt <- deparse(substitute(x))
    if (class(x) == 'logical') {
        if (!any(x)) {
            outfun(sQuote(condtxt), ' does not match any record')
        }
    } else {
        if (!length(x)) {
            outfun(sQuote(condtxt), ' does not match any record')
        }
    }
    return(x)
}

## palettes <- function(n = 10, pal.type = 'sequential') {
##     library(ggplot2)
##     library(paletteer)
##     library(data.table)
##     pals <- data.table(palettes_c_names)[type == pal.type]
    
##     rbindlist(lapply(seq_len(nrow(palettes_c_names)), function(i) {
##         pak <- pals[i, package]
##         pal <- pals[i, palette]

##         pale <- paletteer_c(eval(pak, parent.frame()), eval(pal, parent.frame()), n)
        
##         pale <- paletteer_c(eval(substitute(pak)), eval(substitute(pal)), n)

##         pal <- paletteer_c(get(pak), get(pal), n)
        
##     }
## }

paletteers <- function() {
    library(paletteer)

    allpals <- rbind(
        ## x=names(palettes_d)[1]
        rbindlist(lapply(names(palettes_d), function(x) {
            ## y=names(palettes_d[[x]])[1]
            rbindlist(lapply(names(palettes_d[[x]]), function(y) {
                data.table(source = 'palettes_d', pkg = x, pal = y, n = 0, col = palettes_d[[x]][[y]])
            }))
        })),
        ## x=names(palettes_dynamic)[1]
        rbindlist(lapply(names(palettes_dynamic), function(x) {
            ## y=names(palettes_dynamic[[x]])[1]
            rbindlist(lapply(names(palettes_dynamic[[x]]), function(y) {
                ## z=5
                rbindlist(lapply(seq_along(palettes_dynamic[[x]][[y]]), function(z) {
                    data.table(source = 'palettes_dynamic', pkg = x, pal = y, n = z, col = palettes_dynamic[[x]][[y]][[z]])
                }))
            }))
        }))
    )
    allpals[, lab := sprintf('%s | %s | %s | %s', source, pkg, pal, n)]
    allpals[, lab := factor(lab, levels = unique(lab))]

    allpals[, y := as.numeric(lab)]
    allpals[, x := as.numeric(rowid(lab))]
    allpals[, xtop := max(x), lab]
    allpals[, xmin := scales::rescale(x, to = c(0, 1), from = c(1, xtop+1)), by = 1:nrow(allpals)]
    allpals[, xmax := scales::rescale(x+1, to = c(0, 1), from = c(1, xtop+1)), by = 1:nrow(allpals)]
    allpals[, x := 0]

    offset <- 1.3
    allpals[y >= max(y)/2, `:=`(xmin = xmin + offset, xmax = xmax + offset, x = x + offset, y = y - max(y)/2 + 1)]

    g <- ggplot(allpals, aes(x = x, xmin = xmin, xmax = xmax, y = y, ymin = y - 0.4, ymax = y + 0.4, fill = col,
                        label = lab)) +
        geom_rect() +
        geom_text(hjust = 1, size = 0.6) +
        scale_fill_identity() +
        scale_x_continuous(expand = c(0.1, 0.1)) +
        theme_void()
    
    tmpfile <- tempfile(fileext='.pdf')
    ggsave(tmpfile, g, width = 7, height = 50, limitsize = F)
    system(sprintf('xdg-open %s', tmpfile))

}


make_0_360 <- function(polyg, split_lon = 0, namefield='code') {
    library(sf)

    shp           <- tempfile(fileext='.shp')
    shp_p1        <- tempfile(fileext='.shp')
    shp_p2        <- tempfile(fileext='.shp')
    shp_p1_s      <- tempfile(fileext='.shp')
    shp_out_raw   <- tempfile(fileext='.shp')
    shp_out_split <- tempfile(fileext='.shp')
    shp_out       <- tempfile(fileext='.shp')

    write_sf(polyg, shp)

    cmd <- sprintf(
        '
ogr2ogr %2$s %1$s -clipsrc -180 -90 %11$f 90
ogr2ogr %3$s %1$s -clipsrc %11$f -90 180 90
ogr2ogr %4$s %2$s -dialect sqlite -sql "SELECT ShiftCoords(geometry,360,0), %12$s FROM %5$s"
ogr2ogr %6$s %3$s
ogr2ogr -update -append %6$s %4$s -nln %7$s
ogr2ogr %8$s %6$s -dialect sqlite -sql "SELECT ST_Union(Geometry), %12$s FROM \"%7$s\" GROUP BY %12$s"
ogr2ogr %9$s %8$s -dialect sqlite -sql "SELECT %12$s, ST_Union(ST_Buffer(Geometry, 0.000001)) as geometry FROM \"%10$s\" GROUP BY %12$s"
',
shp,  shp_p1,  shp_p2,  shp_p1_s,  sub('\\.shp', '', basename(shp_p1)),
shp_out_raw,  sub('\\.shp', '', basename(shp_out_raw)),
shp_out_split, shp_out, sub('\\.shp', '', basename(shp_out_split)),
split_lon, namefield
)

    system(cmd)
    
    polyg_out <- read_sf(shp_out)

    return(polyg_out)

}

namap <- function(dt, alpha.trans='sqrt') {
    namap <- as.data.table(lapply(dt, is.na))
    namap <- namap[, .N, names(namap)][order(-N)]
    na_n <- namap$N
    namap[, row := seq_len(.N)]
    namap <- melt(namap, id.vars = c('row', 'N'), measure.vars = setdiff(names(namap), c('row', 'N')))
    namap[, h := log(N+1)]
    namapfile <- file.path(tmpfold, 'na-map.png')
    g <- ggplot(namap, aes(x = variable, y = row, fill=value)) +
        geom_tile(aes(alpha = N), size=0.1, colour = 'white') +
        scale_fill_manual(name = 'is NA?', values=c('TRUE' = "#E41A1C", 'FALSE' = "#A6CEE3")) +
        scale_y_continuous(breaks = 1:max(namap$row), labels = na_n, expand=c(0,0), trans='reverse') +
        scale_x_discrete(position='top') +
        scale_alpha_continuous(trans = alpha.trans, guide = 'none') +
        theme(axis.text.x.top   = element_text(angle = 90, hjust = 0, vjust = 0.5),
              panel.grid   = element_blank()) +
        labs(x = NULL, y = 'No. rows')
    return(g)
}


datareport <- function(dt, tmpfold=tempdir(), rmdfile=file.path(tmpfold, 'data-summary.Rmd'),
                       htmlfile=file.path(tmpfold, 'data-summary.html'), ignore.cols = character(0)) {

    library(data.table)
    library(knitr)
    library(ggplot2)

    n <- nrow(dt)

    rmd <- sprintf('---
title: Summary of %s
author: datareport()
date: "%s"
output:
  rmdformats::readthedown:
    mathjax: null
    use_bookdown: false
    lightbox: true
    thumbnails: false
    css: ~/Dropbox/templates/report-html/custom.css
    gallery: true
    toc_depth: 3
    toc_float:
      collapsed: false
      smooth_scoll: true
mode: selfcontained
---
\n', as.character(substitute(dt)), Sys.time())
    cat(rmd, file=rmdfile)

    cat('# General summary\n', file = rmdfile, append=T)

    cat(n, 'rows\n\n', file = rmdfile, append=T)

    cat(kable(head(dt, 5), format='html', caption = 'First 5 rows', padding=1), file = rmdfile, append=T)
    cat(kable(tail(dt, 5), format='html', caption =  'Last 5 rows', padding=1), file = rmdfile, append=T)

    cat('# Map of missing values\n', file = rmdfile, append=T)
    
    g <- namap(dt)
    ggsave(namapfile, g, width = 9, height = 6)
    cat(sprintf('<td><img src="%1$s" alt="%1$s" height="600" width="900"></td></tr>\n', namapfile), file = rmdfile, append=T)

    cat('# Fields summary\n', file = rmdfile, append=T)

    colno <- 1
    for (z in names(dt)) {
        
        if (!(z %in% ignore.cols)) {
            cat(z, '\n')
            
            cat('\n##', colno, '-', z, '\n\n', file = rmdfile, append=T)

            dt[, zevalue := get(z)]
            
            x <- dt[[z]]

            cat('<table><tr><td><table><tr><td>\n', file = rmdfile, append=T)
            
            un <- uniqueN(x)
            pun <- 100 * un/n
            nas <- sum(is.na(x))
            pnas <- 100 * nas/n
            
            cat(sprintf('Class: `%s`\n\nUnique values: `%i` (%0.2f%%)\n\nNAs: `%i` (%0.2f%%)\n\n',
                        class(x), un, pun, nas, pnas),
                file = rmdfile, append=T)
            cat('</td></tr>', file=rmdfile, append=T)


            zz <- gsub('[^a-zA-Z0-9_-]', '', gsub('[[:blank:]]+', '_', tolower(z)))
            
            densfile <- file.path(tmpfold, sprintf('density-%s.png', zz))
            
            if (is.numeric(x)) {

                cat('<tr><td>', file=rmdfile, append=T)
                
                ## ** Summary of values
                t <- kable(data.table(min = min(x, na.rm=T), lcl = quantile(x, 0.025, na.rm=T, names=F),
                                      mean = mean(x, na.rm=T), med = median(x, na.rm=T),
                                      ucl = quantile(x, 0.975, na.rm=T, names = F), max = max(x, na.rm=T)),
                           format = 'html')
                cat(t, file = rmdfile, append=T)
                cat('</td></tr></table></td>', file=rmdfile, append=T)
                
                ## ** Density plot
                g <- ggplot(dt, aes(zevalue)) +
                    geom_density(fill = "#43A1C9AA", colour = "#225165") +
                    scale_x_continuous(expand = c(0,0)) +
                    scale_y_continuous(expand = c(0,0)) +
                    labs(x = z)
                ggsave(densfile, g, width = 6, height = 4)
                cat(sprintf('<td><img src="%1$s" alt="%1$s" height="400" width="700"></td></tr>\n', densfile), file = rmdfile, append=T)
                
            } else if (is.factor(x) | is.character(x)) {

                if (is.factor(x)) {
                    y <- as.character(x)
                } else y <- x

                suppressWarnings(ynum <- as.numeric(y))
                couldbenum <- ifelse(all(is.na(ynum)) | any(is.na(ynum) & !is.na(y)), F, T)
                
                cat(sprintf('<tr><td>Could be numeric: `%s`</td></tr>', couldbenum),
                    file = rmdfile, append=T)
                
                cat('<tr><td>', file=rmdfile, append=T)
                
                ## ** Summary of the number of characters
                t <- kable(data.table(min = min(nchar(y), na.rm=T), 
                                      mean = mean(nchar(y), na.rm=T), med = median(nchar(y), na.rm=T),
                                      max = max(nchar(y), na.rm=T)),
                           caption = 'Number of characters', format = 'html')
                cat(t, file = rmdfile, append=T)
                cat('</td></tr>', file=rmdfile, append=T)
                
                if (couldbenum & !is.factor(x)) {
                    dt[, zevalue := round(as.numeric(zevalue), 7)]
                    dt[, zevalue := factor(zevalue)]
                }

                if (is.factor(x) | pun < 0.05 | un <= 20) {

                    cat('<tr><td>', file=rmdfile, append=T)
                    
                    ## ** Table of values
                    t <- kable(dt[, .N, zevalue][order(-N)], col.names = c(z, 'N'), format = 'html',
                               caption = 'Frequency of all values')
                    cat(t, file = rmdfile, append=T)
                    cat('</td></tr></table></td>', file=rmdfile, append=T)
                    
                    ## ** Bar plot
                    g <- ggplot(dt, aes(zevalue)) +
                        geom_bar(fill = "#43A1C9AA", colour = "#225165") +
                        labs(x = z)
                    ggsave(densfile, g, width = 6, height = 4)
                    cat(sprintf('<td><img src="%1$s" alt="%1$s" height="400" width="700"></td></tr>\n', densfile),
                        file = rmdfile, append=T)
                    
                } else {
                    cat('</table></td><td>', file=rmdfile, append=T)
                    ## ** 15 Most common values
                    t <- kable(dt[, .N, zevalue][order(-N)][1:pmin(.N, 15)], col.names = c(z, 'N'), format = 'html',
                               caption = '15 most common values')
                    cat(t, file = rmdfile, append=T)
                    cat('</td><td>', file=rmdfile, append=T)
                    ## ** 15 Least common values
                    t <- kable(dt[, .N, zevalue][order(N)][1:pmin(.N, 15)], col.names = c(z, 'N'), format = 'html',
                               caption = '15 least common values')
                    cat(t, file = rmdfile, append=T)
                    cat('</td></tr>', file=rmdfile, append=T)
                }
                
            } else  {
                
            }
            cat('</table>\n', file = rmdfile, append=T)

        }
        
        colno <- colno + 1
    }

    ## ** Correlations
    numcols <- sapply(dt, is.numeric)
    numcols <- names(numcols)[numcols==T]
    if (length(numcols) > 1) {
        
    }
    
    rmarkdown::render(input=rmdfile, output_file = htmlfile, clean=T)
    system(sprintf('xdg-open %s', htmlfile), wait=F)
    dt[, zevalue := NULL]
    ## cat(readLines(rmdfile), sep='\n')
    return(htmlfile)
}


make_filename <- function(x) {
    gsub('[^-_a-zA-Z0-9]', '', gsub('[[:blank:]]+', '_', x))
}

check_txt_num <- function(d, details=F, maxn = 10) {
    
    check1 <- function(x, maxn=10) {

        if (details==T & is.numeric(x))  cat('ALREADY NUMERIC!\n')
        xnum <- suppressWarnings(as.numeric(as.character(x)))
        xint <- as.integer(xnum)
        couldbenum <- !(is.na(xnum) & !is.na(x))
        couldbeint <- couldbenum & (is.na(xnum) | as.numeric(xint) == xnum)
        ngoodnum <- sum(couldbenum, na.rm=T)
        ngoodint <- sum(couldbeint, na.rm=T)
        n <- length(x)
        numbutnotint <- !is.na(couldbeint) & !couldbeint & couldbenum
        notgood <- sort(table(x[!couldbenum]), decr=T)
        notint  <- sort(table(x[numbutnotint]), decr=T)
        if (details) {
            cat(sprintf('%i values (%0.2f%%) can be converted to numeric\n',
                        ngoodnum, 100*ngoodnum/n))
            cat(sprintf('%i values (%0.2f%%) can be converted to integer\n\n',
                        ngoodint, 100*ngoodint/n))
            if (any(!couldbenum)) {
                if (details) {
                    cat(sprintf('%i values (%0.2f%%) cannot be converted to numeric\n',
                                n-ngoodnum, 100*(n-ngoodnum)/n))
                }
                if (length(notgood) <= 20) {
                    cat('\n* All values not convertible to numeric:\n\n')
                    print(notgood)
                } else {
                    cat(sprintf('\n* First %i most common values not convertible to numeric:\n', maxn))
                    print(head(notgood, maxn))
                }
            }
            if (any(numbutnotint)) {
                if (length(notint) <= 20) {
                    cat('\n* All numericable values not convertible to integer:\n\n')
                    print(notint)
                } else {
                    cat(sprintf('\n* First %i most common numericable values not convertible to integer:\n', maxn))
                    print(head(notint, maxn))
                }
            }
        }
        exnotnum <- names(notgood)[1]
        if (length(exnotnum) == 0 | is.na(c(exnotnum, NA))[1]) exnotnum <- ''
        if (nchar(exnotnum) > 15) exnotnum <- paste0(substr(exnotnum, 1, 15), '...')
        if (any(couldbenum)) {
            exnotint <- names(notint)[1]
            if (length(exnotint) == 0 | is.na(c(exnotint, NA))[1]) exnotint <- ''
            if (nchar(exnotint) > 8) exnotint <- paste0(substr(exnotint, 1, 8), '...')
        } else exnotint <- ''
        return(data.table(perc_na = round(100*mean(is.na(x)),1),
                          isnum = all(couldbenum), isint = all(couldbeint), exnotnum = exnotnum, exnotint = exnotint))
    }

    if ('data.frame' %in% class(d)) {
        return(rbindlist(lapply(names(d), function(m) {
            if (details) cat('\n\n===', m, '\n\n')
            cbind(column=m, class=paste(class(d[[m]]), collapse=','), check1(d[[m]]))
        }), fill=T))
    } else check1(d)
}
