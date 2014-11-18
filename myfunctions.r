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
    if (warn)
        {
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
    id <- apply(df[,cols], 1, paste, collapse='_')
    isdup <- duplicated(df[,cols])
    d <- df[id %in% id[isdup], ]
    id <- apply(d[, cols], 1, paste, collapse = "_")
    return(d[order(id),])
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

upper1st <- function(x) {
    x1 <- strsplit(x, '')
    x <- sapply(x1, function(x) {
        x[1] <- toupper(x[1]) 
        return(paste(x, collapse=''))
    })
    return(x)
}

## from help of chartr()
capwords <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s,1,1)),
                             {s <- substring(s,2); if(strict) tolower(s) else s},
                             sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

## Returns string of comma-separated values, with quotes (or not)
Pprint <- function(x, sep=',', use.quotes=T) {
    if (use.quotes)
        cat(paste(sprintf('\'%s\'', x), collapse=sep),'\n') else
    cat(paste(sprintf('%s', x), collapse=sep),'\n')
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
    if (grad=='exp')
        {
            alphas <- exp(seq(curv,1,length.out=n))
            alphas <- (alphas-min(alphas))/max(alphas-min(alphas))
        } else 
            if (grad=='log')
                {
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
    if (grad=='exp')
        {
            alphas <- exp(seq(curv,1,length.out=n))
        } else 
            if (grad=='log')
                {
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
    if (is.factor(var) | is.character(var))
        {
            if (!is.factor(var))  var <- factor(var, levels=unique(var))
            colors <- rep(colors, length.out=nlevels(var))
            names(colors) <- levels(var)
            Cols <- colors[match(var, names(colors))]
        } else
            {
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
    x = 1:length(lcl)
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
    for (di in 1:nspp)                  # di=nspp
        {
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
    for (pos in poss)
        {
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
        cols <- merge(xy, d, all.x=T, all.y=F, by.x='xy', by.y='xy')$col
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
    if (!(all(class(df) == 'SpatialPointsDataFrame')))
        { ## Transform df to spatial object
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
    return(exp(x)/(1+exp(x)))
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
localc <- function(df, row.names=T, newl.at=100, basename='temp', ...) {
    f <- makeuniquefilename(sprintf('/tmp/%s.csv',basename))
    if (!is.na(newl.at)) { ## insert return line when field is too long
        nch = apply(df,2,function(x) max(nchar(as.character(x))))
        toolongcols = names(nch[nch>newl.at])
        for (c in toolongcols) {
            df[,c] <- as.character(df[,c])
            toolongvals = nchar(df[[c]]) > newl.at
            df[toolongvals,c] <- sapply(df[toolongvals, c], function(x) {
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
    write.csv(as.data.frame(df), f, row.names=row.names, ...)
    res <- system(sprintf('localc %s', f), wait=F)
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


## "%nin%" <- function(x,y) !(x %in% y)

## Check labels in LaTeX report that are not cited in text
## reportfile='~/dragonfly/sra-foundations/report/notes/report.tex'
check_cited_labels <- function(reportfile, ignore=c('sec','eq','app')) {
    if (!grepl('^/|^~', reportfile))  reportfile <- sprintf('%s/%s',getwd(),reportfile)
    txt <- readLines(reportfile)
    comments <- grepl('^ *%', txt)
    txt <- txt[!comments]
    inputs <- grep('\\\\input\\{', txt, value=T)
    inputs <- gsub('^ *\\\\input\\{(.*)\\}', '\\1', inputs)
    alltext <- NULL
    alllabels <- NULL
    f=inputs[4]
    for (f in inputs) {
        file <- f
        if (!grepl('^/|^~', file))  file <- sprintf('%s/%s',getwd(), file)
        file <- sprintf('%s.tex', file)
        txt <- readLines(file)
        comments <- grepl('^ *%', txt) | txt==''
        txt <- txt[!comments]
        alltext[[f]] <- txt
        labels0 <- grep('\\\\label\\{',txt)
        labels <- gsub('^ *\\\\label\\{(.*)\\}', '\\1', txt[labels0])
        c <- sapply(strsplit(labels,':'), function(x) x[1]) %in% ignore
        labels <- labels[!c]
        alllabels[[f]] <- labels
    }
    l <- unlist(alllabels)[1]
    for (l in unlist(alllabels)) {
        wh <- grep(l, unlist(alltext), value=T)
        c <- grepl('\\\\label\\{', wh)
        cited <- wh[!c]
        if (!length(cited))
            cat(sprintf(
                sprintf('Not referenced: %%%is  in  %%s\n', max(nchar(unlist(alllabels)))),
                l, names(alltext[grep(sprintf('\\\\label\\{%s\\}', l), alltext)])))
    }
}


##makefile <- '~/dragonfly/sra-foundations/modelling/bh-dd-k50/makefile'
## makefile <- '~/dragonfly/sra-foundations/report/notes/makefile'
graph_makefile <- function(makefile='makefile', rankdir='BT', nodesep=0.1, ranksep=0.2, ratio=0.66, margin=1,
                           ignore.clusters=F) {
    opt <- options("useFancyQuotes")
    options(useFancyQuotes = FALSE)

    ## Create dependency graph from makefile
    ##
    ## Assumptions:
    ## - no tabs in dependencies or actions names
        
    cola <- '"#FFC6AF"'                 # colour of actions
    colp <- '"#FFF8AF"'                 # colour of programs
    cold <- '"#97DDBF"'                 # colour of data
    colg <- '"#B89DDC"'                 # colour of graphical output
    colf <- '"#C2C2C2"'                 # colour of flags
    colu <- '"#E2E2E2"'                 # colour of unknown types
    colc <- 'grey98'                    # colour of clusters fills


    progtypes <- '\\.r\"|\\.py\"|\\.bug\"|\\.cmd\"|\\.sh\"'
    datatypes <- '\\.rdata\"|\\.csv\"|\\.Rdata\"'
    graphtypes <- '\\.pdf\"|\\.png\"'
    gettype <- function(z) {
        pp <- grep(progtypes, z, value=T)
        dd <- grep(datatypes, z, value=T)
        gg <- grep(graphtypes, z, value=T)
        ff <- z[!grepl('\\.', z) & !(z %in% c(pp, dd, gg))]
        uu <- z[!(z %in% c(pp, dd, ff, gg))]
        return(list(u=paste(uu, collapse=' '),
                    p=paste(pp, collapse=' '),
                    g=paste(gg, collapse=' '),
                    f=paste(ff, collapse=' '),
                    d=paste(dd, collapse=' ')))
    }
        
    mdir <- dirname(makefile)
    mk <- readLines(makefile, warn=F)

    mk <- gsub('^ *','', mk)
    mk <- mk[grepl('^\t', mk) | !grepl('=', mk) | grepl('=[\']', mk)] # remove assignments of environmental variables
    mk <- mk[!(grepl('^include', mk))]
    mk <- mk[!(grepl('\\.PHONY', mk))]
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
        
    mk <- mk[!grepl('^#', mk)]          # remove comments
    mk <- sub('\t$', '', mk)
    mk <- sub('([^ ])\\\\', '\\1 \\\\', mk)
    mk <- mk[mk!='']
    c <- grepl(':', mk) & !grepl('^\t', mk) & !grepl('#.*:', mk) #
    mk[c] <- gsub('\t', ' ', mk[c])

    mk2 <- paste(mk, collapse='&&&&')

    ## remove consecutive blank lines
    mk3 <- gsub('&&&&&&&&*','&&&&',mk2)
    ## mk3 <- gsub('\\|\\|*','|',mk2)
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
    all <- rapply(all, function(x) gsub('\'|\"', '', x), how='replace')

    if (withclusters)
        clusters <- sapply(ps, function(x) sapply(all[x], function(y) y$targs), simplify=F)

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
            els <- gettype(td)
            gf <- c(gf, sprintf('subgraph cluster%1$i {
label=%2$s; style="rounded,filled"; color=gray50; fillcolor=%3$s; fontcolor=red; fontsize=20;',
                                i, dQuote(names(ps)[i]), colc))
            gf <- c(gf, sprintf('node [fontsize=16, height=.3, style="rounded,filled", fillcolor=%1$s, shape=rectangle] %2$s;', colu, els$u))
            gf <- c(gf, sprintf('node [fontsize=16, height=.3, style="rounded,filled", fillcolor=%1$s, shape=rectangle] %2$s;', colp, els$p))
            gf <- c(gf, sprintf('node [fontsize=16, height=.3, style="rounded,filled", fillcolor=%1$s, shape=rectangle] %2$s;', cold, els$d))
            gf <- c(gf, sprintf('node [fontsize=16, height=.3, style="rounded,filled", fillcolor=%1$s, shape=rectangle] %2$s;', colg, els$g))
            gf <- c(gf, sprintf('node [fontsize=16, height=.3, style="rounded,filled", fillcolor=%1$s, shape=rectangle] %2$s;', colf, els$f))
            gf <- c(gf, sprintf('node [fontsize=16, height=.3, style="rounded,filled", fillcolor=%1$s, shape=rectangle] %2$s;}', cola, paste(ac, collapse=' ')))
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
    system(sprintf('dot -Tpdf %s -o %s', dotfile, pdffile))
    ## system(sprintf('xdg-open %s', pdffile), wait=F)
    system(sprintf('xdot %s', dotfile), wait=F)
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
        if (grepl('\\$\\(.*\\)', c[2]))
            {
                vars <- c(sapply(
                    regmatches(c[2],gregexpr(sprintf('\\$\\(([^\\)]+)\\)'),c[2])),
                    function(x) gsub('\\$\\((.*)\\)', '\\1', x)))
                        
                for (j in 1:length(vars))
                    if (vars[j] %in% ls(envir=.GlobalEnv))
                        regmatches(c[2],regexec(sprintf('\\$\\([^\\)]+\\)'),c[2])) <-
                            get(vars[j], envir=.GlobalEnv)  else
                stop(sprintf('Problem parsing makefile. Variable %s not declared?',
                             vars[j]))
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

get_stats <- function(x, na.rm=T) {
    return(c(mean=mean(x, na.rm=na.rm), med=median(x, na.rm=na.rm),
             lcl=quantile(x, 0.025, names=F, na.rm=na.rm),
             ucl=quantile(x, 0.975, names=F, na.rm=na.rm)))
}


NAanalyse <- function(df, plotx=NULL, nx=10, ncol=4, mar=c(3,3,1,1)) {
    sumna <- apply(df, 2, function(x) sum(is.na(x)))
    ## sort(sumna, decreasing=T)
    propna <- sort(round(100*sumna/nrow(df), 1), decreasing=T)
    cat('\n===', sum(propna>0), 'columns with some NAs (out of', ncol(df), ',',
        round(100*sum(propna>0)/length(propna), 1), '%)\n')
    cat('\n=== Proportion of NAs:\n')
    print(propna[propna>0])
    cat('\n===', sum(propna==0),'columns without NAs:\n')
    print(names(propna[propna==0]))
    naom <- na.omit(df)
    cat('\n===', nrow(naom), 'rows without NAs (out of', nrow(df),',',
        round(100*nrow(naom)/nrow(df),1), '%)\n\n')
    if (!is.null(plotx)) {
        par(mfrow=c(ceiling(sum(sumna>0)/ncol), ncol), mar=mar)
        for (v in names(df)) {
            if (any(is.na(df[,v]))) {
                d1 <- unique(data.frame(x=df[,plotx], v=df[,v]))
                d1 <- d1[order(d1$x),]
                d2 <- tapply(d1$v, d1$x, function(x) sum(is.na(x)))
                xx <- myreplace(pretty(1:length(d2), nx), c(0, 1), verbose=F)
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

## fold='~/dragonfly/sra-2012/report'; ignore=c('^/usr/|^/var/lib|^/etc/tex'); only=c('/')
check.latex.deps <- function(fold='.', ignore=c('^/usr/|^/var/lib|^/etc/tex|sweave/|^/dragonfly|/share/'),
                             only=c('/'), recursive=T, save_untracked=T, use.xelatex=T, ignore.rnw=F) {
    alldeps <- NULL
    prevdir <- getwd()
    setwd(fold)
    ## File dependencies in Sweave files
    if (!ignore.rnw) {
        rnw <- dir('.', '*.rnw$|*.Rnw$', recursive=recursive)
        basedir <- getwd()
        r1=rnw[1]
        for (r1 in rnw) {
            cat('\n************  ', r1, '  ************\n')
            rdir <- dirname(r1)
            setwd(rdir)
            r2 <- basename(r1)
            r <- readLines(r2)
            c1 <- r[grepl('\\bload\\(', r) & !grepl('^[[:blank:]]*#', r)]
                fs1 <- sub('load\\([\'\"]+(.*)[\'\"]+.*', '\\1', c1)
            c2 <- r[grepl('\\bread.csv\\(', r) & !grepl('^[[:blank:]]*#', r)]
                fs2 <- sub('read.csv\\([\'\"]+(.*)[\'\"]+.*', '\\1', c2)
            fs <- c(fs1, fs2)
            ## Replace global variables in .mk files by their value
            c <- grepl('load\\([a-zA-Z]+', fs)
            alldeps1 <- sub('load\\(([a-zA-Z_.0-1]+).*\\)', '\\1', fs[c])
            s <- unlist(sapply(dir('.', '*.mk.parsed'), function(mk) readLines(mk), simplify=F))
            if (!is.null(s)) {
                s1 <- do.call('rbind', strsplit(s, '[[:blank:]]*=[[:blank:]]*'))
                s2 <- sapply(alldeps1, function(x) s1[which(s1[,1] %in% x),2], simplify=F)
                fs[c] <- ifelse(sapply(s2, length), sapply(s2, '[', 1), names(s2))
            }
            cat(paste(fs, collapse='\n'),'\n')
            fs <- normalizePath(fs)
            alldeps <- c(alldeps, fs)
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
        ## system(sprintf('mkjobtexmf --jobname %s --cmd-tex pdflatex', bt))
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
                alldeps <- c(alldeps, normalizePath(fls))
                cat(paste(fls, collapse='\n'))
                cat('\n')
            } else cat('fls file inexistent. There is a problem with this file...\n')
        } else cat('Not a master file. Skip...\n')
        setwd(basedir)
    }
    cat('\n\n')
    alldeps <- alldeps[!grepl('^[ ]*\\%', alldeps)]
    alldeps <- unique(alldeps)
    alldeps <- strtrim(alldeps[!grepl(ignore, alldeps)])
    nonprocessed <- alldeps[grepl('[\\(\\)]+', alldeps)]
    alldeps <- alldeps[!grepl('[\\(\\)]+', alldeps)]
    ## Check if the dependencies are git-tracked
    s <- sapply(alldeps, is.git.tracked)
    nt <- names(s)[!s]
    if (!is.null(nt)) {
        cat('************====  Files not tracked by GIT:  ====************\n')
        cat(paste(nt, collapse='\n'))
        cat('\n\n')
        cat(paste(nt, collapse='  '))
        cat('\n\n')
    }
    if (save_untracked)
        write.csv(nt, 'untracked-dependencies.csv', row.names=F)
    setwd(prevdir)
    if (length(nonprocessed)) {
        cat('\n--- Non-processed dependencies:\n')
        print(nonprocessed)
    }
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

\\newcommand{\\plaintitle}{Temp output}
\\newcommand{\\reporttitle}{Temp output}
\\newcommand{\\pdftitle}{Temp output}
\\newcommand{\\pdfauthors}{authors}
\\newcommand{\\reportno}{??}

\\usepackage{type1cm}
\\usepackage{eso-pic}

\\input{/dragonfly/latex/mfish/aebr.tex}

\\usepackage[textsize=scriptsize]{todonotes}

\\begin{document}
',
             sprintf('\\input{%s}', sub('.tex', '', texfile)),
             '\\end{document}\n')
    ## tex <- paste(tex, collapse='\n')
    writeLines(tex, sprintf('%s/tex_output.tex', dir), sep='\n')
    system(sprintf('cd %s && pdflatex tex_output && xdg-open tex_output.pdf', dir))
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



