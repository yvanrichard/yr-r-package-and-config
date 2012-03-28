# package.skeleton(list=ls(), name="mypkg")


###############################################################################
###  VECTOR/MATRIX MANIPULATION
###############################################################################

## replace values in a vector by corresponding ones using a lookup table
# ex: lookup <- c(
# oldval1, newval1,
# oldval2, newval2)
myreplace <- function(what, lookup, verbose=T, warn=T)
    {
    w = what
    lkup = matrix(lookup, ncol=2, byrow=T)
    c = w %in% lkup[,1]
    if (warn)
	{
	cond = !(lkup[,1] %in% what)
	if (sum(cond))
	    warning(sum(cond),' value(s) in lookup vector not in original data')
	}
    w[c] = lkup[match(w[c], lkup[,1]), 2]
    s = sum(w!=what)
    if (verbose)
	cat('\nReplaced ', s, ' values out of ', length(w), ' (', round(100*s/length(w),2), '%).\n', sep='')
    return(w)
    }


sample_df <- function(df, n=10)
    {
    df[sort(sample(1:nrow(df))[1:n]),]
    }

## sample from a vector without knowing its length (useful in sapply functions)
sample_in <- function(x, n=100, ...)
  {
    if (length(dim(x)))
      stop('Not a vector') else
    return(x[sample(1:length(x), n, ...)])
  }
    
###############################################################################
###  TEXT MANIPULATION
###############################################################################

## Function to test if text is a number
is_num_txt <- function(x)
    return(suppressWarnings(!is.na(as.numeric(x))))
    
## Lower 1st letter (mainly for species)
lower1st <- function(x)
    {
#     x <- dat$name
    x1 <- strsplit(x, '')
    x <- sapply(x1, function(x) {
	x[1] <- tolower(x[1]) 
	return(paste(x, collapse=''))
	})
    return(x)
    }

upper1st <- function(x)
    {
    x1 <- strsplit(x, '')
    x <- sapply(x1, function(x) {
	x[1] <- toupper(x[1]) 
	return(paste(x, collapse=''))
	})
    return(x)
    }

## from help of chartr()
capwords <- function(s, strict = FALSE)
    {
    cap <- function(s) paste(toupper(substring(s,1,1)),
	{s <- substring(s,2); if(strict) tolower(s) else s},
	    sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
    }

## Returns string of comma-separated values, with quotes (or not)
Pprint <- function(x, use.quotes=T) {
  if (use.quotes)
    cat(paste(sprintf('\'%s\'', x), collapse=','),'\n') else
  cat(paste(sprintf('%s', x), collapse=','),'\n')
}


###############################################################################
###  PLOTS
###############################################################################

alpha <- function (colour, alpha)  # alpha() function from ggplot2
    {
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

alphagrad <- function(n, grad='exp', curv=20, inv=F)
    {
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
	cols <- rev(alpha(rep('#FFFFFF',n),alphas)) else
	    cols <- alpha(rep('#FFFFFF',n),alphas)
    return(cols)
    }

applyalphagrad <- function(colors, grad='exp', curv=15, inv=F, startalpha=0, endalpha=1)
    {
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


scaleminmax <- function(in.seq, out.min=0, out.max=1, in.ref.min=NA, in.ref.max=NA)  # in.seq=sort(rnorm(10)); out.min=0.2; out.max=0.8
    {
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

colorfrom <- function(var, colors=c('black','red','gold','darkgreen'), ncolors=100, alpha=1, rev=F, ...) 
  {
    library(RColorBrewer)
    if (is.factor(var) | is.character(var))
      {
        if (!is.factor(var))  var <- factor(var, levels=unique(var))
        colors <- rep(brewer.pal(12, 'Set3'), length.out=nlevels(var))
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

interms <- function(y, min=0)
    {
    if (max(y)<5)
	float = T else
	    float = F
    x <- log(y+1)
    x <- max(x)
    if (!float)
	return(round(c(min, signif(exp(x/4)-1, 1), signif(exp(x/2)-1,1), signif(exp(3*x/4)-1,1), exp(x)-1))) else
	    return(c(min, signif(exp(x/4)-1, 1), signif(exp(x/2)-1,1), signif(exp(3*x/4)-1,1), round(exp(x)-1,3)))
    }

getmfrow <- function(n, dim=c(7,9))   # n=12; dim=c(4,5)
  {
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
draw.env <- function(x=1:length(lcl), lcl, ucl, fill=gray(0.9), line=gray(0.75))
    {
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
                 )
  {
    if (clean)
      {
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
      for (xi in 1:lay[1])
        {
          ni <- ni + 1
          if (ni <= n)
            x11(xpos=xposs[xi], ypos=yposs[yi], width=W/res[1], height=H/res[2])
        }
    dev.set(dn)
  }

col3d <- function(x, y, z, space='rgb')
  {
    f <- switch(space, rgb=rgb, hcl=hcl, hsv=hsv) 
    return(f(scaleminmax(x), scaleminmax(y), scaleminmax(z)))
  }



## Calculate density of y for each x, and plot them on same plot
# col='red'; alph=0.5; col.border=NULL; xmin=NULL; at=NULL; ylevels=NULL; ylab=NA; cap1st=T
plotdensapply <- function(y, by, col='red', alph=0.5, col.border=NULL, xmin=NULL, at=NULL,
                          ylevels=NULL, ylab=NA, cap1st=T, gridx=0, gridy=0,
                          gridcol=grey(0.8), ...)
  {
    if (!is.factor(by))
      by <- factor(by, levels=sort(unique(by)))
    by <- factor(by, levels=rev(levels(by)))
    spp <- levels(by)
    nspp <- nlevels(by)
    dd <- tapply(y, by, density)
    minx <- min(sapply(dd, function(x) min(x$x)))
    maxx <- max(sapply(dd, function(x) max(x$x)))
    rngx <- extendrange(c(minx, maxx), f=0.04)
    ## plot(NA, xlim=c(ifelse(is.null(xmin), minx*0.1, xmin), maxx*1.02), ylim=c(0,nspp+1), ylab=NA, yaxs='i', xaxs='i', yaxt='n')
    plot(NA, xlim=switch(is.null(xmin)+1, c(xmin, rngx[2]), rngx), ylim=c(0,nspp+1),
         ylab=ylab, yaxs='i', xaxs='i', yaxt='n', ...)
    grid(gridx, gridy, gridcol)
    if (is.null(at))   at <- 1:nspp
    if (length(at) != nspp) stop('Length of "at" does not match that of levels of "by"')
    if (length(col)==1)  col <- rep(col, nspp)
    if (is.null(col.border))  col.border <- col
    col <- rev(col)
    col.border <- rev(col.border)
    for (di in 1:nspp)  # di=nspp
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
    mtext(ylev2, 2, at=at2, las=1, line=0.5)
  }



lighten <- function(col, c=.5)
  {
    rgbs <- col2rgb(col)
    r <- rgbs['red',]/255
    g <- rgbs['green',]/255
    b <- rgbs['blue',]/255
    rd <- r + c*(1-r)
    gd <- g + c*(1-g)
    bd <- b + c*(1-b)
    return(rgb(rd,gd,bd))
  }

darken <- function(col, c=.3)
  {
    rgbs <- col2rgb(col)
    r <- rgbs['red',]/255
    g <- rgbs['green',]/255
    b <- rgbs['blue',]/255
    rd <- r * (1-c)
    gd <- g * (1-c)
    bd <- b * (1-c)
    return(rgb(rd,gd,bd))
  }



###############################################################################
###  MAPS / GIS
###############################################################################

## Points to FMA
getfma <- function(df, xcolumn, ycolumn)
    {
    library(rgdal)
    fmas <- readOGR('/dragonfly/gis/shapes/mfish','fma')
    df$x <- df[[xcolumn]]
    df$y <- df[[ycolumn]]

    coordinates(df) = ~x+y
    proj4string(df) <- CRS("+proj=longlat +datum=WGS84")

    return(overlay(df, fmas))
    }


## NZ basemap
# corners=list(x=c(162,189), y=c(-54,-30))
nz <- function(type='contour', corners=list(x=c(162,189), y=c(-54,-30)), pshift=0.005, simple=F, title=NA)
    {
    if (!length(dev.list()))
	par(mar=c(0,0,0,0))

    require(maptools)
    gpclibPermit()  # enable licence for polygon computations

    minx <- min(corners$x)
    maxx <- max(corners$x)
    miny <- min(corners$y)
    maxy <- max(corners$y)
    xshift <- diff(range(corners$x))*pshift
    yshift <- -diff(range(corners$y))*pshift

    cols <- colorRampPalette(c("#AAAAFF", "#EEEEFF"))
    ## Load NZ contour and bathymetry maps
    NZ <- Rgshhs('/dragonfly/gis/gshhs/gshhs_i.b', level=1, 
		xlim=c(minx,maxx), ylim=c(miny,maxy))
    
    plot(NA, xlim=c(minx,maxx), ylim=c(miny,maxy), main=title)	

    if (!simple)
	{
	load('/dragonfly/gis/r/bathy.rdata')

	if (type=='raster')
	    {
	    image(bathy$lon,bathy$lat,bathy$height, add=T, col=cols(50), useRaster=T)
	    plot(elide(NZ$SP, shift=c(xshift,yshift)), col=grey(0.1), border=grey(0.1), add=T)
	    plot(NZ$SP, add=T, col=grey(0.7), border=grey(0.68))	
	    }

	if (type=='contour')
	    {
	    plot(elide(NZ$SP, shift=c(xshift,yshift)), col=grey(0.1), border=grey(0.1), add=T)
	    plot(NZ$SP, add=T, col=grey(0.7), border=grey(0.68))	
	    contour(bathy$lon, bathy$lat, -bathy$height, levels=c(200,500,1000,2000,5000),
		col=gray(log(seq(exp(0.5*5),exp(0.85*5),length.out=5))/5), add=TRUE, labcex=0.2, lwd=0.5, drawlabels=F)
	    }
	} else
	    plot(NZ$SP, col=grey(0.7), border=grey(0.68))	
    }



###############################################################################
###  CALCULATIONS
###############################################################################
    
## coefficient of variation
cv <- function(x) sd(x)/mean(x)

## round a number (or a vector of numbers), higher or lower depending on decimal part    
rnd_round <- function(K)   # K=4.3    # K=c(4.1, 4.33333, 4.7, 4.99)
    {
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
nearest <- function(x, val=0.2)
    {
    coeff <- 1/val    
    return(round(x*coeff)/coeff)
    }

logit <- function(x)
    {
    return(log(x/(1-x)))
    }
invlogit <- function(x)
    {
    return(exp(x)/(1+exp(x)))
    }


# My own function to estimate x to get a given y
# Limits are not strict, just to give an idea of the slope and where to start (lims[1])
optimise_y <- function(f, target, lims, incr=0.01, tol=0.00001, ...) 
  {
    ## target=0.05; lims=c(0,3); incr=0.01; tol=0.00001
    xys <- NULL
    xi <- lims[1]
    ci <- lims
    yi <- f(xi, ...)
    #yi <- f(xi, pbrx, spx)
    slope <- sign(f(ci[2], ...) - f(ci[1], ...))
    dir = ifelse(sign(yi-target)==slope, -1, 1)
    samedir = 0
    steps = 0
    stillhunt = T
    first <- T
    ## 1- Hunt: start anywhere then move with increasing step until limit is passed
    while (stillhunt)
      {
        steps = steps + 1
        if (!first)
          {
            yi <-  f(xi, ...)
            #yi <- f(xi, pbrx, spx)
          } else first <- F
        xys = rbind(xys, c(xi, yi, dir, incr))
        if (dir < 0 & yi < target)
          {
            ci[1] = xi
            stillhunt = F
          }
        if (dir > 0 & yi > target)
          {
            ci[2] = xi
            stillhunt = F
          }
        if (dir > 0 & yi < target)
          {
            ci[1] = xi
          }
        if (dir < 0 & yi > target)
          {
            ci[2] = xi
          }
        if (stillhunt) 
          {
            incr = incr * (sqrt(5)+1)/2
            xi = xi + dir * incr
          } else
        {
          incr = incr * (sqrt(5)-1)/2
          dir = -1 * dir
          xi = xi + dir * incr
        }
      }

    ## 2- Bisection: halve the step
    while (abs(yi - target) > tol  &  incr > 1e-6  &  incr != tol & steps<666)
      {
        steps = steps + 1
        yi <- f(xi, ...)
        #yi <- f(xi, pbrx, spx)
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


###############################################################################
###  DIAGNOSE
###############################################################################

## table() as data frame
tabl <- function(x, sort=T)
    {
    if (length(x))
	{
	t = as.data.frame(table(x, useNA='always'), stringsAsFactors=F)
	names(t) <- c('value','freq')
	if (sort)
	    t <- t[order(t$freq, decreasing=T),]
	return(t)
	} else
	    stop('x is empty')
    }

## customised table()
tab <- function(x)
    {
    return(sort(table(x, useNA='always'), decreasing=T))
    }
    

makeuniquefilename <- function(x)  # x='a1'
  {
    if (length(x)>1) stop('Function takes only one file name')
    x2 <- strsplit(x,'.',fixed=T)[[1]]
    x_1 <- paste(x2[1:(length(x2)-1)], collapse='.')
    ext <- ifelse(length(x2)>1, sprintf('.%s',x2[length(x2)]), '')
    fs <- grep(sprintf('^%s.*%s$', x_1, ext), dir(), value=T)
    fs1 <- sub(sprintf('%s',ext),'', fs)
    return(sprintf('%s%s', tail(make.names(c(fs1, x_1), unique=T), 1), ext))
  }

## Open data frame in oocalc
localc <- function(df, row.names=T, newl.at=100)
    {
      f <- makeuniquefilename('temp.csv')
      if (!is.na(newl.at))
	{ ## insert return line when field is too long
          nch = apply(df,2,function(x) max(nchar(as.character(x))))
          toolongcols = names(nch[nch>newl.at])
          for (c in toolongcols)        # c=toolongcols[1]
	    {
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
      write.csv(as.data.frame(df), f, row.names=row.names)
      res <- system(sprintf('localc %s', f), wait=F)
      system(sprintf('sleep 10; rm %s', f), wait=F)
    }


###############################################################################
###  UTILS
###############################################################################

## quit R without confirmation
Q <- function(){base::q('no')}

## Get size of memory objects
lsmem <- function()
    {
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
# rdata = '~/dragonfly/abundance/report/data-summary/data/postgrooming_report.RData'
peepinto <- function(rdata, more=F)  # rdata='test.rdata'; more=F
  {
    e1 <- new.env()
    load(rdata, envir=e1)
    z = sapply(ls(envir=e1), function(x) object.size(get(x, envir=e1)))
    z = data.frame(object=names(z), size=z, stringsAsFactors=F)
    z = z[order(z$size, decreasing=T),]
    z$dim <- NULL
    z$length <- NULL
    z$type <- NULL
    for (i in 1:nrow(z))                # i=1
      {
        o <- get(z$object[i], envir=e1)
        z$dim[i] <- list(dim(o))
        z$length[i] <- length(o)
        z$type[i] <- typeof(o)
      }
    if (more)
      {
        z$summ <- NULL
        for (i in 1:nrow(z))            # i=1
          {
            o <- get(z$object[i], envir=e1)
            z$summ[i] <- list(summary(o))
          }
      }
    rownames(z) <- z$object
    return(z[,-1])
  }


## List variables contained in a .rdata file, with their structure
ls.str.into <- function(rdata)
  {
    e1 <- new.env()
    load(rdata, envir=e1)
    ls.str(envir=e1)
  }

## dim that works also on vectors
Dim <- function(dat)
  {
    dm <- dim(dat)
    if (is.null(dm))
      return(length(dat)) else
         return(dm)
  }


## Progress bar. Draw first line and return indices when a symbol should be drawn.
## ex: if (i %in% ids) cat('.')
progressbar <- function(n, length=50)  # n=237; length=50
  {
    cat(sprintf('|%s|\n', paste(rep('-',length-2), collapse='')))
    s <- 1:n
    sp <- s/n * length
    target <- 1:length
    ids <- sapply(target, function(x) which.min(abs(sp-x)))
    return(ids)
  }


## Summary of all computer processes
compprocsumm <- function(comps=c('robin','leon','titi','tieke','frank','jeremy'), user='yvan', getres=F)
  {
    res <- NULL
    cp=comps[1]
    for (cp in comps)
      {
        cmd <- sprintf('ssh -A %s@%s \"ps -eo \\\"%%c|%%C|%%x|%%z|%%U\\\" --no-heading | column -t\"', user, cp)
        r <- system(cmd, intern=T)
        if (length(r))
          {
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
proc_in_dfly <- function(comm='R', comps=c('robin','leon','titi','tieke','frank','jeremy'), user='yvan', getres=F)
  {
    res <- NULL
    cp=comps[1]
    for (cp in comps)
      {
        cmd <- sprintf('ssh -A %s@%s \"ps -C %s -o \\\"%%c|%%C|%%x|%%z|%%U\\\" --no-heading | column -t\"', user, cp, comm)
        r <- system(cmd, intern=T)
        if (length(r))
          {
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


## return random factor for testing
rndfactor <- function(n=15, nlev=4)
  return(factor(sample(LETTERS[1:nlev], n, replace=T)))


## return string to be copied in script to setwd to current dir and copy string to clipboard
setwdhere <- function()
  {
    home <- Sys.getenv('HOME')
    here <- getwd()
    txt <- sprintf('setwd(\'%s\')',sub(home, '~', here))
    cat(sprintf('\n%s\n\n',txt))
    #system(sprintf('echo \"%s\" | xclip', txt))
    File = pipe("xclip -i", "w")
    cat(txt, file=File)
    close(File)
  }
