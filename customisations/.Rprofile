##						Emacs please make this -*- R -*-
## empty Rprofile.site for R on Debian
##
## Copyright (C) 2008 Dirk Eddelbuettel and GPL'ed
##
## see help(Startup) for documentation on ~/.Rprofile and Rprofile.site

# ## Example of .Rprofile
# options(width=65, digits=5)
# options(show.signif.stars=FALSE)
# setHook(packageEvent("grDevices", "onLoad"),
#         function(...) grDevices::ps.options(horizontal=FALSE))
# set.seed(1234)
#.First <- function()
#{
#source('/home/yvan/dragonfly/mytests/myfunctions.r')
#}
#
.First <- function() {
  cat(as.character(Sys.time()),' -- R session started.\n\n',sep='')
  MarkViewer <- "gvim"
}
.Last <- function()  {
  w <- warnings()
  if (length(w))
    print(w)
  cat('\n',as.character(Sys.time()),' -- R session finished.\n\n',sep='')
}

"%nin%" <- function(x, y) return(!(x %in% y))

# ## Example of Rprofile.site
# local({
#  # add MASS to the default packages, set a CRAN mirror
#  old <- getOption("defaultPackages"); r <- getOption("repos")
#  r["CRAN"] <- "http://my.local.cran"
#  options(defaultPackages = c(old, "MASS"), repos = r)
#})

local({
old <- getOption("defaultPackages")
r <- getOption("repos")
r["CRAN"] <- "http://cran.stat.auckland.ac.nz"
options(defaultPackages = c(old, "yrpkg"),
        repos = r,
        Ncpus=5,
        warn=1,
        warnPartialMatchDollar=T,
        scipen=1,
        error=NULL)
})




