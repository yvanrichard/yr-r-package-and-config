##						Emacs please make this -*- R -*-
## empty Rprofile.site for R on Debian
##
## Copyright (C) 2008 Dirk Eddelbuettel and GPL'ed
##
## see help(Startup) for documentation on ~/.Rprofile and Rprofile.site

#### Example of .Rprofile
options(width=115)
## options(width=65, digits=5)
## options(show.signif.stars=FALSE)
## setHook(packageEvent("grDevices", "onLoad"),
##         function(...) grDevices::ps.options(horizontal=FALSE))
## set.seed(1234)

## .First <- function() {
##     cat(as.character(Sys.time()),' -- R session started  {{{\n\n',sep='')
##     MarkViewer <- "gvim"
## }
## .Last <- function()  {
##     w <- warnings()
##     if (length(w))
##         print(w)
##     cat('\n',as.character(Sys.time()),' -- R session finished }}}\n\n',sep='')
## }
options(error = function() traceback(2))


## ## Example of Rprofile.site
## local({
##  # add MASS to the default packages, set a CRAN mirror
##  old <- getOption("defaultPackages"); r <- getOption("repos")
##  r["CRAN"] <- "http://my.local.cran"
##  options(defaultPackages = c(old, "MASS"), repos = r)
##})

local({
    old <- getOption("defaultPackages")
    r <- getOption("repos")
    r["CRAN"] <- "https://cran.stat.auckland.ac.nz"
    ## r["INLA"] <- "http://www.math.ntnu.no/inla/R/testing"
    options(defaultPackages        = c(old, "yrpkg"),
            repos                  = r,
            Ncpus                  = 6,
            warn                   = 1,
            warnPartialMatchDollar = T,
            scipen                 = 100,
            error                  = NULL,
            datatable.print.nrows  = 57,
            menu.graphics = F)
    ## options(error = function() traceback(2))
    ## "%nin%" <- function(x, y) return(!(x %in% y))
})


## local({
##     r <- getOption("repos")
##     r["CRAN"] <- "http://cran.stat.auckland.ac.nz"
##     options(repos = r)
## })




options(RDocs.override = TRUE)
options(spacy_condaenv = "spacy_condaenv")
