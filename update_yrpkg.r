rm(list=ls())
basedir <- '~/dragonfly/yr-r-package-and-config'
setwd(basedir)

source('myfunctions.r')

if (file.exists('yrpkg'))
  {
    d <- readLines('yrpkg/DESCRIPTION')
    d <- d[grep('Version',d)]
    version <- as.numeric(gsub('Version: ([0-9.]*)','\\1',d))
    version <- version + 0.01
  } else version <- 1

system('rm yrpkg* -fr')

funs <- ls()
funs <- funs[!(funs %in% c('basedir','version', 'd'))]

package.skeleton(list=funs, name="yrpkg")
setwd('yrpkg')
manfiles <- sort(sub('.Rd$','',dir('man')))
rfiles <- sort(sub('.R$','',dir('R')))
## fill-up help files
setwd('man')
rds <- dir('.',pattern='*.Rd')
d=rds[length(rds)]
for (d in rds)  # d=rds[53]  # d="check_cited_labels.Rd"
  {
    cat('\n',d,'\n')
    D <- readLines(d)
    D[which(D == '\\title{')+1] <- sub('.Rd$', '', d)
    if (d != 'yrpkg-package.Rd')
      {
        w1 <- grep('The function is currently defined as', D) + 1
        w2 <- min(which(D == '}')[which(D == '}') > w1])
        c <- 1:length(D) >= w1 & 1:length(D) <= w2
        D[c] <- sprintf(' ## %s', D[c])
      }
    ## if (d == 'z%nin%.Rd')
    ##   {
    ##     D <- gsub('\\\\%', '%', D)
    ##   } else{
    torep <- grep('^~~|^%%',D)
    D <- D[-torep]
    D <- gsub('\\\\\\|','\\\\\\\\\\|', D)
    ## }
    writeLines(D, d)
  }

# Update version number
setwd(sprintf('%s/yrpkg', basedir))
d <- readLines('DESCRIPTION')
d[grep('^Version', d)] <- sprintf('Version: %0.2f', version)
writeLines(d, 'DESCRIPTION', sep='\n')


setwd(basedir)
out <- system('R CMD build yrpkg 2>&1', intern=T)
print(out)
if (!length(grep('ERROR',out)))
    {
    out <- system('R CMD check yrpkg 2>&1', intern=T)
    print(out)
    }

if (!length(grep('ERROR',out)))
    {
    system('sudo R CMD INSTALL yrpkg --byte-compile 2>&1', intern=T)
    print(out)
    }
