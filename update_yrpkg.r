rm(list=ls())
basedir <- '~/dragonfly/yr-r-package-and-config'
setwd(basedir)

source('myfunctions.r')

system('rm yrpkg* -fr')


if (file.exists('yrpkg'))
    {
    setwd('yrpkg')
    currfuns <- ls()
    newfuns <- currfuns[!(currfuns %in% sub('.R$','',dir('R')))]
    cat(newfuns, sep='\n')

    ## some house-keeping
    manfiles <- sub('.Rd$','',dir('man'))
    rfiles <- sub('.R$','',dir('R'))
    missing <- sprintf('R/%s.R',rfiles[!(rfiles %in% manfiles)])
    if (length(missing))
	system(paste(c('rm', missing), collapse=' '))

    ## export functions to pkg and create help files
    for (f in newfuns)  # f=newfuns[1]
	{
	prompt(f, force.function = T)
	dput(eval(parse(text=f)), sprintf('R/%s.R', f))
	}

    ## fill-up help files
    rds <- dir('.',pattern='*.Rd')
    for (d in rds)  # d=rds[1]
	{
	D <- readLines(d)
	D[which(D == '\\title{')+1] <- sub('.Rd$', '', d)
	writeLines(D, d)
	}
	
    ## put help files into man    
    system('mv *.Rd man 2>&1', intern=T)
    # ## then edit .Rd files in /yrpkg and cut/paste into yrpkg/man
    } else
	{
	package.skeleton(list=ls(), name="yrpkg")
	setwd('yrpkg')
	manfiles <- sort(sub('.Rd$','',dir('man')))
	rfiles <- sort(sub('.R$','',dir('R')))
	## fill-up help files
	setwd('man')
	rds <- dir('.',pattern='*.Rd')
	for (d in rds)  # d=rds[53]  # d="check_cited_labels.Rd"
	    {
	    D <- readLines(d)
	    D[which(D == '\\title{')+1] <- sub('.Rd$', '', d)
            w1 <- which(D == '## The function is currently defined as')+1
            w2 <- min(which(D == '}')[which(D == '}') > w1])
            c <- 1:length(D) >= w1 & 1:length(D) <= w2
            D[c] <- sprintf('## %s', D[c])
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
      }
	
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
