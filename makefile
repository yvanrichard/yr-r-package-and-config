all: .yrpkg

.yrpkg: myfunctions.r \
		update_yrpkg.r
	Rscript update_yrpkg.r && touch .yrpkg

install:
	sudo R CMD INSTALL yrpkg_*.tar.gz --byte-compile
install2:
	R CMD INSTALL yrpkg_*.tar.gz --byte-compile

r:
	rsync -avz customisations/.Rprofile ~

y:
	rsync -avz -a customisations/. ~

updatepkg:
	rsync -avz ~/.emacs.d customisations/
	rsync -avz ~/.emacs* customisations/
	rsync -avz ~/.vim customisations/ -r
	rsync -avz ~/.viminfo customisations/
	rsync -avz ~/.vimrc customisations/
	rsync -avz ~/.screenrc customisations/
	rsync -avz ~/.bashrc customisations/
	rsync -avz ~/.Rprofile customisations/

getpackagelist:
	Rscript -e 'libs<-sort(library()$$results[,"Package"]); save(libs, file="r-packages_libs.rdata")'

installpackagelist:
	sudo Rscript -e 'load("r-packages_libs.rdata"); libs<-libs[!(libs %in% library()$$results[,"Package"])]; if (length(libs)) install.packages(libs) else cat("No packages needed to be installed\n")'
