all: .yrpkg

.yrpkg: myfunctions.r \
		update_yrpkg.r
	Rscript update_yrpkg.r && touch .yrpkg

install:
	sudo R CMD INSTALL yrpkg_*.tar.gz --byte-compile

r:
	cp customisations/.Rprofile ~

y:
	cp -a customisations/. ~

updatepkg:
	cp ~/.emacs.d customisations/ -r
	cp ~/.emacs customisations/
	cp ~/.vim customisations/ -r
	cp ~/.viminfo customisations/
	cp ~/.vimrc customisations/
	cp ~/.screenrc customisations/
	cp ~/.bashrc customisations/
	cp ~/.Rprofile customisations/

getpackagelist:
	Rscript -e 'libs<-sort(library()$$results[,"Package"]); save(libs, file="r-packages_libs.rdata")'

installpackagelist:
	sudo Rscript -e 'load("r-packages_libs.rdata"); libs<-libs[!(libs %in% library()$$results[,"Package"])]; if (length(libs)) install.packages(libs) else cat("No packages needed to be installed\n")'
