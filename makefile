all: .yrpkg

.yrpkg: myfunctions.r \
		update_yrpkg.r
	Rscript update_yrpkg.r && touch .yrpkg

install:
	sudo R CMD INSTALL yrpkg_*.tar.gz --byte-compile
install2:
	mkdir -f ~/R/perso_libs  &&  R CMD INSTALL yrpkg_*.tar.gz --byte-compile --library=~/R/perso_libs

r:
	rsync -avz customisations/.Rprofile ~

y:
	rsync -avz -a customisations/. ~

updatepkg:
	rsync -avz --exclude '.git*' ~/.emacs.d customisations/
	rsync -avz --exclude '.git*' ~/.emacs* customisations/
	rsync -avz --exclude '.git*' ~/.vim customisations/ -r
	rsync -avz --exclude '.git*' ~/.viminfo customisations/
	rsync -avz --exclude '.git*' ~/.vimrc customisations/
	rsync -avz --exclude '.git*' ~/.screenrc customisations/
	rsync -avz --exclude '.git*' ~/.bashrc customisations/
	rsync -avz --exclude '.git*' ~/.Rprofile customisations/

getpackagelist:
	Rscript -e 'libs<-sort(library()$$results[,"Package"]); save(libs, file="r-packages_libs.rdata")'

installpackagelist:
	sudo Rscript -e 'load("r-packages_libs.rdata"); libs<-libs[!(libs %in% library()$$results[,"Package"])]; if (length(libs)) install.packages(libs) else cat("No packages needed to be installed\n")'
