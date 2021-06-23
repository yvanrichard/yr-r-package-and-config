all: .yrpkg

.yrpkg: myfunctions.r \
		update_yrpkg.r
	Rscript --vanilla update_yrpkg.r && touch .yrpkg

install:
	sudo R CMD INSTALL yrpkg_*.tar.gz --byte-compile

PERSLIB := $(shell Rscript --vanilla -e "cat(Sys.getenv('R_LIBS_USER'))")
install2:
	mkdir -p $(PERSLIB)  &&  R CMD INSTALL yrpkg_*.tar.gz --byte-compile --library=$(PERSLIB)

r:
	rsync -avz customisations/.Rprofile ~

y:
	rm ~/.emacs.d -fr
	rsync -avz -a customisations/. ~

updatepkg:
	rm customisations/.emacs.d -fr
	rsync -avz --exclude '.git*' ~/.emacs.d customisations/
	cp ~/.emacs customisations/
	rsync -avz --exclude '.git*' ~/.vim customisations/ -r
	rsync -avz --exclude '.git*' ~/.viminfo customisations/
	rsync -avz --exclude '.git*' ~/.vimrc customisations/
	rsync -avz --exclude '.git*' ~/.screenrc customisations/
	rsync -avz --exclude '.git*' ~/.bashrc customisations/
	rsync -avz --exclude '.git*' ~/.psqlrc customisations/
	rsync -avz --exclude '.git*' ~/.Rprofile customisations/
	rsync -avz --exclude '.git*' ~/.Renviron customisations/
	rsync -avz --exclude '.git*' ~/.gitconfig customisations/
	rsync -avz --exclude '.git*' ~/.gitignore customisations/
	rsync -avz --exclude '.git*' ~/.pushover customisations/
	rsync -avz --exclude '.git*' ~/.rpushbullet.json customisations/
	rsync -avz --exclude '.git*' ~/.wakatime.cfg customisations/
	rsync -avz --exclude '.git*' ~/.tmux.conf customisations/
	rsync -avz --exclude '.git*' ~/.tmux customisations/

getpackagelist:
	Rscript -e 'libs<-sort(library()$$results[,"Package"]); save(libs, file="r-packages_libs.rdata")'

installpackagelist:
	sudo Rscript --vanilla -e 'load("r-packages_libs.rdata"); libs<-libs[!(libs %in% library()$$results[,"Package"])]; if (length(libs)) install.packages(libs, repos="https://cran.stat.auckland.ac.nz") else cat("No packages needed to be installed\n")'

