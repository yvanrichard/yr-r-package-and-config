all: yrpkg_1.0.tar.gz

yrpkg_1.0.tar.gz: myfunctions.r \
		update_yrpkg.r
	Rscript update_yrpkg.r


r:
	cp customisations/.Rprofile ~

y:
	cp -a customisations/. ~

updatepkg:
	cp ~/.emacs.d ~/dragonfly/yvan-r-pkg/customisations/ -r
	cp ~/.emacs ~/dragonfly/yvan-r-pkg/customisations/
	cp ~/.vim ~/dragonfly/yvan-r-pkg/customisations/ -r
	cp ~/.viminfo ~/dragonfly/yvan-r-pkg/customisations/
	cp ~/.vimrc ~/dragonfly/yvan-r-pkg/customisations/
	cp ~/.screenrc ~/dragonfly/yvan-r-pkg/customisations/
	cp ~/.bashrc ~/dragonfly/yvan-r-pkg/customisations/
	cp ~/.Rprofile ~/dragonfly/yvan-r-pkg/customisations/
