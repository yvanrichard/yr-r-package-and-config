all: yrpkg_1.0.tar.gz

yrpkg_1.0.tar.gz: myfunctions.r \
		update_yrpkg.r
	Rscript update_yrpkg.r


r:
	cp .Rprofile ~

y:
	cp .Rprofile ~
	cp .emacs ~
	cp .vimrc ~
	cp .screenrc ~