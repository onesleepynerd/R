# Developers: make && make install && make docs

all:
	R CMD build .
	R CMD check *.tar.gz

install:
	R CMD install *.tar.gz

clean: docs.clean
	rm -rf opani.Rcheck
	rm -fi *.tgz *.tar.gz

docs: docs.clean
	R CMD Rd2dvi --pdf .

docs.clean:
	rm -rf ..pdf .Rd2dvi*

