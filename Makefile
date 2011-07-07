
all:
	R CMD build .
	R CMD build --binary .
	R CMD check .
	R CMD install .

clean:
	rm -rf ..Rcheck
