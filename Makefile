all: build

run: build
	fpm run

build:
	fpm build

test:
	fpm test

clean:
	rm -f *.f *.f90 *.o *.mod *.x

.PHONY: all run build test clean
