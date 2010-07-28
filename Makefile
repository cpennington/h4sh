# configuration variables
#

# installation prefix
PREFIX?=/usr/local

# GNU m4 command (leave blank to use /usr/local/bin/gm4)
GM4 = m4

# Options:
#   "mmap"
#   "packed"
#   "list"
# default: mmap
#
# Try "packed", if the default produces segfaults, and try "list" if
# "packed "is flakey too.
#
IO_MODE=packed

GHC = ghc

default: all

.PHONY: all build install clean check
all: build

build:
	$(GHC) --make -o build Build.hs
	./build $(IO_MODE)
	cabal configure --enable-shared
	cabal build

install:
	cabal install

clean: 
	./build clean
	cabal clean
	rm -rf *.hi *.o *~
	rm -rf h4sh.cabal
	rm -rf build dist
	cd testsuite && $(MAKE) clean

check:
	@echo "Running testsuite"
	( cd testsuite ; GM4="$(GM4)" $(MAKE); ./run-utests )
