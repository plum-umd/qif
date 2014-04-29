## Probabilistic Programming Demo for ITA

### Installation

1. ocaml:
  sudo apt-get install ocamlc

2. opam (for debian, get from source):
  > wget https://github.com/ocaml/opam/archive/1.1.1.tar.gz
  > tar -zxvf 1.1.1.tar.gz
  > cd opam-1.1.1
  > ./configure
  > make
  > sudo make install

  > opam init

3. opam, with opam, get:
  * ocamlfind
    > opam install ocamlfind
  * ocamlnet
    > opam install ocamlnet
  * extlib
    > opam install extlib

4. gmp
  ./configure --prefix=/Users/piotrm/probreq --enable-cxx
  make
  make install

5. mpfr: http://www.mpfr.org/mpfr-current/#download
  ./configure --prefix=/Users/piotrm/probreq --with-gmp=/Users/piotrm/probreq

6. mpc: http://www.multiprecision.org/index.php?prog=mpc&page=download
  ./configure --prefix=/Users/piotrm/probreq --with-gmp=/Users/piotrm/probreq

7. mlgmp: http://www-verimag.imag.fr/~monniaux/download/mlgmp_20120224.tar.gz
  make clean
  edit the first few lines of Makefile: 
    OCAML_LIBDIR:= /usr/local/lib/ocaml
    GMP_INCLUDES:= -I/Users/piotrm/probreq/include
    
    GMP_LIBDIR=/Users/piotrm/probreq/lib
    
    DESTDIR= /Users/piotrm/probreq/lib/ocaml/gmp

  make
  make install

8. ppl-1.0
  ./configure --prefix=/Users/piotrm/probreq --with-gmp=/Users/piotrm/probreq --with-mlgmp=/Users/piotrm/probreq/lib/ocaml/gmp --enable-interfaces=ocaml

  if using gmp version >= 5.0 then apply the patches in install/ppl-1.0
    patch -p1 < t1.patch
    patch -p1 < t2.patch

  make
  make install

### Contact
Piotr Mardziel: [piotrm@gmail.com](mailto:piotrm@gmail.com), [http://www.cs.umd.edu/~piotrm](http://www.cs.umd.edu/~piotrm)
