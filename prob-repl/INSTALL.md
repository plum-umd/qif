# WORK IN PROGRESS

## Probabilistic Programming Demo for ITA

### Installation

    These instructions are for an installation on Debian Linux, Wheezy. They
    should adaptable to *nix variants. Otherwise, a VirtualBox image that
    has the entire demo installed and ready to go can be found at:

    http://www.cs.umd.edu/~piotrm/virtualboxes/debian_x11_prob_repl.gz

    Everything has been already set up in this image. Login as
    *user*/*user* and look in the *~/prob-repl* folder for more
    information. The root password is *toor* if you need it.

* Install PPL from source:

  > git clone git://git.cs.unipr.it/ppl/ppl.git
  > cd ppl

  > autoreconf -fi

  ./configure --prefix=/Users/piotrm/probreq --with-gmp=/Users/piotrm/probreq --with-mlgmp=/Users/piotrm/probreq/lib/ocaml/gmp --enable-interfaces=ocaml

  if using gmp version >= 5.0 then apply the patches in install/ppl-1.0
    patch -p1 < t1.patch
    patch -p1 < t2.patch

  make
  make install


* Install with apt-get:
        git

        autoconf
        libtool
        libgmp-dev

?        libmpfr-dev

        ocaml
        camlp4-extra
        ocaml-native-compilers
        opam

        freeglut3-dev
        libjpeg-dev
        libsdl-dev

  > sudo apt-get install NAME

* Configure OPAM

  > opam config
  > eval `opam config env`

* Install opam from source:
  > wget https://github.com/ocaml/opam/archive/1.1.1.tar.gz
  > tar -zxvf 1.1.1.tar.gz
  > cd opam-1.1.1
  > ./configure
  > make
  > sudo make install
  > opam init

* Install with opam:

  ocamlfind
  core_extended
  ocamlnet
  extlib
  glMLite
  ocamlsdl
  lambda-term
  mlgmp

  > opam install NAME


0. Add backports to apt-sources, /etc/apt/sources.list :

  deb http://ftp.us.debian.org/debian/ wheezy-backports main
 
  > apt-get update

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

8. ppl


### Contact
Piotr Mardziel: [piotrm@gmail.com](mailto:piotrm@gmail.com), [http://www.cs.umd.edu/~piotrm](http://www.cs.umd.edu/~piotrm)
