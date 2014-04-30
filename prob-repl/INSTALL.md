# WORK IN PROGRESS

## Probabilistic Programming Demo for ITA

### Installation

    These instructions are for an installation on Debian Linux (Jessie
    amd64). They should adaptable to *nix variants. 

* Install with apt-get:

        git

        autoconf
        libtool
        libgmp-dev

        libmpfr-dev

        ocaml
        camlp4-extra
        ocaml-native-compilers
        opam

        freeglut3-dev
        libjpeg-dev
        libsdl2-dev
        libsdl2-image-dev

  > sudo apt-get install NAME

* Install PPL from source:

  > git clone git://git.cs.unipr.it/ppl/ppl.git
  > cd ppl
  > autoreconf -fi
  > ./configure --enable-interfaces=ocaml
  > make
  > sudo make install

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
  lambda-term
  mlgmp

  > opam install NAME

* Install OcamlSDL2:

  > git clone https://github.com/piotrm0/OCamlSDL2.git
  > cd OcamlSDL2/src
  > cp Makefile.config.unix Makefile.config  
  > make findreinstall
  > sudo make install_h

* Install OcamlSDL2_image:

  > git clone https://github.com/piotrm0/OCamlSDL2_Image.git
  > cd OcamlSDL2_Image
  > make reinstall

### Contact
Piotr Mardziel: [piotrm@gmail.com](mailto:piotrm@gmail.com), [http://www.cs.umd.edu/~piotrm](http://www.cs.umd.edu/~piotrm)
