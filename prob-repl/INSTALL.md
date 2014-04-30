# WORK IN PROGRESS

## Probabilistic Programming Demo for ITA

### Installation

    These instructions are for an installation on Debian Linux (Jessie
    amd64). They should adaptable to *nix variants. Otherwise, a
    VirtualBox image that has the entire demo installed and ready to
    go can be found at:

    http://www.cs.umd.edu/~piotrm/virtualboxes/debian_x11_prob_repl.gz

    Everything has been already set up in this image. Login as
    *user*/*user* and look in the *~/prob-repl* folder for more
    information. The root password is *toor* if you need it.

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
        libsdl-image1.2-dev
        libsdl-ttf2.0-dev

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
  conf-sdl-image
  conf-sdl-ttf
  lambda-term
  mlgmp

  > opam install NAME

* Install ocamlsdl2:

### Contact
Piotr Mardziel: [piotrm@gmail.com](mailto:piotrm@gmail.com), [http://www.cs.umd.edu/~piotrm](http://www.cs.umd.edu/~piotrm)
