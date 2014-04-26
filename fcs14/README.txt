Adversary Gain vs. Defender Loss in Quantified Information Flow
FCS 2014 Implementation and Experiments
Piotr Mardziel <piotrm@gmail.com>

REQUIREMENTS:
  gmake (if installing on FreeBSD)
  ocaml
  ocamlfind
  extlib
  gnuplot (optional)
  ps2pdf (optional)

INSTALLATION:
  The installation instructions provided here are specific to either
  Mac OSX, Debian Linux, or FreeBSD. Everything listed above should be
  installable on other unix-like OSs.

On Mac OSX:

  1. Install homebrew.
     > ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"

  2. Install ocaml using homebrew:
     > brew install ocaml

  3. Install opam using homebrew:
     > brew install opam
     > opam init

     Follow instructions to configure your environment.

  4. Install ocamlfind using opam:
     > opam install ocamlfind

  5. Install extlib using opam:
     > opam install extlib

  6. (optional) To make the figures used in the paper, you will also
       need gnuplot: 
     > brew install gnuplot

  7. (optional) To make .pdf figures from .eps figures you will need ps2pdf.

On Debian Linux (or linux with apt):

  1. Install wget
     > sudo apt-get install wget

  2. Install ocaml and camlp4-extras
     > sudo apt-get install ocaml-nox
     > sudo apt-get install ocamlp4
     > sudo apt-get install ocamlp4-extra

  3. Install opam. Not present in Weezy, so install from source:
     > wget --no-check-certificate https://github.com/ocaml/opam/releases/download/1.1.1/opam-full-1.1.1.tar.gz
     > tar -zxvf opam-full-1.1.1.tar.gz
     > cd opam-1.1.1
     > ./configure
     > make
     > sudo make install

     > opam init

     Follow instructions to configure your environment.

  4. Install ocamlfind using opam:
     > opam install ocamlfind

  5. Install extlib using opam:
     > opam install extlib

  6. (optional) To make the figures used in the paper, you will also
       need gnuplot: 
     > sudo apt-get install gnuplut

  7. (optional) To make .pdf figures from .eps figures you will need ps2pdf.

On FreeBSD using pkg:

  1. Install wget, gmake
     > sudo pkg install wget
     > sudo pkg install gmake

  2. Install ocaml
     > sudo pkg install ocaml-nox11

  3. Install opam from source, use gmake
     > wget --no-check-certificate http://github.com/ocaml/opam/releases/download/1.1.1/opam-full-1.1.1.tar.gz
     > tar -zxvf opam-full-1.1.1.tar.gz
     > cd opam-1.1.1
     > ./configure
     > gmake
     > sudo gmake install

  4. Install ocamlfind using opam:
     > opam install ocamlfind

  5. Install extlib using opam:
     > opam install extlib

  6. (optional) To make the figures used in the paper, you will also
       need gnuplot: 
     > sudo pkg install gnuplut

  7. (optional) To make .pdf figures from .eps figures you will need ps2pdf.

BUILDING:
  note: If using FreeBSD, replace "make" with "gmake" below.

  8. You should now be ready to build and run the experiments:
     cd experiments
     make all
     sh gen_all.sh

  9. (optional) You can also make the figures used in the paper:
     cd ../data
     make

CONTENTS:

  experiments/
    pmonad.ml pmap.ml pmap.mli util.ml - probabilistic monad
      implementation 
    common.ml scenario.ml - scenario optimizations and metric computations
    exp_XXX.ml - the various experiments
      these can be built individually using "make exp_XXX"
    gen_XXX.sh - scripts to generate the data for the experiments in
      the paper 
    gen_all.sh - script to run all of the previous

  data/
    data_XXX/ - outputs of experiments are put here
    plot_XXX.gnu - gnuplot files for generating figures
      these can be built using "gnuplot plot_XXX.gnu"
    fig_XXX.eps - generated figures using the above
    common.gnu - some common definitions for the figures
    makepdfs.sh - generates all figures, converts them to pdf and
      moves them to ../figures 

  figures/
    fig_XXX.pdf - figures from paper, created using makepdfs.sh above
