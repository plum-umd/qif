*** WARNING: this is outdated ***

  The sources of the below are included in the install directory.

  Replace /Users/piotrm/prob below with installation directory, or
  remove the prefixes and various include/lib/bin dirs mentioning the
  above if installing to the default locations.  If using default
  locations, replace "make install" or other high permission commands
  with "sudo make install", etc.

  Note, on mac we need to use at least gcc-4.2 and g++-4.2 . Some
  ./configure below thus have CC=gcc-4.2 and CXX=g++-4.2 but might not
  be necessary for other computers or might point to wrong
  things. Correct accordingly.

  Make sure the correct verstion of ocaml is used (>= 3.12) (the one
  provided in install is 3.12):

    export PATH=/Users/piotrm/prob/bin:$PATH
    export INCPATH=/Users/piotrm/prob/include:$INCPATH

  Sometimes the path must be provided in reverse order to make the
  installed version be found first:

    export PATH=$PATH:/Users/piotrm/prob/bin

  On some macs I find one has to use ABI=32 for the configurations to
  make sure things are made for the same "architecture" throughout the
  process. I'm not sure what exactly is going on really. For some
  steps I provide two different configuration commands below, only one
  is applicable in a situation:

  ocaml >= 3.12
    one of:
      ./configure -cc gcc-4.2 -prefix /Users/piotrm/prob
      ./configure
    make world.opt
    make install

  camlp5: http://pauillac.inria.fr/~ddr/camlp5/
    follow default installation procedure

  gmp 4.x (NOT 5.x): http://gmplib.org/
    one of:
      ABI=32 CC=gcc-4.2 CXX=g++-4.2 CPPFLAGS=-fexceptions ./configure --enable-cxx --prefix=/Users/piotrm/prob
      CC=gcc-4.2 CXX=g++-4.2 CPPFLAGS=-fexceptions ./configure --enable-cxx --prefix=/Users/piotrm/prob
      CPPFLAGS=-fexceptions ./configure --enable-cxx --prefix=/Users/piotrm/prob
    make
    make install

  mpfr: http://www.mpfr.org/mpfr-current/#download
    one of:
      ABI=32 CC=gcc-4.2 CXX=g++-4.2 ./configure --with-gmp=/Users/piotrm/prob --prefix=/Users/piotrm/prob
      CC=gcc-4.2 CXX=g++-4.2 ./configure --with-gmp=/Users/piotrm/prob --prefix=/Users/piotrm/prob
      ./configure
    make
    make install

  mlgmp: http://www-verimag.imag.fr/~monniaux/programmes.html.en
    Replace some lines in Makefile:
      #GMP_INCLUDES= -I/opt/gmp/include -I/users/absint2/local/include -I$(HOME)/packages/gmp/include
      GMP_INCLUDES= -I/Users/piotrm/prob/include

      #GMP_LIBDIR=/opt/gmp/lib
      GMP_LIBDIR=/Users/piotrm/prob/lib

      #CC= gcc
      CC = gcc-4.2
    Or if installed in default, this might work
      #GMP_INCLUDES= -I/opt/gmp/include -I/users/absint2/local/include -I$(HOME)/packages/gmp/include

      #GMP_LIBDIR=/opt/gmp/lib
      GMP_LIBDIR=.

    make install

  ppl >= 0.11.2: http://www.cs.unipr.it/ppl/
    one of
      ABI=32 CC=gcc-4.2 CXX=g++-4.2 ./configure --enable-interfaces="c cxx ocaml" --with-libgmp-prefix=/Users/piotrm/prob --prefix=/Users/piotrm/prob
      CC=gcc-4.2 CXX=g++-4.2 ./configure --enable-interfaces="c cxx ocaml" --with-libgmp-prefix=/Users/piotrm/prob --prefix=/Users/piotrm/prob
      ./configure --enable-interfaces="c cxx ocaml"
    make
    make install

  ppl 1.0:
   ./configure --enable-interfaces="c cxx ocaml" --prefix=/Users/piotrm/classes/secure-sharing/prob2 --with-gmp=/Users/piotrm/classes/secure-sharing/prob2
    


  latte: http://www.math.ucdavis.edu/~mkoeppe/latte/
  	 http://www.math.ucdavis.edu/~mkoeppe/latte/download/latte-for-tea-too-1.2-mk-0.9.3.tar.gz

    install in place so that install/latte-for-tea-too-1.2-mk-0.9.3/dest/bin/count is available

    ./configure --prefix=/Users/piotrm/prob
    make

make:

  make

run:

    ./prob something.prob
    ./prob --poly something.prob

  or

    cat something.prob | ./prob

STMT syntax:

  BINOP = + | - | * | / | == | < | > | <= | >= | or | and

  EXP =
    | n			(* where n is an integer *)
    | x      	   	(* where x is a string variable name *)
    | EXP BINOP EXP
    | ( EXP )		(* for parsing disambiguation *)

  STMT =
    | skip
    | if EXP then STMT endif
    | if EXP then STMT else STMT endif
    | pif n1 : n2 then STMT endpif		(* where n1, n2 are integers *)
    | pif n1 : n2 then STMT else STMT endpif	(* where n1, n2 are integers *)
    | x = EXP  	       	      	     	  	(* where x is a variable name *)
    | while EXP do STMT endwhile               	(* not evaluated right now *)
    | STMT ; STMT
    | { STMT }					(* for parsing disambiguation *)
    | STMT ;
    | uniform x n1 n2				(* where x is a var name, n1,n2 are integers)

input file syntax:

   secret: STMT
   belief: STMT
   program x y z ... -> u v w : STMT
   query: STMT
   query: STMT
   ...

   That is, the input file begins with "secret: " followed by a statement
   which when evaluated produces a state that is then used as the secret data.

   Follows is a statement that when evaluated probabilistically produces the
   initial belief.

   Follows is the program, which includes a list of input variables and output variables
   followed by the statement of the program.

   Finally there are a series of queries, which are statements that each produce
   states, which are used as inputs to the program.

   See examples/password.prob for an example.
