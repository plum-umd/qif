## Dynamic Enforcement of Knowledge-based Security Policies using Probabilistic Abstract Interpretation [pdf](http://www.cs.umd.edu/~mwh/papers/beliefpol-extended.pdf)

Piotr Mardziel, Stephen Magill, Michael Hicks, Mudhakar Srivatsa

**CSF 2011, JCS 2013 Implementation and experiments**

## Knowledge-Oriented Secure Multiparty Computation [pdf](http://www.cs.umd.edu/~mwh/papers/belief-smc.pdf)

Piotr Mardziel, Michael Hicks, Jonathan Katz, Mudhakar Srivatsa

**PLAS 2012 Implementation and experiments**

### Requirements
  * opam      (available with homebrew)

  * ocaml >= 3.12 (available with opam)

  * ocamlfind (available with opam)

  * ppl [http://bugseng.com/products/ppl/download]
    - gmp   (available with homebrew)
    - mpfr  (available with homebrew)
    - mlgmp (available with opam)

  * latte [https://www.math.ucdavis.edu/~latte/software.php]

### Installation

Given instructions are for OSX using homebrew but should be adaptable
to other package managers or source installations.

1. Install homebrew

2. Install opam

   ```bash
   brew install opam
   ```

3. Install ocaml and ocamlfind:

   ```bash
   opam install ocaml
   opam install ocamlfind
   ```

4. Install gmp, mpfr, and mlgmp:

   ```bash
   brew install gmp
   brew install mpfr
   opam install mlgmp
   ```

5. Install ppl with ocaml interface enabled [ftp://ftp.cs.unipr.it/pub/ppl/releases/1.1/ppl-1.1.tar.gz]:

    ```bash
    ./configure --enable-interfaces=ocaml --with-mlgmp=~/.opam/4.02/lib/gmp
    make
    sudo make install
    ```

    ** Warning: ppl's configuration may silently fail when it cannot find
       something it needs (like mlgmp). In those cases it might install
       the main library and even the ocaml interface documentation without
       actually installing the interface itself.**

6. Install latte [https://www.math.ucdavis.edu/~latte/software/packages/latte_current/latte-integrale-1.7.2.tar.gz]:

   ```bash
   ./configure
   make
   ```

   Make latte's `count` and `latte-maximize` available on your path. You
   can do this by making links to `dest/bin/count` and `dest/bin/latte-maximize` like so:

   ```bash
   ln -s /Users/piotrm/Downloads/install/latte-integrale-1.7.2/dest/bin/count /usr/local/bin/count
   ln -s /Users/piotrm/Downloads/install/latte-integrale-1.7.2/dest/bin/latte-maximize /usr/local/bin/latte-maximize
   ```

### Contact
Piotr Mardziel: [piotrm@gmail.com](mailto:piotrm@gmail.com),
[http://www.cs.umd.edu/~piotrm](http://www.cs.umd.edu/~piotrm)
