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

5. Install ppl with ocaml interface enabled:

    ```bash
    ./configure --enable-interfaces=ocaml --with-mlgmp=~/.opam/4.02/lib/gmp
    make
    sudo make install
    ```

6. Install latte:

  ```bash
  ./configure
  make
  ```

### Contact
Piotr Mardziel: [piotrm@gmail.com](mailto:piotrm@gmail.com),
[http://www.cs.umd.edu/~piotrm](http://www.cs.umd.edu/~piotrm)
