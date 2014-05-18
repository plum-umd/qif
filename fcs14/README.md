## Adversary Gain vs. Defender Loss in Quantified Information Flow
Piotr Mardziel, Mario S. Alvim, Michael Hicks

**FCS 2014 Implementation and Experiments**

### Papers

1. [Quantifying Information Flow for Dynamic
   Secrets](http://www.cs.umd.edu/~mwh/papers/qif-dynamic-secrets.pdf)

   Initial paper. [Implementation and
   experiments](https://github.com/plum-umd/qif/tree/master/oakland14).

2. Tech Report: Quantifying Information Flow for Dynamic Secrets

   Includes proofs and analysis of a memory-limited adversary.

3. [Adversary Gain vs. Defender Loss in Quantified Information
   Flow](http://www.cs.umd.edu/~mwh/papers/qifgl.pdf)

   Introduces a distinction between defender's loss and the
   adversary's gain. [Implementation and
   experiments](https://github.com/plum-umd/qif/tree/master/fcs14).

### Installation

There are two options for running the experiments.

 1. On your own computer. This requires to install various
    prerequisites and has only been tested on Debian Linux, FreeBSD,
    and Mac OSX. See README.txt inside
    [experiments.tar.gz](experiments.tar.gz) for
    further instruction.

 2. Inside a VirtualBox image, located at:

    http://www.cs.umd.edu/~piotrm/virtualboxes/debian_oakland14_fcs14.tar.gz

    Everything has been already set up in this image. Login as
    *user*/*user* and look in the *~/fcs14* folder for more
    information. The root password is *toor* if you need it. The
    optional gnuplot package for generating the graphs in the paper is
    not installed to keep the image smaller. You can easily install
    it:

    > sudo apt-get install gnuplot

### Contact
Piotr Mardziel: [piotrm@gmail.com](mailto:piotrm@gmail.com), [http://www.cs.umd.edu/~piotrm](http://www.cs.umd.edu/~piotrm)
