# Installation procedure for SMTCoq-API
First, you need to install SMTCoq for Coq-8.11, par following [these
instructions](https://github.com/smtcoq/smtcoq/blob/coq-8.11/INSTALL.md#installation-from-the-sources-using-opam-).
Take care of using [the branch coq-8.11 of SMTCoq](https://github.com/smtcoq/smtcoq/tree/coq-8.11).

Then, simply go to the `src` directory and run:
```
./configure.sh
make
```
