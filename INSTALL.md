# Installation

Currently the only way to use this project is to run from the REPL or build an executable from source. 

## Requirements

- Common Lisp:
  - e.g. [SBCL](http://www.sbcl.org/getting.html), [ECL](https://common-lisp.net/project/ecl/), ...
  - [Quicklisp](https://www.quicklisp.org/beta/)
  - (recommended) [Emacs](https://www.gnu.org/software/emacs/) + [SLIME](https://common-lisp.net/project/slime/)/[SLY](https://github.com/joaotavora/sly)
- [BearLibTerminal](http://foo.wyrd.name/en:bearlibterminal)

## Setup
Having installed the components described above, we can now set everything up. Perhaps the easiest way for the project to be picked up by Quicklisp is to clone the repository into `local_projects`:

```sh
git clone https://github.com/lewis-weinberger/cprl.git ~/quicklisp/local_projects/cprl
```

Start your lisp REPL (from the shell or via SLIME/SLY) and load `cprl`:

```lisp
CL-USER> (ql:quickload :cprl)
```

This should bring in the dependencies ([CFFI](https://common-lisp.net/project/cffi/), [Deploy](https://github.com/Shinmera/deploy), [float-features](https://github.com/Shinmera/float-features)) if not already installed, and compile the source (to *fasl*).

## Running
The entry-point to a playable "game" is `main`:

```lisp
CL-USER> (cprl:main)
```

Note that you can usually feed the above two expressions to your lisp implementation at the command-line to launch the game directly, for example:

```sh
sbcl --eval "(ql:quickload :cprl)" --eval "(cprl:main)"
```

You can use your native window closing functionality to exit the game.

## Deploying

To compile an executable (with the necessary libraries packaged alongside it), use `asdf:make`:

```lisp
CL-USER> (asdf:make :cprl)
```

This takes advantage of the handy [Deploy](https://github.com/Shinmera/deploy) library. The above command should produce a new directory, `bin`, containing the executable and libraries (e.g. on Linux `cprl` and `libBearLibTerminal.so`).

## Troubleshooting

Please post an [issue](https://github.com/lewis-weinberger/cprl/issues) if you run into problems with the above.
