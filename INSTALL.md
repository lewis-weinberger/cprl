# Installation

Currently the only way to use this project is to run from the REPL or build an executable from source. 

## Requirements

- Common Lisp:
  - [SBCL](http://www.sbcl.org/getting.html)
  - [Quicklisp](https://www.quicklisp.org/beta/)
  - [Emacs](https://www.gnu.org/software/emacs/) + [SLIME](https://common-lisp.net/project/slime/) (recommended)
- [BearLibTerminal](http://foo.wyrd.name/en:bearlibterminal)

## Setup
Having installed the components described above, we can now set everything up. Perhaps the easiest way for the project to be picked up by Quicklisp is to clone the repository into `local_projects`:

```sh
git clone https://github.com/lewis-weinberger/cprl.git ~/quicklisp/local_projects/cprl
```

Start the `sbcl` REPL (from the shell or via SLIME) and load `cprl`:

```lisp
CL-USER> (ql:quickload :cprl)
```

This should bring in the dependencies ([CFFI](https://common-lisp.net/project/cffi/)) if not already installed, and compile the source (to *fasl*).

## Running
The entry-point to a playable "game" is `main`:

```lisp
CL-USER> (cprl:main)
```

Note that you can feed `sbcl` the above two expressions at the command-line to launch the game directly:

```sh
sbcl --eval "(ql:quickload :cprl)" --eval "(cprl:main)"
```

You can use your native window closing functionality to exit the game.

## Known-working versions
The above setup has been tested to work on:

- MacOS with SBCL 2.0.6, BearLibTerminal 0.15.7, CFFI 0.23.0
- (Gentoo) Linux with SBCL 2.0.5, BearLibTerminal 0.15.7, CFFI 0.23.0

## Troubleshooting

Please post an [issue](https://github.com/lewis-weinberger/cprl/issues) if you run into problems with the above.
