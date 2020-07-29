# Installation

Currently the only way to use this project is to run from the REPL or build an executable from source. 

## Requirements

- Common Lisp:
  - [SBCL](http://www.sbcl.org/getting.html)
  - [QuickLisp](https://www.quicklisp.org/beta/)
  - [Emacs](https://www.gnu.org/software/emacs/) + [SLIME](https://common-lisp.net/project/slime/) (recommended)
- [BearLibTerminal](http://foo.wyrd.name/en:bearlibterminal)

## Setup
Having installed the components described above, we can now setup this project to be run. To be picked up by QuickLisp, clone the repository into `local_projects`:

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

You can use your native window closing functionality to exit the game.

## Known-working versions
The above setup has been tested to work on:

- MacOS with SBCL 2.0.6, BearLibTerminal 0.15.7, CFFI 0.23.0

## Troubleshooting

Please post an [issue](./issues) if you run into problems with the above.
