# cl-sdl3

`cl-sdl3` is a Common Lisp wrapper for the SDL 3.0 C Library.

It is licensed under the [MIT license](https://opensource.org/licenses/MIT).

# Installation

sdl3 is in Quicklisp, see below for instructions.

## SDL 3.0 C Library Install
See https://wiki.libsdl.org/SDL3/FrontPage

On Linux, you can probably find SDL3 in your distribution's package
set.  For other platforms, or for building manually, [download the
source](https://github.com/libsdl-org/SDL/releases/latest)

### Package
* Debian based: Ubuntu, Mint etc
```bash
sudo apt-get install libsdl3-dev
```
* Arch
```bash
sudo pacman -S sdl3
```

### Compilation

If you need to compile from source for your Linux platform:

1. Download [source code](https://github.com/libsdl-org/SDL)
2. Compile
3. Install

For example:
```bash
cd /tmp
wget https://github.com/libsdl-org/SDL/archive/refs/tags/release-3.2.24.tar.gz
tar -xzvf release-3.2.24.tar.gz
cd SDL-release-3.2.24
cmake -S . -B build
cmake --build build
sudo cmake --install build --prefix /usr/local
```

This will install the SDL-3.2.x C Library into your /usr/local location.

It's generally a good idea to install at a minimum the version of SDL3 that was
wrapped; however, sub revisions should not introduce binary incompatibility and
should be fine.  If you install a different version, certain features may not be
available or may not work correctly.

## Ocicl Install

If you don't have Ocicl, then follow [the
directions](https://github.com/ocicl/ocicl) to install it. 

```bash
cd cl-sdl3
```
Start your lisp and then:

```lisp
(asdf:load-system :sdl3)
```

## Quicklisp Install

If you don't have Quicklisp, then follow [the
directions](http://www.quicklisp.org/beta/) to install it. We assume
you placed the Quicklisp repository in the default place as indicated
by the directions and have added it to your lisp init file.

## Github install
```bash
cd $HOME/quicklisp-local-projects
git clone https://github.com/ellisvelo/cl-sdl3.git
```
Then, use quicklisp to install the libraries required by cl-sdl3:

Start your lisp. Then, just:

```lisp
(ql:quickload :sdl3)
```

## Swank/Slynk features

sdl3 enables certain restarts for friendly interaction with SLIME or
Sly if you have either properly installed.  "Proper installation" in
this case means `swank.asd` or `slynk.asd` is linked such that ASDF
can find and load it.

Note this is easily achieved even if you have installed them from
github or some other non-Quicklisp repository:

* Symlink the directory to `$HOME/quicklisp/local-projects/`
* Symlink the `.asd` to `$HOME/.local/common-lisp/sources/`

Similarly you could just clone into `~/quicklisp/local-projects` as
well; this should work on Windows as well.  There are numerous other
options for configuring and managing ASDs, as well.

# Running the sdl3 examples

Start your lisp:

```lisp
(asdf:load-system :sdl3-examples)
(sdl3-examples:basic-test)
```

This example will open a window with an opengl primitive in it. Any mouse
movements or keystrokes are recorded in the terminal (or emacs SLIME output
buffer ```*inferior-lisp*```). Hitting the ESCAPE key will terminate the
example.

## macOS

MacOS has difficulties as calls which require `nextEventMatchingMask` must be
called from the main thread of your program.

This is especially relevant to SBCL, although issues have also been noticed in
CCL.

Currently, initialisation must take place on the main thread:

```lisp
(asdf:load-system :sdl3-examples)

;; We should be able to run examples as normal on CCL
#-sbcl (sdl3-examples:basic-test)

;; SBCL requires that we initialise in the main thread
#+sbcl (sdl3:make-this-thread-main #'sdl3-examples:basic-test)
```

Thank you for using sdl3!
