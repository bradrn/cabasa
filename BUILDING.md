# Building Cabasa from source

For all OSs, the first step is cloning the repo:

```
$ git clone https://github.com/bradrn/cabasa
```

[`stack`](https://docs.haskellstack.org/en/stable/README/) is recommended,
  especially on Windows, since it includes a copy of MinGW.
The rest of this guide assumes the use of Stack.

### User Manual

Before building, the [user manual](data/doc/UserManual.tex) must also be compiled using LaTeX.
LaTeX may be installed using either [MiKTeX](https://miktex.org/) or [TeX Live](https://tug.org/texlive/);
  I personally use MiKTeX.
Instructions are given for each OS separately.

## Windows

### Installing GTK

Cabasa relies on the GTK GUI library and its GObject Introspection bindings.
To install, run the following command:

```
> stack exec -- pacman -S mingw-w64-x86_64-gtk3 mingw-w64-x86_64-glade mingw64/mingw-w64-x86_64-pkg-config mingw64/mingw-w64-x86_64-gobject-introspection
```

### Building the User Manual

[Python](https://www.python.org/) will need to be installed before building,
  since the user manual requires `pygmentize`, a Python application, for syntax highlighting.
Commands:

```
> pip install pygments
> cd data\doc
> pdflatex -shell-escape UserManual.tex
> cd ..\..
```

> **NOTE:** You may need to run the `pip install` command in administrator mode.

### Building Cabasa

After this, Cabasa may be build with the following command:

```
> stack exec -- bash -c 'XDG_DATA_DIRS=/mingw64/share stack build'
```

You may then run Cabasa with `stack exec cabasa`.

## Linux

### Ubuntu

#### Building the User Manual

```
$ sudo apt-get install python-pygments
$ cd data/doc
$ cd ../..
```

> **NOTE:** If you get an error about `\inputminted was probably given a file that does not exist`,
> try changing the line `\usepackage{minted}` to `\usepackage[cache=false]{minted}` in [`data/doc/UserManual.tex`](data/doc/UserManual.tex).

#### Building Cabasa

**WARNING: This section is OUTDATED. It will be updated when I have some time to figure out how to build the current version of Cabasa on Ubuntu.**

Run the following commands:

```
$ sudo apt-get install libcairo2-dev libpango1.0-dev gtk+-3.0
$ stack build
```

After this, Cabasa may be run with `stack exec cabasa`.

## Other OSs

So far, I have not tried building Cabasa on any other OSs.
If you have successfully managed it, feel free to send me a PR with your method!
