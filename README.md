# Cabasa

Cabasa is an application for the simulation of arbitrary 2D cellular automata (henceforth CA/CAs for clarity).
In contrast to other CA simulation applications which only allow a limited set of CAs to be simulated,
  Cabasa allows allows the simulation of any 2D CA on a rectangular grid.

![screenshot](screenshot.png)

## Features

- Allows the use of two flexible languages to specify CAs:
  the [ALPACA](https://github.com/catseye/ALPACA/blob/0b2d57b8739dc240969c62c8e1cd13c1863770e0) language (originally by @cpressey)
  as well as the Haskell programming language.
- An interactive user interface based on the widely-used [Golly](http://golly.sourceforge.net/) CA simulator
  allowing patterns to be drawn easily.
- Able to open and save patterns in the [MCell](http://psoup.math.wisc.edu/mcell/ca_files_formats.html#MCell) file format.

## Installation

As of this moment, there are no pre-build binaries to download; you will have to compile Cabasa from source.
For more details on how to do this, see [`BUILDING.md`](BUILDING.md).