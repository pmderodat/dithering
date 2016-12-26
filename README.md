Black and white dithering algorithm
===================================

This is a toy project to implement the Floyd-Steinberg dithering algorithm.
The project contains a dummy BMP file reader/writer to easily test the
algorithm.

It uses the
[Ada_Drivers_Library](https://github.com/AdaCore/Ada_Drivers_Library/) in order
to provide a compatible interface.

In order to build it, first make sure you download the git submodules:

```shell
$ git submodules init
$ git submodules update
```

Then, with a GNAT compiler and [GPRbuild](https://github.com/AdaCore/gprbuild),
run:

```shell
$ gprbuild -Pdithering
```

You can then run the program to dither a BMP image:

```shell
$ obj/dithering example.bmp
$ $VIEWER example.bmp.new
```
