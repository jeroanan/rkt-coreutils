# Rkt-Coreutils

Implementing some/all of the functionality from some/all of the programs
from GNU Coreutils in Racket

## 1. Aims

Mostly this project is for my coding pleasure and more in-depth learning
about:

* Racket generally

* Scribl

* The ins-and outs of coreutils

* The workings of areas of the standard C library

Hopefully people will wander along and see the wonders of programming in
Racket and maybe learn something/find bits of code that will help them
out.

In terms of the programs in this project I am initially implementing a
range of the programs that can be found in GNU Coreutils with the most
basic functionality. This means that in the vast majority of cases, as a
first go at implementing a program, the aim is for it to run as it does
under my Ubuntu GNU/Linux system with either no command-line parameters
or the minimum that I would generally use.

Once I have a good range of programs basically running I will come back
and implement more command line options and functionalities, as well as
making it all a bit more robust.

## 2. Project Structure

### 2.1. /docs

Generated documentation. The documentation is generated using Scribble,
Racket’s documentation language. The source for the documentation can be
found in /Scribblings. I generate documentation in html and markdown,
which can be found in their respective subfolders.

### 2.2. /scribbings

Documentation sourcecode, written in Scribble. As well as documentation,
this file and other markdown files in this repository are also generated
from Scribble.

### 2.3. /src

Project sourcecode. The racket files here form the programs of this
project that can be executed from the command line. It also contains the
following high-level subfolders:

#### 2.3.1. libc

I am trying to stick to Racket where possible but there are some things
for which support just doesn’t exist in Racket. For these situations I
am using the files in this directory to call into the standard C
library. Mostly this uses Racket’s FFI functionality so at first glance
it appears to be just more racket but occasionally I am resorting to the
dynamic FFI library to embed C programs.

#### 2.3.2. repl

This folder contains the files that do most of the "heavy lifting" of
program execution. Programs in the root directory instantiate programs
in this directory, set them up and execute them. This means that
programs in this directory can be run directly from Racket’s REPL.

#### 2.3.3. typedef

Type definitions for interaction between typed and untyped code.

#### 2.3.4. util

various utility files and macros.
