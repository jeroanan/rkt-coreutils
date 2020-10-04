#lang scribble/base

@title{Rkt-Coreutils}

Implementing some/all of the functionality from some/all of the programs from GNU Coreutils in Racket

@section{Aims}
Mostly this project is for my coding pleasure and more in-depth learning about:

@itemlist[@item{Racket generally}
 @item{Scribl}
 @item{The ins-and outs of coreutils}
 @item{The workings of areas of the standard C library}]

Hopefully people will wander along and see the wonders of programming in Racket and maybe learn something/find bits of code that will help them out.

In terms of the programs in this project I am initially implementing a range of the programs that can be found in GNU Coreutils with the most basic functionality.
This means that in the vast majority of cases, as a first go at implementing a program, the aim is for it to run as it does under my Arch GNU/Linux system with either no
command-line parameters or the minimum that I would generally use.

Of late I have been including some code from the GNU project's gnulib. This means that there is now a fair bit if C code in this project. Over time I hope to replace some of this with Racket but for now they offer some portability and relief from dealing with some of the nitty-gritty of algorithms.

Currently the modules included from gnulib are:

@itemlist[@item{human}
  @item{nproc}
  @item{xstrtol}
  @item{fcntl}
  @item{error}
  @item{xbinary-io}
  @item{quotearg}
  @item{safe-read}]

Once I have a good range of programs basically running I will come back and implement more command line options and functionalities, as well as making it all a bit more robust.
           
@section{Project Structure}

@subsection{/docs}

Generated documentation. The documentation is generated using Scribble, Racket's documentation language. The source for the documentation can be found in /Scribblings.
I generate documentation in html and markdown, which can be found in their respective subfolders.

@subsection{/scribblings}

Documentation sourcecode, written in Scribble. As well as documentation, this file and other markdown files in this repository are also generated from Scribble.

@subsection{/src}
Project sourcecode. The racket files here form the programs of this project that can be executed from the command line. It also contains the following high-level subfolders:


@subsubsection{repl}
This folder contains the files that do most of the "heavy lifting" of program execution. Programs in the root directory instantiate programs in this directory,
set them up and execute them. This means that programs in this directory can be run directly from Racket's REPL.

