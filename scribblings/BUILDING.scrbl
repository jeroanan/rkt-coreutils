#lang scribble/base

@title{Building/using Rkt-Coreutils}

@section{Pre-requisites}
@itemlist[
 @item{GNU/Linux}
 @item{Racket}
 @item{racket, raco and scribble executables must be in the PATH}]

In addition, the following Rakcet packages must be present:
@itemlist[
 @item{dynamic-ffi}
 @item{sha}]

@section{Running (from the shell)}

The programs in the src/ directory can be run directly using racket. To get better performance out of
the programs they can be compiled to bytecode first (see below Makefile targets).

@section{Makefile Targets}
@bold{all}: The default target. Compiles programs to bytecode and builds html and markdown
documentation.

@bold{make-all}: Compiles programs to bytecode.

@bold{docs}: Builds html and markdown documentation.

@bold{launchers}: Builds launcher scripts

@bold{clean}: Removes the docs/ directory, ready for full rebuild.

@bold{deploy}: Copies the launcher scripts to ~/bin. This is a folder that I have in my path. The
DEPDIR variable in the makefile can be altered to change where the launcher scripts are copied to.

@section{Launcher Scripts}
The launcher scripts, generated with the "launchers" Make target, are shell scripts that reference the
relevant racket file. This reference takes the form of an absolute path so they can be copied
elsewhere in the filesystem and still be successfully executed.

Out of the box this project provides no tools for building executables of the programs, although raco
can be used for this. When experimenting with executables made from this project's source code they
were weighing in at > 15MB each. So instead I am using the launcher scripts. When coupled with
bytecode compilation of the scripts the launcher script approach incurs no performance penalty
compared with executables.

@section{On Operating System}
The pre-requisites above mention GNU/Linux as a pre-requisite. In fact, most of the programs should
run under any operating system that Racket does, with the exception of those that reference files in
the src/libc directory. Libc is only commonly found on Unix-like systems.
