# Building/using Rkt-Coreutils

## 1. Pre-requisites

* GNU/Linux

* Racket

* racket, raco and scribble executables must be in the PATH

## 2. Running \(from the shell\)

The programs in the src/ directory can be run directly using racket. To
get better performance out of the programs they can be compiled to
bytecode first \(see below Makefile targets\).

## 3. Makefile Targets

**all**: The default target. Compiles programs to bytecode and builds
html and markdown documentation.

**make-all**: Compiles programs to bytecode.

**docs**: Builds html and markdown documentation.

**launchers**: Builds launcher scripts

**clean**: Removes the docs/ directory, ready for full rebuild.

**deploy**: Copies the launcher scripts to ~/bin. This is a folder that
I have in my path. The DEPDIR variable in the makefile can be altered to
change where the launcher scripts are copied to.

## 4. Launcher Scripts

The launcher scripts, generated with the "launchers" Make target, are
shell scripts that reference the relevant racket file. This reference
takes the form of an absolute path so they can be copied elsewhere in
the filesystem and still be successfully executed.

Out of the box this project provides no tools for building executables
of the programs, although raco can be used for this. When experimenting
with executables made from this project’s source code they were weighing
in at > 15MB each. So instead I am using the launcher scripts. When
coupled with bytecode compilation of the scripts the launcher script
approach incurs no performance penalty compared with executables.

## 5. On Operating System

The pre-requisites above mention GNU/Linux as a pre-requisite. In fact,
most of the programs should run under any operating system that Racket
does, with the exception of those that reference files in the src/libc
directory. Libc is only commonly found on Unix-like systems.
