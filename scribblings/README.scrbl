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

Once I have a good range of programs basically running I will come back and implement more command line options and functionalities, as well as making it all a bit more robust.
           
@section{The Utils}

Here is a list of the utils included in GNU coreutils, with ones that have been touched upon already by this project in @bold{bold}.

@itemlist[@item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/arch-invocation.html"]{arch}}
  @item{@bold{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/base64-invocation.html"]{base64}}}
  @item{@bold{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/basename-invocation.html"]{basename}}}
  @item{@bold{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/cat-invocation.html"]{cat}}}
  @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/chcon-invocation.html"]{chcon}}
  @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/chgrp-invocation.html"]{chgrp}}
  @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/chmod-invocation.html"]{chmod}}
  @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/chown-invocation.html"]{chown}}
  @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/chroot-invocation.html"]{chroot}}
  @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/cksum-invocation.html"]{cksum}}
  @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/comm-invocation.html"]{comm}}
  @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/cp-invocation.html"]{cp}}
  @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/csplit-invocation.html"]{csplit}}
  @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/cut-invocation.html"]{cut}}
  @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html"]{date}}
  @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/dd-invocation.html"]{dd}}
  @item{@bold{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/df-invocation.html"]{df}}}
  @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/dir-invocation.html"]{dir}}
  @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/dircolors-invocation.html"]{dircolors}}
  @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/dirname-invocation.html"]{dirname}}
  @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/du-invocation.html"]{du}}
  @item{@bold{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/echo-invocation.html"]{echo}}}
  @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/env-invocation.html"]{env}}
  @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/expand-invocation.html"]{expand}}
  @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/expr-invocation.html"]{expr}}
  @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/factor-invocation.html"]{factor}}
  @item{@bold{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/false-invocation.html"]{false}}}
  @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/fmt-invocation.html"]{fmt}}
  @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/fold-invocation.html"]{fold}}
  @item{@bold{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/groups-invocation.html"]{groups}}}
  @item{@bold{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/head-invocation.html"]{head}}}
  @item{@bold{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/hostid-invocation.html"]{hostid}}}
  @item{@bold{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/hostname-invocation.html"]{hostname}}}
  @item{@bold{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/id-invocation.html"]{id}}}
  @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/install-invocation.html"]{install}}
  @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/join-invocation.html"]{join}}
  @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/kill-invocation.html"]{kill}}
  @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/link-invocation.html"]{link}}
  @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/ln-invocation.html"]{ln}}
  @item{@bold{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/logname-invocation.html"]{logname}}}
  @item{@bold{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/ls-invocation.html"]{ls}}}
  @item{@bold{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/md5sum-invocation.html"]{md5sum}}}
  @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/mkdir-invocation.html"]{mkdir}}
  @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/mkfifo-invocation.html"]{mkfifo}}
  @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/mknod-invocation.html"]{mknod}}
  @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/mktemp-invocation.html"]{mktemp}}
  @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/mv-invocation.html"]{mv}}
  @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/nice-invocation.html"]{nice}}
  @item{@bold{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/nl-invocation.html"]{nl}}}
  @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/nohup-invocation.html"]{nohup}}
  @item{@bold{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/nproc-invocation.html"]{nproc}}}
  @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/numfmt-invocation.html"]{numfmt}}
  @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/od-invocation.html"]{od}}
  @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/paste-invocation.html"]{paste}}
 @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/pathchk-invocation.html"]{pathchk}} 
 @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/pinky-invocation.html"]{pinky}}
 @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/pr-invocation.html"]{pr}}
 @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/printenv-invocation.html"]{printenv}}
 @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/printf-invocation.html"]{printf}}
 @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/ptx-invocation.html"]{ptx}}
 @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/pwd-invocation.html"]{pwd}}
 @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/readlink-invocation.html"]{readlink}}
 @item{@bold{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/realpath-invocation.html"]{realpath}}}
 @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/rm-invocation.html"]{rm}}
 @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/rmdir-invocation.html"]{rmdir}}
 @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/runcon-invocation.html"]{runcon}}
 @item{@bold{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/seq-invocation.html"]{seq}}}
 @item{@bold{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/sha1sum-invocation.html"]{sha1sum}}}
 @item{@bold{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/sha224sum-invocation.html"]{sha224sum}}}
 @item{@bold{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/sha256sum-invocation.html"]{sha256sum}}}
 @item{@bold{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/sha384sum-invocation.html"]{sha384sum}}}
 @item{@bold{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/sha512sum-invocation.html"]{sha512sum}}}
 @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/shred-invocation.html"]{shred}}
 @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/shuf-invocation.html"]{shuf}}
 @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/sleep-invocation.html"]{sleep}}
 @item{@bold{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/sort-invocation.html"]{sort}}}
 @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/split-invocation.html"]{split}}
 @item{@bold{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/stat-invocation.html"]{stat}}}
 @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/stdbuf-invocation.html"]{stdbuf}}
 @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/stty-invocation.html"]{stty}}
 @item{@bold{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/sum-invocation.html"]{sum}}}
 @item{@bold{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/tac-invocation.html"]{tac}}}
 @item{@bold{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/tail-invocation.html"]{tail}}}
 @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/tee-invocation.html"]{tee}}
 @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/test-invocation.html"]{test}}
 @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/timeout-invocation.html"]{timeout}}
 @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/touch-invocation.html"]{touch}}
 @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html"]{tr}}
 @item{@bold{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/true-invocation.html"]{true}}}
 @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/truncate-invocation.html"]{truncate}}
 @item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/tsort-invocation.html"]{tsort}}
 @item{@bold{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/tty-invocation.html"]{tty}}}
 @item{@bold{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/uname-invocation.html"]{uname}}}
@item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/unexpand-invocation.html"]{unexpand}} 
@item{@bold{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/uniq-invocation.html"]{uniq}}}
@item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/unlink-invocation.html"]{unlink}}
@item{@bold{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/uptime-invocation.html"]{uptime}}}
@item{@bold{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/users-invocation.html"]{users}}}
@item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/vdir-invocation.html"]{vdir}}
@item{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/wc-invocation.html"]{wc}}
@item{@bold{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/who-invocation.html"]{who}}}
@item{@bold{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/whoami-invocation.html"]{whoami}}}
@item{@bold{@hyperlink["https://www.gnu.org/software/coreutils/manual/html_node/yes-invocation.html"]{yes}}}]

  

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

