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
under my Arch GNU/Linux system with either no command-line parameters or
the minimum that I would generally use.

Once I have a good range of programs basically running I will come back
and implement more command line options and functionalities, as well as
making it all a bit more robust.

## 2. The Utils

Here is a list of the utils included in GNU coreutils, with ones that
have been touched upon already by this project in **bold**.

* [arch](https://www.gnu.org/software/coreutils/manual/html_node/arch-invocation.html)

* **[base64](https://www.gnu.org/software/coreutils/manual/html_node/base64-invocation.html)**

* **[basename](https://www.gnu.org/software/coreutils/manual/html_node/basename-invocation.html)**

* **[cat](https://www.gnu.org/software/coreutils/manual/html_node/cat-invocation.html)**

* [chcon](https://www.gnu.org/software/coreutils/manual/html_node/chcon-invocation.html)

* [chgrp](https://www.gnu.org/software/coreutils/manual/html_node/chgrp-invocation.html)

* [chmod](https://www.gnu.org/software/coreutils/manual/html_node/chmod-invocation.html)

* [chown](https://www.gnu.org/software/coreutils/manual/html_node/chown-invocation.html)

* [chroot](https://www.gnu.org/software/coreutils/manual/html_node/chroot-invocation.html)

* [cksum](https://www.gnu.org/software/coreutils/manual/html_node/cksum-invocation.html)

* [comm](https://www.gnu.org/software/coreutils/manual/html_node/comm-invocation.html)

* [cp](https://www.gnu.org/software/coreutils/manual/html_node/cp-invocation.html)

* [csplit](https://www.gnu.org/software/coreutils/manual/html_node/csplit-invocation.html)

* [cut](https://www.gnu.org/software/coreutils/manual/html_node/cut-invocation.html)

* [date](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)

* [dd](https://www.gnu.org/software/coreutils/manual/html_node/dd-invocation.html)

* **[df](https://www.gnu.org/software/coreutils/manual/html_node/df-invocation.html)**

* [dir](https://www.gnu.org/software/coreutils/manual/html_node/dir-invocation.html)

* [dircolors](https://www.gnu.org/software/coreutils/manual/html_node/dircolors-invocation.html)

* [dirname](https://www.gnu.org/software/coreutils/manual/html_node/dirname-invocation.html)

* [du](https://www.gnu.org/software/coreutils/manual/html_node/du-invocation.html)

* **[echo](https://www.gnu.org/software/coreutils/manual/html_node/echo-invocation.html)**

* [env](https://www.gnu.org/software/coreutils/manual/html_node/env-invocation.html)

* [expand](https://www.gnu.org/software/coreutils/manual/html_node/expand-invocation.html)

* [expr](https://www.gnu.org/software/coreutils/manual/html_node/expr-invocation.html)

* [factor](https://www.gnu.org/software/coreutils/manual/html_node/factor-invocation.html)

* **[false](https://www.gnu.org/software/coreutils/manual/html_node/false-invocation.html)**

* [fmt](https://www.gnu.org/software/coreutils/manual/html_node/fmt-invocation.html)

* [fold](https://www.gnu.org/software/coreutils/manual/html_node/fold-invocation.html)

* **[groups](https://www.gnu.org/software/coreutils/manual/html_node/groups-invocation.html)**

* **[head](https://www.gnu.org/software/coreutils/manual/html_node/head-invocation.html)**

* **[hostid](https://www.gnu.org/software/coreutils/manual/html_node/hostid-invocation.html)**

* **[hostname](https://www.gnu.org/software/coreutils/manual/html_node/hostname-invocation.html)**

* **[id](https://www.gnu.org/software/coreutils/manual/html_node/id-invocation.html)**

* [install](https://www.gnu.org/software/coreutils/manual/html_node/install-invocation.html)

* [join](https://www.gnu.org/software/coreutils/manual/html_node/join-invocation.html)

* [kill](https://www.gnu.org/software/coreutils/manual/html_node/kill-invocation.html)

* [link](https://www.gnu.org/software/coreutils/manual/html_node/link-invocation.html)

* [ln](https://www.gnu.org/software/coreutils/manual/html_node/ln-invocation.html)

* **[logname](https://www.gnu.org/software/coreutils/manual/html_node/logname-invocation.html)**

* **[ls](https://www.gnu.org/software/coreutils/manual/html_node/ls-invocation.html)**

* **[md5sum](https://www.gnu.org/software/coreutils/manual/html_node/md5sum-invocation.html)**

* [mkdir](https://www.gnu.org/software/coreutils/manual/html_node/mkdir-invocation.html)

* [mkfifo](https://www.gnu.org/software/coreutils/manual/html_node/mkfifo-invocation.html)

* [mknod](https://www.gnu.org/software/coreutils/manual/html_node/mknod-invocation.html)

* [mktemp](https://www.gnu.org/software/coreutils/manual/html_node/mktemp-invocation.html)

* [mv](https://www.gnu.org/software/coreutils/manual/html_node/mv-invocation.html)

* [nice](https://www.gnu.org/software/coreutils/manual/html_node/nice-invocation.html)

* **[nl](https://www.gnu.org/software/coreutils/manual/html_node/nl-invocation.html)**

* [nohup](https://www.gnu.org/software/coreutils/manual/html_node/nohup-invocation.html)

* **[nproc](https://www.gnu.org/software/coreutils/manual/html_node/nproc-invocation.html)**

* [numfmt](https://www.gnu.org/software/coreutils/manual/html_node/numfmt-invocation.html)

* [od](https://www.gnu.org/software/coreutils/manual/html_node/od-invocation.html)

* [paste](https://www.gnu.org/software/coreutils/manual/html_node/paste-invocation.html)

* [pathchk](https://www.gnu.org/software/coreutils/manual/html_node/pathchk-invocation.html)

* [pinky](https://www.gnu.org/software/coreutils/manual/html_node/pinky-invocation.html)

* [pr](https://www.gnu.org/software/coreutils/manual/html_node/pr-invocation.html)

* [printenv](https://www.gnu.org/software/coreutils/manual/html_node/printenv-invocation.html)

* [printf](https://www.gnu.org/software/coreutils/manual/html_node/printf-invocation.html)

* [ptx](https://www.gnu.org/software/coreutils/manual/html_node/ptx-invocation.html)

* [pwd](https://www.gnu.org/software/coreutils/manual/html_node/pwd-invocation.html)

* [readlink](https://www.gnu.org/software/coreutils/manual/html_node/readlink-invocation.html)

* **[realpath](https://www.gnu.org/software/coreutils/manual/html_node/realpath-invocation.html)**

* [rm](https://www.gnu.org/software/coreutils/manual/html_node/rm-invocation.html)

* [rmdir](https://www.gnu.org/software/coreutils/manual/html_node/rmdir-invocation.html)

* [runcon](https://www.gnu.org/software/coreutils/manual/html_node/runcon-invocation.html)

* **[seq](https://www.gnu.org/software/coreutils/manual/html_node/seq-invocation.html)**

* **[sha1sum](https://www.gnu.org/software/coreutils/manual/html_node/sha1sum-invocation.html)**

* **[sha224sum](https://www.gnu.org/software/coreutils/manual/html_node/sha224sum-invocation.html)**

* **[sha256sum](https://www.gnu.org/software/coreutils/manual/html_node/sha256sum-invocation.html)**

* **[sha384sum](https://www.gnu.org/software/coreutils/manual/html_node/sha384sum-invocation.html)**

* **[sha512sum](https://www.gnu.org/software/coreutils/manual/html_node/sha512sum-invocation.html)**

* [shred](https://www.gnu.org/software/coreutils/manual/html_node/shred-invocation.html)

* [shuf](https://www.gnu.org/software/coreutils/manual/html_node/shuf-invocation.html)

* [sleep](https://www.gnu.org/software/coreutils/manual/html_node/sleep-invocation.html)

* **[sort](https://www.gnu.org/software/coreutils/manual/html_node/sort-invocation.html)**

* [split](https://www.gnu.org/software/coreutils/manual/html_node/split-invocation.html)

* **[stat](https://www.gnu.org/software/coreutils/manual/html_node/stat-invocation.html)**

* [stdbuf](https://www.gnu.org/software/coreutils/manual/html_node/stdbuf-invocation.html)

* [stty](https://www.gnu.org/software/coreutils/manual/html_node/stty-invocation.html)

* **[sum](https://www.gnu.org/software/coreutils/manual/html_node/sum-invocation.html)**

* **[tac](https://www.gnu.org/software/coreutils/manual/html_node/tac-invocation.html)**

* **[tail](https://www.gnu.org/software/coreutils/manual/html_node/tail-invocation.html)**

* [tee](https://www.gnu.org/software/coreutils/manual/html_node/tee-invocation.html)

* [test](https://www.gnu.org/software/coreutils/manual/html_node/test-invocation.html)

* [timeout](https://www.gnu.org/software/coreutils/manual/html_node/timeout-invocation.html)

* [touch](https://www.gnu.org/software/coreutils/manual/html_node/touch-invocation.html)

* [tr](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)

* **[true](https://www.gnu.org/software/coreutils/manual/html_node/true-invocation.html)**

* [truncate](https://www.gnu.org/software/coreutils/manual/html_node/truncate-invocation.html)

* [tsort](https://www.gnu.org/software/coreutils/manual/html_node/tsort-invocation.html)

* **[tty](https://www.gnu.org/software/coreutils/manual/html_node/tty-invocation.html)**

* **[uname](https://www.gnu.org/software/coreutils/manual/html_node/uname-invocation.html)**

* [unexpand](https://www.gnu.org/software/coreutils/manual/html_node/unexpand-invocation.html)

* **[uniq](https://www.gnu.org/software/coreutils/manual/html_node/uniq-invocation.html)**

* [unlink](https://www.gnu.org/software/coreutils/manual/html_node/unlink-invocation.html)

* **[uptime](https://www.gnu.org/software/coreutils/manual/html_node/uptime-invocation.html)**

* **[users](https://www.gnu.org/software/coreutils/manual/html_node/users-invocation.html)**

* [vdir](https://www.gnu.org/software/coreutils/manual/html_node/vdir-invocation.html)

* [wc](https://www.gnu.org/software/coreutils/manual/html_node/wc-invocation.html)

* **[who](https://www.gnu.org/software/coreutils/manual/html_node/who-invocation.html)**

* **[whoami](https://www.gnu.org/software/coreutils/manual/html_node/whoami-invocation.html)**

* **[yes](https://www.gnu.org/software/coreutils/manual/html_node/yes-invocation.html)**

## 3. Project Structure

### 3.1. /docs

Generated documentation. The documentation is generated using Scribble,
Racket’s documentation language. The source for the documentation can be
found in /Scribblings. I generate documentation in html and markdown,
which can be found in their respective subfolders.

### 3.2. /scribblings

Documentation sourcecode, written in Scribble. As well as documentation,
this file and other markdown files in this repository are also generated
from Scribble.

### 3.3. /src

Project sourcecode. The racket files here form the programs of this
project that can be executed from the command line. It also contains the
following high-level subfolders:

#### 3.3.1. repl

This folder contains the files that do most of the "heavy lifting" of
program execution. Programs in the root directory instantiate programs
in this directory, set them up and execute them. This means that
programs in this directory can be run directly from Racket’s REPL.
