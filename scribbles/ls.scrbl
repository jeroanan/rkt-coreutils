#lang scribble/base

@title{ls}

@section{Name}

ls - list directory contents

@section{Synopsis}

@bold{ls} [Option]... [DIRECTORY]

@section{Description}

List information about the DIRECTORY (the current directory by default).

@section{Parameters}
@bold{-a, --all} -- do not ignore entries starting with .
@linebreak{}
@bold{-A, --almost-all} -- do not list implied . and ..
@linebreak{}
@bold{-i, --inode} -- print the index number of each file
@linebreak{}
@bold{-l} -- use a long listing format
@linebreak{}
@bold{-v} -- display version information and exit

@include-section["author.scrbl"]
@include-section["copyright.scrbl"]
