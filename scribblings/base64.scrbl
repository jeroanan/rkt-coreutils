#lang scribble/base

@title{base64}

@section{Name}

base64 - Base64-encode files and print on standard output.

@section{Synopsis}
@bold{base64} [FILE]...

@section{Description}

Base64-encode FILEs and print on stadnard output. If no FILEs are provided then read stdin. If
multiple files are provided then concatenate their contents and base-64 encode the result.

@include-section["author.scrbl"]
@include-section["copyright.scrbl"]
