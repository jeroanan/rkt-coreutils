#!/bin/bash

mkdir -p compiled

cd src

for f in *.rkt; do
  echo $f
 raco exe -lo "../compiled/${f%.rkt}" "$f"
done

