#!/bin/bash

mkdir -p compiled

cd src

for f in *.rkt; do
 raco exe --exf -U -lo "../compiled/rkt-${f%.rkt}" "$f"
done

