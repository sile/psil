#! /bin/bash

for src in $@
do
    target=${src/'.cc'/'.o'}
    g++ -MT $target -MM -MG $src | sed -e 's_ psil/_ include/psil/_g'
done
