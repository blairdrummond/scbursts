#!/bin/bash

BUILDS=$HOME/builds
MAILING_LIST=$HOME/MAILING_LIST

newest=$(ls -Art $BUILDS | tail -n 1)
build_time=${newest}

echo "Mailing new build..."

echo "Latest UOttawaIonChannel build
build time: $build_time" | mutt -a $BUILDS/$newest/* -s "R Library build - $build_time" -- $(cat $MAILING_LIST | tr '\n' ' ')
