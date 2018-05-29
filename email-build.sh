#!/bin/bash

BUILDS=$HOME/builds
MAILING_LIST=$HOME/MAILING_LIST

newest=$(ls -Art $BUILDS | tail -n 1)
build_time=${newest}

if [ ! -f $MAILING_LIST ]; then
    echo "No mailing list found."
    exit 0
fi

echo "MAILING_LIST:"
cat $MAILING_LIST
echo

while true; do

    read -p "Email new build to the Mailing List? [y/n] " yn
    case $yn in
	[Yy]* ) break ;;
	[Nn]* ) exit  ;;
	* )     echo "Please answer yes or no." ;;
    esac
done


echo
echo "Mailing new build..."

echo "Latest UOttawaIonChannel build
build time: $build_time" | mutt -a $BUILDS/$newest/* -s "R Library build - $build_time" -- $(cat $MAILING_LIST | tr '\n' ' ')
