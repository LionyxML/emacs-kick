#!/bin/sh

clear
echo ">>> (re)Installing Emacs Kick ..."
sleep 2

echo ">>> Deleting packages, grammars and native compilation cache ..."
rm -rf eln-cache/ elpa/ tree-sitter/
sleep 2

echo ">>> Starting Emacs and auto-package fetching/installing ..."
sleep 2
emacs --init-dir="./" -nw --eval="(ek/first-install)"
