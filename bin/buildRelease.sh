#!/bin/bash

version=$(sbcl --noinform --disable-ldb --lose-on-corruption --end-runtime-options --eval '(format t "~A" (asdf:component-version (asdf:find-system :clnl-gltk)))' --eval "(quit)")

echo -n "Building version $version, hit enter to continue"
read

mkdir clnl-gltk_$version
cp -ap src/main/* clnl-gltk_$version/
tar zcf clnl-gltk_${version}.tar.gz clnl-gltk_$version/
rm -rf clnl-gltk_$version

echo "All done, it's in clnl-gltk_${version}.tar.gz, you should tag it and push it up to github"
