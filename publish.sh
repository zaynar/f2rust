#!/bin/bash

set -e

# Don't run this script directly, it may need some manual intervention

# Do run this from a clean checkout (delete the old rsspice directory)

# Remember to update version numbers etc

rm -rf tspice
if [ ! -f tspice.tar ]; then
    wget https://naif.jpl.nasa.gov/pub/naif/misc/tspice/N0067/PC_Linux_64bit/tspice.tar
fi
echo "5ee6d1c6435586023c02c08410bed833dbd78e58a88e76b5fb4a3bb91d85b306  tspice.tar" | sha256sum -c
tar xf tspice.tar
cd tspice
git init .
git add .
git commit -m "Import tspice.tar"
git am ../tspice-patches/*.patch
cd ..

cargo run --bin rsspice_build
cargo fmt -p rsspice

cd rsspice
sed -i 's~path = "../f2rust_std"~git = "https://github.com/zaynar/f2rust.git"~' Cargo.toml
git init
git remote add origin git@github.com:zaynar/rsspice.git
git fetch
git reset --soft origin/main
git branch -M main
git add .
# TODO: rm any no-longer-present files
git commit -m "Import generated code from https://github.com/zaynar/f2rust"
git push -u origin main

# Then use 'cargo publish' for:
#   f2rust_std
#   rsspice
